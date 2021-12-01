library(data.table)
library(glmnet)
library(ggplot2)

library(tidyverse)
library(caret)
library(datarium)


  #data.table::fread("C:\\Users\\sh2742\\Documents\\CS-599\\GRA\\merged.data.csv")

#r_add.R



merged.data$Duration.of.Event <- period_to_seconds(merged.data$Duration.of.Event)

#merged.data$Duration.of.Event <- seconds_to_period(merged.data$Duration.of.Event)
input.data <-  merged.data 


head(input.data)
print(names(input.data))
#total rows = 25547
# training - 20439
# testing - 5108

colnames(input.data)[7] <- "ETLAverage"
input.data <- input.data[, c(3,4,5,6, 7)]

fold.vec <- sample(rep(1:10,l=nrow(input.data)))
fold.vec <- sample(rep(1:10,l=nrow(input.data)))
folds.input.data <- data.table::data.table(input.data,fold=factor(fold.vec))

folds.input.data$OrdinalETL <- ""
for(i in 1:nrow(folds.input.data)) {
  row <- folds.input.data[i,]
  etlScore <- row$ETLAverage
  # print(etlScore)
  if (etlScore <= 1) {
    folds.input.data[i,]$OrdinalETL <- "Very negative"
    # print("Very negative")
  } else if (etlScore > 1 && etlScore <= 2) {
    folds.input.data[i,]$OrdinalETL <- "Negative"
    # print("Negative")
  } else if ( etlScore > 2 && etlScore <= 3) {
    folds.input.data[i,]$OrdinalETL <- "Neutral"
    # print("Neutral")
  } else if(etlScore > 3 && etlScore <= 4){
    folds.input.data[i,]$OrdinalETL <- "Very positive"
    # print("Very positive")
  }else{
    folds.input.data[i,]$OrdinalETL <- "Positive"
    # print("Positive")
  }
  
}
folds.input.data.updatedTypes<- data.frame(Date.of.Event.Visit.Month = as.numeric(folds.input.data$`Date.of.Event.Visit.Month`),
                                           Duration.of.Event = as.integer(folds.input.data$`Duration.of.Event`),
                                           frequency.month = as.numeric(folds.input.data$`frequency.month`),
                                           gap.month = as.numeric(folds.input.data$`gap.month`),
                                           
                                           ETLAverage = folds.input.data$ETLAverage,
                                           fold= as.numeric(folds.input.data$fold),
                                           stringsAsFactors = FALSE)

input.cols <- c("Date.of.Event.Visit.Month", "Duration.of.Event", "frequency.month", "gap.month")

#ordinal importance plot


weight.dt.list <- list()
for(i in 1:10){
  set.seed(i)
  print(i)
  fold.i<- folds.input.data.updatedTypes[folds.input.data.updatedTypes$fold == i,]
  trainind <- sort(sample(1:nrow(fold.i), size=floor(nrow(fold.i)*(3/4))))
  testind <- setdiff(1:nrow(fold.i), trainind)
  datatrain <- fold.i[trainind,]
  datatest <- fold.i[testind,]
  y_obs =  datatest[,5] 
  x = as.matrix(datatrain[,input.cols])
  y = datatrain[,5]
  set.seed(1011)

  cvob1 = cv.glmnet(x, y)
  
  weight.dt.list[[i]] <- data.table(
    i,
    row.name=rownames(coef(cvob1))[-1],
    weight=coef(cvob1)[-1])
  print("------")
  
  
}
weight.dt <- do.call(rbind, weight.dt.list)

ggplot()+
  geom_point(aes(
    weight, row.name),
    data=weight.dt)

weight.dt[, n.nonzero := sum(weight != 0), by=row.name]


ggplot()+
  facet_grid(n.nonzero ~ ., scales="free", space="free")+
  geom_point(aes(
    weight, row.name),
    data=weight.dt)
#===================================== Ordinal net

ord.dt.list <- list()
for(i in 1:10){
  set.seed(i)
  print(i)
  fold.i<- folds.input.data.updatedTypes[folds.input.data.updatedTypes$fold == i,]
  trainind <- sort(sample(1:nrow(fold.i), size=floor(nrow(fold.i)*(3/4))))
  testind <- setdiff(1:nrow(fold.i), trainind)
  datatrain <- fold.i[trainind,]
  datatest <- fold.i[testind,]
  y_obs =  datatest[,5] 
  x = as.matrix(datatrain[,-5])
  y = datatrain[,5]
  y <- as.factor(y)
  set.seed(1011)
  
  ord <- ordinalNetTune(x, y)
  
  best.lambda <- which.max(ord$loglik)
  
  ord.dt.list[[i]] <- data.table(
    i,
    row.name=colnames(ord$fit$coefs),
    weight= as.vector(ord$loglik[,best.lambda]))
  print("------")
  
  ggplot()+
    facet_grid(i ~ ., scales="free", space="free")+
    geom_point(aes(
      weight, row.name),
      data=ord.dt)
  
}
ord.dt <- do.call(rbind, ord.dt.list)


ggplot()+
  geom_point(aes(
    weight, row.name),
    data=ord.dt)

weight.dt[, n.nonzero := sum(weight != 0), by=row.name]


ggplot()+
  facet_grid(n.nonzero ~ ., scales="free", space="free")+
  geom_point(aes(
    weight, row.name),
    data=ord.dt)

fit1 <- ordinalNet(x, y, family="cumulative", link="logit",
                   parallelTerms=TRUE, nonparallelTerms=FALSE)

ord.dt <- data.table(
  row.name=rownames(coef(fit1))[-1],
  weight=coef(fit1)[-1])

#=====================================================================

final.accuracy.list <- list()
accuracy.dt <- list()
predictions.list <- list()
split = 10
for(i in 1:split){
  set.seed(i)
  # print(i)
  # test.fold = 1#i
  # datatrain <-folds.input.data[fold != test.fold] #69637 obs. of  129 variables:
  # datatest <- folds.input.data[fold == test.fold]
  #ord_y_train <- datatrain$OrdinalETL
  
  fold.i<- folds.input.data.updatedTypes[folds.input.data.updatedTypes$fold == i,]
  trainind <- sort(sample(1:nrow(fold.i), size=floor(nrow(fold.i)*(3/4))))
  testind <- setdiff(1:nrow(fold.i), trainind)
  datatrain <- fold.i[trainind,]
  datatest <- fold.i[testind,]
  
  y_train <- as.matrix(datatrain[,5])
  
  ord_y_test <- datatest[,5]
  y_test <- datatest[,5]
  
  train <- as.matrix(datatrain[,-5])
  test <-  as.matrix(datatest[,-5])
  
  ordfor_train <- datatrain[,-5]
  ordfor_test <- datatest[,-5]
  

  ord_y_train = datatrain[,5]
  ord_y_train <- as.factor(ord_y_train)
  
  #ordnet <- ordinalNetTune(train,ord_y_train)
  #bestLambdaIndex <- which.max(rowMeans(ordnet$loglik))
  cv.fit.gaussian <- cv.glmnet(train,y_train)
  
 
  
  #ordforres <- ordfor(depvar="OrdinalETL", data=ordfor_train, nsets=1000, ntreeperdiv=100,ntreefinal=5000, perffunction = "probability")
  
  one.pred <- function(x)rep(x, nrow(test))
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  freq <-as.data.frame(table(y_train))
  median.values <- sort(y_train)
  median.ind.val <- median(median.values)
  mean.ind.val <- mean(y_train)
  
  predictions.list <-  list(
    glmnet.gaussian=as.numeric(predict(cv.fit.gaussian,newx=test,s=cv.fit.gaussian$lambda.1se,type="response")),
    baseline.l0=one.pred(as.numeric.factor(freq[which.max(freq$Freq),]$y_train)),
    baseline.l1=one.pred(median.ind.val),
    baseline.l2=one.pred(mean.ind.val)
    #ordnet.pred = predict(ordnet$fit,newx=(test), type="class",whichLambda=bestLambdaIndex)
    #ordinalForest=as.integer(predict(ordforres, newdata=(ordfor_test),type="class")$ypred)
   )
  
  accuracy.dt.list <- list()
  for(algo in names(predictions.list)){
    print(algo)
    pred.vec = predictions.list[[algo]]
    #print(class(pred.vec))
    accuracy.dt.list[[algo]] <- data.table(
      algo.name = algo,
      meanabs.error.percent= mean(abs((pred.vec) - (y_test)))
      #rmse.error.percent = sqrt(mean(((pred.vec) - (y_test$ETL.AVERAGE))^2))#l2 error
    )
  }
  
  accuracy.dt[[i]] <- data.table(fold=i,accuracies = do.call(rbind, accuracy.dt.list))
}
final.accuracy.list <- do.call(rbind, accuracy.dt) 
final.accuracy.list
error.values = final.accuracy.list$accuracies.meanabs.error.percent#final.accuracy.list$accuracies.error.percent
model=final.accuracy.list$accuracies.algo.name
ggplot()+
  geom_point(aes(
    x = error.values,y=model)) + ggtitle("CV.glmnet VS Baselines") 
   # x = error.values,y=model)) + ggtitle("L2ordinalNetVsglmnetVsbaslinesVsordinalForest") 


# final.accuracy.list <- do.call(rbind, accuracy.dt) 
# final.accuracy.list
# error.values = final.accuracy.list$accuracies.rmse.error.percent#final.accuracy.list$accuracies.error.percent
# model=final.accuracy.list$accuracies.algo.name
# ggplot()+
#   geom_point(aes(
#     x = error.values,y=model)) + ggtitle("L1ordinalNetVsglmnetVsbaslinesVsordinalForest") 


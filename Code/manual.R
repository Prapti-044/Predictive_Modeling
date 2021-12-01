#r_add.R


cwis.data <- read.csv("C:\\Users\\sh2742\\Documents\\CS-599\\GRA\\cwisdata.csv") #,header = TRUE,check.names=TRUE,format = "%m/%d/%Y"  )
required.cwis.cols <- cwis.data[,c(2, 9, 62)]

required.cwis.cols <- required.cwis.cols[!(is.na(required.cwis.cols$created_at) | required.cwis.cols$created_at==""), ]

required.cwis.cols$Date <- sapply(strsplit(required.cwis.cols$created_at,' '), "[", 1)

#11/23/2020  	2021-03-02 y-m-d

required.cwis.cols$period <- ""
for(i in 1:nrow(required.cwis.cols)){
  date <-required.cwis.cols$Date[i]
  print(date)
  Month <- as.numeric(sapply(strsplit(as.character(date),'-'), "[", 2))
  Year <- as.numeric(sapply(strsplit(as.character(date),'-'), "[", 1))
  if(Month < 3){
    val <- paste0("Aug", Year-1 , "-" , "Feb", Year)
  }else if(Month > 3 & Month < 8){
    val <- paste0("Mar", Year , "-" , "July" , Year)
    print(val)
  }else{
    val <- paste0("Aug", Year , "-" , "Feb" , Year+1)
  }
  required.cwis.cols$period[i] <- val 
}
# required.timeframe.cols$Date.of.Event.Visit <- as.Date(required.timeframe.cols$Date.of.Event.Visit, "%m/%d/%Y")
# 
# required.timeframe.cols$Date.of.Event.Visit.Year <- as.numeric(format(required.timeframe.cols$Date.of.Event.Visit, "%Y"))
# required.timeframe.cols$Date.of.Event.Visit.Month <- as.numeric(format(required.timeframe.cols$Date.of.Event.Visit, "%m"))

required.cwis.cols$ETL.AVERAGE[is.na(required.cwis.cols$ETL.AVERAGE)] <- 0

cwis.cols <- required.cwis.cols[,c(1, 2, 5)]

cwis.aggregated.district.period <- aggregate(.~State.District.ID+period, cwis.cols, mean) #mean

cwis.aggregated.district.period <- aggregate(.~State.District.ID+period, cwis.cols, sum) #sum

# MERGE

cwis.logs.aggregate <- merge(aggregated.district.yearwise.data, cwis.aggregated.district.period, by=c("State.District.ID", "period"))

cwis.logs.aggregate$Duration.of.Event <- period_to_seconds(cwis.logs.aggregate$Duration.of.Event)

cwis.logs.aggregate$Duration.of.Event <- seconds_to_period(cwis.logs.aggregate$Duration.of.Event)

#Lasso

cwis.logs.aggregate <- as.data.frame(cwis.logs.aggregate)
input.cols <- c("Duration.of.Event", "frequency.month", "gap.month")

random_sample <- createDataPartition(cwis.logs.aggregate$ETL.AVERAGE, p = 0.8, list = FALSE)
input.columns <- cwis.logs.aggregate[,input.cols]

training_dataset <- input.columns[random_sample, ]
#testing_dataset <- input.data[~random_sample, ]
x <- as.matrix(training_dataset)
head(x)
# y <- as.factor(merged.data[random_sample,7])
y <- cwis.logs.aggregate[random_sample,6]
head(y)

library(lars)
fit <- lars(x,y,type="lasso")
fit$lambda

pred.nox <- predict(fit, type="coef")
beta <- scale(pred.nox$coefficients, FALSE, 1/fit$normx)
arclength <- rowSums(abs(beta))
path.list <- list()
for(variable in colnames(beta)){
  standardized.coef <- beta[, variable]
  path.list[[variable]] <- data.table::data.table(
    step=seq_along(standardized.coef),
    lambda=c(fit$lambda, 0),
    variable,
    standardized.coef,
    fraction=pred.nox$fraction,
    arclength)
}
path <- do.call(rbind, path.list)
variable.colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A")

library(animint2)
library("ggplot2")
gg.lambda <- ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  scale_color_manual(values=variable.colors)+
  geom_line(aes(
    lambda, standardized.coef, color=variable, group=variable),
    data=path)+
  ggtitle("LASSO path for ETL learning calculated using the LARS")

gg.lambda

#Apply to cvglmnet model


weight.dt.list <- list()
for(i in 1:10){
  set.seed(i)
  print(i)
  fold.i<- folds.input.data.updatedTypes[folds.input.data.updatedTypes$fold == i,]
  trainind <- sort(sample(1:nrow(fold.i), size=floor(nrow(fold.i)*(3/4))))
  testind <- setdiff(1:nrow(fold.i), trainind)
  datatrain <- fold.i[trainind,]
  datatest <- fold.i[testind,]
  y_obs =  datatest[,4] 
  x = as.matrix(datatrain[,input.cols])
  y = datatrain[,4]
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


#================================error comparison with baselines================================================


final.accuracy.list <- list()
accuracy.dt <- list()
predictions.list <- list()
split = 10
for(i in 1:split){
  set.seed(i)
  
  fold.i<- folds.input.data.updatedTypes[folds.input.data.updatedTypes$fold == i,]
  trainind <- sort(sample(1:nrow(fold.i), size=floor(nrow(fold.i)*(3/4))))
  testind <- setdiff(1:nrow(fold.i), trainind)
  datatrain <- fold.i[trainind,]
  datatest <- fold.i[testind,]
  
  y_train <- as.matrix(datatrain[,4])
  
  ord_y_test <- datatest[,4]
  y_test <- datatest[,4]
  
  train <- as.matrix(datatrain[,-4])
  test <-  as.matrix(datatest[,-5])
  
  cv.fit.gaussian <- cv.glmnet(train,y_train)
  
  # ord_y_train = datatrain[,4]
  # ord_y_train <- as.factor(ord_y_train)
  
  # ordnet <- ordinalNetTune(train,ord_y_train)
  # bestLambdaIndex <- which.max(rowMeans(ordnet$loglik))
  
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


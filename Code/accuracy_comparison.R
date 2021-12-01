#=========== MOdel==================

for (i in 1:NROW(cwis.logs.merge)) {
  if(cwis.logs.merge$ETL.AVERAGE[i] < 2.7){cwis.logs.merge$ETL.AVERAGE[i] <- 1}
  else{cwis.logs.merge$ETL.AVERAGE[i] <- 0}
}

for (i in 1:NROW(cwis.logs.merge)) {
  if ((cwis.logs.merge$ETL.AVERAGE[i]<= 1)){cwis.logs.merge$ETL.AVERAGE[i] = "Very Neg"
  } else if((cwis.logs.merge$ETL.AVERAGE[i]>1) && (cwis.logs.merge$ETL.AVERAGE[i] <=2)){cwis.logs.merge$ETL.AVERAGE[i] = "Neg"
  }else if((cwis.logs.merge$ETL.AVERAGE[i]>2) && (cwis.logs.merge$ETL.AVERAGE[i] <=3)){cwis.logs.merge$ETL.AVERAGE[i] = "Neutral"
  }else if((cwis.logs.merge$ETL.AVERAGE[i]>3) && (cwis.logs.merge$ETL.AVERAGE[i] <=4)){cwis.logs.merge$ETL.AVERAGE[i] = "Pos"
  }else{cwis.logs.merge$ETL.AVERAGE[i] = "Very Pos"}
}


periods <- unique(cwis.logs.merge$Period)

etl <- unique(cwis.logs.merge$ETL.AVERAGE)
cwis.logs.merge$ETL.AVERAGE <- as.numeric(factor(cwis.logs.merge$ETL.AVERAGE,
                                            levels = etl,
                                            labels = 1:length(etl) # follow the order of the levels
))

cwis.logs.merge$Period <- as.numeric(factor(cwis.logs.merge$Period,
                                            levels = periods,
                                            labels = 1:length(periods) # follow the order of the levels
))

input.data <- cwis.logs.merge[, -c("State.District.ID")]
# train_rows <- sample(1:nrow(input.data), 0.75*nrow(input.data))
# X <- input.data[, -c("ETL.AVERAGE")]
# y <- input.data[, c("ETL.AVERAGE")]
# 
# X_train <- as.matrix(X[train_rows, ])
# X_test <- as.matrix(X[-train_rows, ])
# y_train <- as.matrix(y[train_rows, ])
# y_test <- as.matrix(y[-train_rows, ])
# 
# cv.fit.gaussian <- cv.glmnet(X_train, y_train, family = "binomial", type.measure = "mse", nfolds=10, keep=TRUE)
# 
# weight.dt <- data.table(
#   row.name=rownames(coef(cv.fit.gaussian))[-1],
#   weight=coef(cv.fit.gaussian)[-1]
# )
# 
# # ggplot()+
# #   geom_point(aes(
# #     weight, row.name),
# #     data=weight.dt)
# 
# weight.dt[, n.nonzero := sum(weight != 0), by=row.name]
# 
# 
# ggplot()+
#   facet_grid(n.nonzero ~ ., scales="free", space="free")+
#   geom_point(aes(
#     weight, row.name),
#     data=weight.dt)



# Accuracy

split = 5
fold.vec <- sample(rep(1:split,l=nrow(input.data)))
folds.input.data <- data.table::data.table(input.data,fold=as.numeric(factor(fold.vec)))


final.accuracy.list <- list()
accuracy.dt <- list()
predictions.list <- list()

weight.dt.list <- list()

for(i in 1:split){
  set.seed(i)
  datatrain <-folds.input.data[fold != i] #69637 obs. of  129 variables:
  datatest <- folds.input.data[fold == i]
  
  y_train <- as.matrix(datatrain[,c("ETL.AVERAGE")])
  y_test <- as.matrix(datatest[,c("ETL.AVERAGE")])
  
  X_train <- as.matrix(datatrain[,-c("ETL.AVERAGE")])
  X_test <-  as.matrix(datatest[,-c("ETL.AVERAGE")])
  
  cv.fit.gaussian <- cv.glmnet(X_train, y_train, family = "gaussian", type.measure = "mse", nfolds=10, keep=TRUE)
  
  weight.dt.list[[i]] <- data.table(
    i,
    row.name=rownames(coef(cv.fit.gaussian))[-1],
    weight=coef(cv.fit.gaussian)[-1])
  
  one.pred <- function(x)rep(x, nrow(X_test))
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  freq <-as.data.frame(table(y_train))
  median.values <- sort(y_train)
  median.ind.val <- median(median.values)
  mean.ind.val <- mean(y_train)
  
  predictions.list <-  list(
    glmnet.gaussian=as.numeric(predict(cv.fit.gaussian,newx=X_test,s=cv.fit.gaussian$lambda.1se,type="response")),
    baseline.l0=one.pred(as.numeric.factor(freq[which.max(freq$Freq),]$y_train)),
    baseline.l1=one.pred(median.ind.val),
    baseline.l2=one.pred(mean.ind.val)
  )
  
  accuracy.dt.list <- list()
  for(algo in names(predictions.list)){
    pred.vec = predictions.list[[algo]]
    accuracy.dt.list[[algo]] <- data.table(
      algo.name = algo,
      meanabs.error.percent= mean(abs((pred.vec) - (y_test)))
      #rmse.error.percent = sqrt(mean(((pred.vec) - (y_test))^2))#l2 error
    )
  }
  
  accuracy.dt[[i]] <- data.table(fold=i,accuracies = do.call(rbind, accuracy.dt.list))
}

weight.dt <- do.call(rbind, weight.dt.list)

weight.dt[, n.nonzero := sum(weight != 0), by=row.name]


ggplot()+
  facet_grid(n.nonzero ~ ., scales="free", space="free")+
  geom_point(aes(
    weight, row.name),
    data=weight.dt)

final.accuracy.list <- do.call(rbind, accuracy.dt) 
final.accuracy.list
error.values = final.accuracy.list$accuracies.meanabs.error.percent#final.accuracy.list$accuracies.error.percent
model=final.accuracy.list$accuracies.algo.name
ggplot()+
  geom_point(aes(
    x = error.values,y=model)) + ggtitle("CV.glmnet VS Baselines") 


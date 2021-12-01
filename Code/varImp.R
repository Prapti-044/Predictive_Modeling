library(data.table)
library(glmnet)
library(ggplot2)

library(tidyverse)
library(caret)
library(datarium)
library(glmnet)

library("ordinalForest")
#install.packages("ordinalForest")

input.data<- data.table::fread("C:\\Users\\sh2742\\Documents\\CS-599\\GRA\\merged.data.csv")
head(input.data)
print(names(input.data))
#total rows = 25547
# training - 20439
# testing - 5108
colnames(input.data)[7] <- "ETLAverage"
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

levels <- c("Very negative","Negative","Neutral","Positive","Very positive")
folds.input.data$OrdinalETL <- factor(folds.input.data$OrdinalETL,levels)
head(folds.input.data)
str(folds.input.data)

weight.dt.list <- list()
for(i in 1:2){
  set.seed(i)
  print(i)
  fold.i <-  folds.input.data[folds.input.data$fold == i,]
  #folds.input.data.i$OrdinalETL <- factor(folds.input.data.i$OrdinalETL,levels)
  # folds.input.data.i <- folds.input.data.i[,-27]
  
  #fold.i<- folds.input.data.updatedTypes[folds.input.data.updatedTypes$fold == i,]
  fold.i$OrdinalETL <- factor(fold.i$OrdinalETL,levels)
  
  trainind <- sort(sample(1:nrow(fold.i), size=floor(nrow(fold.i)*(3/4))))
  testind <- setdiff(1:nrow(fold.i), trainind)
  datatrain <- fold.i[trainind,]
  datatest <- fold.i[testind,]
  
  ordfor_train <- datatrain[,-7] #137
  ordfor_test <- datatest[,-7] #137
  
  #set.seed(1011)
  ordforres <- ordfor(depvar="OrdinalETL", data=ordfor_train, nsets=1000, ntreeperdiv=100,
                      ntreefinal=5000, perffunction = "probability")
  
  sort(ordforres$varimp, decreasing=TRUE)
  
# boxplot(ordfor_train$State.District.ID ~ ordfor_train$OrdinalETL, horizontal=TRUE)
fisher.test(table(datatrain$State.District.ID, datatrain$OrdinalETL))
 
  weight.dt.list[[i]] <- data.table(
    i,
    row.name=rownames(ordforres$varimp),
    weight=coef(ordforres$varimp)[-1])
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


library("lubridate") 
library("ggplot2")
library("data.table")

library(tidyverse)
library(caret)
library(ordinalNet)
library("datarium")


coachingdatecheck <- read.csv("C:\\Users\\sh2742\\Documents\\CS-599\\GRA\\Coaching logs Fall 2017- Spring 2021.csv") #,header = TRUE,check.names=TRUE,format = "%m/%d/%Y"  )
required.timeframe.cols <- coachingdatecheck[,c(4,5,8)]
head(required.timeframe.cols)
tail(required.timeframe.cols)

required.timeframe.cols$Duration.of.Event[required.timeframe.cols$Duration.of.Event == ""] <- 0

required.timeframe.cols$Duration.of.Event[required.timeframe.cols$Duration.of.Event == "NA"] <- 0
required.timeframe.cols$Duration.of.Event  <- gsub("\\.000","",required.timeframe.cols$Duration.of.Event)
# required.timeframe.cols$Duration.of.Event  <- gsub("60:00","1",required.timeframe.cols$Duration.of.Event)
# required.timeframe.cols$Duration.of.Event  <-gsub(":",".",gsub(":00","",required.timeframe.cols$Duration.of.Event))

## Minutes Converstion
required.timeframe.cols$Duration.of.Event <- gsub("min","minutes",required.timeframe.cols$Duration.of.Event,fixed = T)
required.timeframe.cols$Duration.of.Event <- gsub("Minutes","minutes",required.timeframe.cols$Duration.of.Event,fixed = T)

required.timeframe.cols$Duration.of.Event <- gsub("mnutes","minutes",required.timeframe.cols$Duration.of.Event,fixed = T)
required.timeframe.cols$Duration.of.Event <- gsub(" minutes ","minutes",required.timeframe.cols$Duration.of.Event,fixed = T)

required.timeframe.cols$Duration.of.Event <- gsub("minutesutes","minutes",required.timeframe.cols$Duration.of.Event,fixed = T)



##HOURS REPLACEMENTS
required.timeframe.cols$Duration.of.Event <- gsub(",",".",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub(" ","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("houra$"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("hour$"," hours",required.timeframe.cols$Duration.of.Event)
#required.timeframe.cols$Duration.of.Event <- gsub("hour"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("hoirs$"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("Hours"," hours",required.timeframe.cols$Duration.of.Event)

required.timeframe.cols$Duration.of.Event <- gsub("hrs.$"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("hrs$"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("hr$"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("hr"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("h$"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("hh$"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("Hrs$"," hours",required.timeframe.cols$Duration.of.Event)


##UNNECCESSARY DATA REMOVAL

required.timeframe.cols$Duration.of.Event <- gsub("s\\(duetolatestartforinclementweather\\)","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("sonsite;9 hoursstravel","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("sand"," ",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("sonsite.4 hoursstravel","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("sonsite;3 hoursstravel$","",required.timeframe.cols$Duration.of.Event)

required.timeframe.cols$Duration.of.Event <- gsub("sonsite;3.5 hoursstravel","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub(".inElem.1.5 hours.H.S.","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("1.5=","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("1.25\\(","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("\\)","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("one","1",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("h hours","hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("FullDay","8:00:00",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("allday","8:00:00",required.timeframe.cols$Duration.of.Event)

required.timeframe.cols$Duration.of.Event <- gsub("plus3hourstravelingtime","",required.timeframe.cols$Duration.of.Event)

required.timeframe.cols$Duration.of.Event <- gsub("plus3travelingtime","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("2hoursdrivingtime","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("plus2hourstravelingtime","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("plus2hourstravelingtime$","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("2hoursdrivingtime$","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("2hourdrivingtime$","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("2hourdrivingtime","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub(";2 hoursstravel","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("s;2 hoursstravel","",required.timeframe.cols$Duration.of.Event)


required.timeframe.cols$Duration.of.Event <- gsub("drivetime2hours","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("sessionsat1.5eac ","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("sessionsat1.5","hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("120minutes/","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("1/2day","4:00:00",required.timeframe.cols$Duration.of.Event)



required.timeframe.cols$Duration.of.Event <- gsub("l hours$","1 hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("i hours","1 hours",required.timeframe.cols$Duration.of.Event)
#required.timeframe.cols$Duration.of.Event <- gsub("^.","0.",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("\\(2days\\)","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("s\\(2days\\)","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("inp.m.","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("Hrs\\(2days","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("s\\(2days","",required.timeframe.cols$Duration.of.Event)


for(tm in 1:nrow(required.timeframe.cols)){
  #CV.Duration.of.Event <- 0
  if(grepl("minutes",required.timeframe.cols$Duration.of.Event[tm])){
    #print(required.timeframe.cols$Duration.of.Event[tm])
    CV.Duration.of.Event <- as.numeric(gsub("minutes","",required.timeframe.cols$Duration.of.Event[tm]))/60
    if(is.na(CV.Duration.of.Event)){
      CV.Duration.of.Event <- gsub("minutes","",required.timeframe.cols$Duration.of.Event[tm])
      CV.Duration.of.Event <- gsub(" ",".",CV.Duration.of.Event)
    }
    required.timeframe.cols$Duration.of.Event[tm] = CV.Duration.of.Event
    print(CV.Duration.of.Event)
  }
}


required.timeframe.cols$Duration.of.Event <- gsub("[hours]+",".",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("[hour]+",".",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("[hourand]+",".",required.timeframe.cols$Duration.of.Event)

required.timeframe.cols$Duration.of.Event <- gsub("\\s$","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("\\.+",".",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("\\.$","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("\\s$","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("^\\.","00:",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("^\\:","00:",required.timeframe.cols$Duration.of.Event)





for(tm in 1:nrow(required.timeframe.cols)){
  if(length( strsplit(required.timeframe.cols$Duration.of.Event[tm], ":", fixed=TRUE)[[1]]) == 1){
    if(length( strsplit(required.timeframe.cols$Duration.of.Event[tm], ".", fixed=TRUE)[[1]]) == 1){
      # print("a")
      if(required.timeframe.cols$Duration.of.Event[tm] < 10){
        required.timeframe.cols$Duration.of.Event[tm]<-paste0(required.timeframe.cols$Duration.of.Event[tm],":00:00")
      }
    }else{
      value <- as.numeric(required.timeframe.cols$Duration.of.Event[tm])
      modifier <- format(round(value, 2), nsmall = 2)
      #print(paste0(modifier))
      # print("sorry although one needs split")
      if(length( strsplit(modifier, ".", fixed=TRUE)[[1]]) > 1){
        hours <- as.numeric(gsub("\\..*","",modifier))
        minutes <- as.numeric(gsub(".*\\.","",modifier))
        if(minutes/60 > 1){
          #  print("b")
          fhours <- as.numeric(gsub("\\..*","",as.character((minutes/60) + hours)))
          fminutes <- gsub(".*\\.","",as.character((minutes/60) + hours))
          required.timeframe.cols$Duration.of.Event[tm] <-paste0(fhours,":",round(as.numeric(fminutes), 2),":00")
          # print(paste0(fhours,":",round(as.numeric(fminutes), 2),":00"))
          # required.timeframe.cols$Duration.of.Event[tm] <-paste0(fhours,":",format(round(as.numeric(fminutes), 2), nsmall = 2),":00")
        }else{
          #print("c")
          required.timeframe.cols$Duration.of.Event[tm] <-paste0(hours,":",round(as.numeric(sub("\\..*","",as.character((minutes)))), 2),":00")
          #print(paste0(hours,":",round(as.numeric(sub("\\..*","",as.character((minutes)))), 2),":00"))
          #required.timeframe.cols$Duration.of.Event[tm] <-paste0(hours,":",format(round(as.numeric(sub("\\..*","",as.character((minutes/60)))), 2), nsmall = 2),":00")
        }
        
      }
    }
  }else if(length( strsplit(required.timeframe.cols$Duration.of.Event[tm], ":", fixed=TRUE)[[1]]) == 2){
    
    required.timeframe.cols$Duration.of.Event[tm] <- paste0("00:",required.timeframe.cols$Duration.of.Event[tm])
  }else{
    #print("e")
    required.timeframe.cols$Duration.of.Event[tm] <- paste0(required.timeframe.cols$Duration.of.Event[tm])
  }
}


required.timeframe.cols$Duration.of.Event <- gsub("11666666666667","00",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("42760","00:42:00",required.timeframe.cols$Duration.of.Event)

for(tm in 1:nrow(required.timeframe.cols)){
  if(required.timeframe.cols$Duration.of.Event[tm] == "3"){
    required.timeframe.cols$Duration.of.Event[tm] <- "03:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "6"){
    required.timeframe.cols$Duration.of.Event[tm] <- "06:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "8"){
    required.timeframe.cols$Duration.of.Event[tm] <- "08:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "0"){
    required.timeframe.cols$Duration.of.Event[tm] <- "00:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "2"){
    required.timeframe.cols$Duration.of.Event[tm] <- "02:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "5"){
    required.timeframe.cols$Duration.of.Event[tm] <- "05:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "4"){
    required.timeframe.cols$Duration.of.Event[tm] <- "04:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "45"){
    required.timeframe.cols$Duration.of.Event[tm] <- "00:45:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "7"){
    required.timeframe.cols$Duration.of.Event[tm] <- "07:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "10"){
    required.timeframe.cols$Duration.of.Event[tm] <- "10:00:00"
  }
}

required.timeframe.cols$Duration.of.Event  <- hms(required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event
required.timeframe.cols$Duration.of.Event <- period_to_seconds(required.timeframe.cols$Duration.of.Event)
# required.timeframe.cols$Duration.of.Event


#must be present
required.timeframe.cols$Date.of.Event.Visit[21] <- "10/12/2019"
required.timeframe.cols$Date.of.Event.Visit[2837] <- "1/1/2019"
required.timeframe.cols$Date.of.Event.Visit[1603] <- "4/14/2021"
required.timeframe.cols$Date.of.Event.Visit[2083] <- "9/25/2020"
required.timeframe.cols$Date.of.Event.Visit[2939] <- "12/9/2017"


required.timeframe.cols$period <- ""
for(i in 1:nrow(required.timeframe.cols)){
  date <-required.timeframe.cols$Date.of.Event.Visit[i]
  print(date)
  Month <- as.numeric(sapply(strsplit(as.character(date),'/'), "[", 1))
  Year <- as.numeric(sapply(strsplit(as.character(date),'/'), "[", 3))
  
  if(Month < 3){
    val <- paste0("Aug", Year-1 , "-" , "Feb", Year)
  }else if(Month > 3 & Month < 8){
    val <- paste0("Mar", Year , "-" , "July" , Year)
    print(val)
  }else{
    val <- paste0("Aug", Year , "-" , "Feb" , Year+1)
  }
  required.timeframe.cols$period[i] <- val
}

# required.timeframe.cols$Interaction.Type[required.timeframe.cols$Interaction.Type == "Virtual w/ Video" 
#                                            | required.timeframe.cols$Interaction.Type == "Virtual w/video"] <- "Virtual w/ Video"
# 
# required.timeframe.cols$Interaction.Type[required.timeframe.cols$Interaction.Type == "In-Person" 
#                                          | required.timeframe.cols$Interaction.Type == "In-person"] <- "In-Person"
# 
# required.timeframe.cols$Interaction.Type[required.timeframe.cols$Interaction.Type == "Phone/conference call" 
#                                          | required.timeframe.cols$Interaction.Type == "phone"
#                                          | required.timeframe.cols$Interaction.Type == "Phone"
#                                          | required.timeframe.cols$Interaction.Type == "Conference Call"
#                                          | required.timeframe.cols$Interaction.Type == "Phone/Conference Call"] <- "Phone/Conference Call"
# 
# required.timeframe.cols$Interaction.Type[required.timeframe.cols$Interaction.Type == "In-Person & Virtual" 
#                                          | required.timeframe.cols$Interaction.Type == "In-person and Virtual"] <- "In-person and Virtual"
# 
# required.timeframe.cols$Interaction.Type[required.timeframe.cols$Interaction.Type == ""] <- 0

# required.timeframe.cols$period[21] <- "Aug2019-Feb2019"
# required.timeframe.cols$period[2083] <- "Aug2019-Feb2021"
# required.timeframe.cols$period[2939] <- "Aug2017-Feb2018"
# required.timeframe.cols$period[1603] <- "Mar2021-July2021"

required.timeframe.cols$Date.of.Event.Visit <- as.Date(required.timeframe.cols$Date.of.Event.Visit, "%m/%d/%Y")

required.timeframe.cols$Date.of.Event.Visit.Year <- as.numeric(format(required.timeframe.cols$Date.of.Event.Visit, "%Y"))
required.timeframe.cols$Date.of.Event.Visit.Month <- as.numeric(format(required.timeframe.cols$Date.of.Event.Visit, "%m"))
# required.timeframe.cols$Date.of.Event.Visit.Day <- as.numeric(format(required.timeframe.cols$Date.of.Event.Visit, "%d"))


# two anomalies
unique(required.timeframe.cols$period)

#required.timeframe.cols$Duration.of.Event <- seconds_to_period(required.timeframe.cols$Duration.of.Event)

coaching.aggregate.func <- function(vec){
  months.count <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  for(i in 1:nrow(vec)) {
    row <- vec[i]
    months.count[row$Date.of.Event.Visit.Month - 1] <- months.count[row$Date.of.Event.Visit.Month - 1] + 1
  }
  total.active.months <- 0
  sum <- 0
  for(i in months.count) {
    if( i > 0) {
      total.active.months <- total.active.months + 1
      sum <- sum + i
    }
  }
  
  freq <- if(total.active.months == 0) {
    0
  } else {
    sum / total.active.months
  } 
  gap <- 12 - total.active.months
  # ret.list <- list("frequency.month" = freq, "gap.month" = gap)
  ret.list <- list("frequency.month" = freq, "gap.month" = gap)
  ret.dt <- as.data.table(ret.list)
  return(ret.dt)
}

required.timeframe.cols <- as.data.table(required.timeframe.cols)

#required.timeframe.cols$Duration.of.Event <- seconds_to_period(required.timeframe.cols$Duration.of.Event)


required.timeframe.cols[,Date.of.Event.Visit:=NULL]
required.timeframe.cols[,Date.of.Event.Visit.Year:=NULL]



#aggregated.district.yearwise.data <- aggregate(.~State.District.ID+period, required.timeframe.cols, sum)  #sum

aggregated.district.yearwise.data <- aggregate(.~State.District.ID+period, required.timeframe.cols, mean) #mean
aggregated.district.yearwise.data <- as.data.table(aggregated.district.yearwise.data)

for(i in 1:nrow(aggregated.district.yearwise.data)) {
  row <- aggregated.district.yearwise.data[i,]
  data.to.aggregate <- as.data.table(required.timeframe.cols[State.District.ID == row$State.District.ID & period == row$period])
  
  result <- coaching.aggregate.func(data.to.aggregate)
  
  aggregated.district.yearwise.data[i,frequency.month:=result$frequency.month]
  aggregated.district.yearwise.data[i,gap.month:=result$gap.month]
}

aggregated.district.yearwise.data$Duration.of.Event <- seconds_to_period(aggregated.district.yearwise.data$Duration.of.Event)
aggregated.district.yearwise.data[,Date.of.Event.Visit.Month:=NULL]

# go to manual.R

cwis.data <- read.csv("C:\\Users\\sh2742\\Documents\\CS-599\\GRA\\cwisdata.csv") #,header = TRUE,check.names=TRUE,format = "%m/%d/%Y"  )
required.cwis.cols <- cwis.data[,c(2, 9, 62)]

required.cwis.cols <- required.cwis.cols[!(is.na(required.cwis.cols$created_at) | required.cwis.cols$created_at==""), ]

required.cwis.cols$created_at <- gsub(" ",required.cwis.cols$created_at)

# required.cwis.cols$ETL.AVERAGE[is.na(required.cwis.cols$ETL.AVERAGE)] <- 0

#aggregated.cwis.data <- aggregate(.~State.District.ID, required.cwis.cols, FUN = sum)

merged.data <- merge(aggregated.district.yearwise.data, required.cwis.cols, by="State.District.ID")

merged.data <- data.table(merged.data)

merged.data$Duration.of.Event <- seconds_to_period(merged.data$Duration.of.Event)


# merged.data[Duration.of.Event <= 60*60*3, Duration.of.Event := 0] # less than half day
# merged.data[Duration.of.Event > 60*60*3 & Duration.of.Event < 60*60*6, Duration.of.Event := 1] # half day
# merged.data[Duration.of.Event >= 60*60*6, Duration.of.Event := 2]  # full day

summary.merged <- aggregate(cbind(ETL.AVERAGE, frequency.month, gap.month)~Duration.of.Event, merged.data, FUN = sum)

# ggplot(data=summary.merged, aes(x=Duration.of.Event, y=ETL.AVERAGE)) +
#   geom_bar(stat="identity", fill="steelblue")+
#   theme_minimal()

dfm <- melt(summary.merged[,c('Duration.of.Event','frequency.month','gap.month', 'ETL.AVERAGE')],id.vars = 1)
names(dfm)[2] <- "Column"

ggplot(dfm,aes(x = Duration.of.Event,y = value)) + 
  geom_bar(aes(fill = Column),stat = "identity",position = "dodge") + 
  scale_y_log10()



merged.data[is.na(frequency.month)]$frequencey.month <- 0
merged.data <- as.data.frame(merged.data)
input.cols <- c("Date.of.Event.Visit.Year", "Duration.of.Event", "Date.of.Event.Visit.Month", "frequency.month", "gap.month")

random_sample <- createDataPartition(merged.data$ETL.AVERAGE, p = 0.8, list = FALSE)
input.columns <- merged.data[,input.cols]

training_dataset <- input.columns[random_sample, ]
#testing_dataset <- input.data[~random_sample, ]
x <- as.matrix(training_dataset)
head(x)
# y <- as.factor(merged.data[random_sample,7])
y <- merged.data[random_sample,7]
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
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
  "#A65628", "#F781BF", "#999999","#FFF00F")

library(animint2)
library("ggplot2")
gg.lambda <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_color_manual(values=variable.colors)+
  geom_line(aes(
    lambda, standardized.coef, color=variable, group=variable),
    data=path)+
  ggtitle("LASSO path for ETL learning calculated using the LARS")

gg.lambda

# Fit parallel cumulative logit model
# fit1 <- ordinalNet(x, y, family="cumulative", link="logit",
#                    parallelTerms=TRUE, nonparallelTerms=FALSE)
# summary(fit1)
# coef(fit1)
# coef(fit1, matrix=TRUE)
# predict(fit1, type="response")
# predict(fit1, type="class")

# Fit parallel cumulative logit model
fit1 <- ordinalNet(x, as.factor(y), family="cumulative", link="logit",
                   parallelTerms=TRUE, nonparallelTerms=FALSE, printIter=TRUE, nLambda=2, maxiterOut=10)
summary(fit1)
coef(fit1)
coef(fit1, matrix=TRUE)
predict(fit1, type="response")
ordnet.pred <- as.numeric(predict(fit1, type="class"))
error <- sqrt(mean((ordnet.pred - y)^2))

error



#new column period with values such as "Aug2019-Mar2020", "Aug2017-Mar2018" 


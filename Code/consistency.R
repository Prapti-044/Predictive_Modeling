setwd("~/GRA/VCS/Codes")

library("lubridate")
library("ggplot2")
library("data.table")

library(tidyverse)
library(caret)
library(ordinalNet)
library("datarium")
library(readxl)
library(glmnet)

#===========get pre-processed data=============
source("dataset.R")


coachingdatecheck <- get_coaching_dataset("../../Data/csv/Coaching logs Fall 2017- Spring 2021.csv")
required.coaching.cols <- coachingdatecheck[,c("Date.of.Event.Visit", "Duration.of.Event", "State.District.ID", "Period", 
                                               "Interaction.Type", "Building.level.Educators", "Collaborative.teams", "Effective.teaching.learning.practices",
                                               "Self.assessment.practice.profile...SAPP.", "Learning.module.materials..i.e..power.points..handouts.",
                                               "DESE.virtual.learning.platform", "CWIS")]

unique(required.coaching.cols$Building.level.Educators)
required.coaching.cols$Building.level.Educators[required.coaching.cols$Building.level.Educators == "yes"
                                                | required.coaching.cols$Building.level.Educators == "Yes"] <- 1
required.coaching.cols$Building.level.Educators[required.coaching.cols$Building.level.Educators == "No"
                                                | required.coaching.cols$Building.level.Educators == ""] <- 0

required.coaching.cols$CWIS[required.coaching.cols$CWIS == "yes"
                                                | required.coaching.cols$CWIS == "Yes"] <- 1
required.coaching.cols$CWIS[required.coaching.cols$CWIS == "No"
                                                | required.coaching.cols$CWIS == ""] <- 0

unique(required.coaching.cols$Collaborative.teams)
required.coaching.cols$Collaborative.teams[required.coaching.cols$Collaborative.teams == "yes"
                                           | required.coaching.cols$Collaborative.teams == "Yes"] <- 1
required.coaching.cols$Collaborative.teams[required.coaching.cols$Collaborative.teams == "No"
                                           | required.coaching.cols$Collaborative.teams == ""] <- 0

unique(required.coaching.cols$Effective.teaching.learning.practices)
required.coaching.cols$Effective.teaching.learning.practices[required.coaching.cols$Effective.teaching.learning.practices == "yes"
                                                             | required.coaching.cols$Effective.teaching.learning.practices == "Yes"] <- 1
required.coaching.cols$Effective.teaching.learning.practices[required.coaching.cols$Effective.teaching.learning.practices == "No"
                                                             | required.coaching.cols$Effective.teaching.learning.practices == ""] <- 0

unique(required.coaching.cols$Self.assessment.practice.profile...SAPP.)
required.coaching.cols$Self.assessment.practice.profile...SAPP.[required.coaching.cols$Self.assessment.practice.profile...SAPP. == "yes"
                                                                | required.coaching.cols$Self.assessment.practice.profile...SAPP. == "Yes"] <- 1
required.coaching.cols$Self.assessment.practice.profile...SAPP.[required.coaching.cols$Self.assessment.practice.profile...SAPP. == "No"
                                                                | required.coaching.cols$Self.assessment.practice.profile...SAPP. == ""] <- 0

unique(required.coaching.cols$Learning.module.materials..i.e..power.points..handouts.)
required.coaching.cols$Learning.module.materials..i.e..power.points..handouts.[required.coaching.cols$Learning.module.materials..i.e..power.points..handouts. == "yes"
                                                                               | required.coaching.cols$Learning.module.materials..i.e..power.points..handouts. == "Yes"] <- 1
required.coaching.cols$Learning.module.materials..i.e..power.points..handouts.[required.coaching.cols$Learning.module.materials..i.e..power.points..handouts. == "No"
                                                                               | required.coaching.cols$Learning.module.materials..i.e..power.points..handouts. == ""] <- 0

unique(required.coaching.cols$DESE.virtual.learning.platform)
required.coaching.cols$DESE.virtual.learning.platform[required.coaching.cols$DESE.virtual.learning.platform == "yes"
                                                      | required.coaching.cols$DESE.virtual.learning.platform == "Yes"] <- 1
required.coaching.cols$DESE.virtual.learning.platform[required.coaching.cols$DESE.virtual.learning.platform == "No"
                                                      | required.coaching.cols$DESE.virtual.learning.platform == ""] <- 0



required.coaching.cols$Duration.of.Event <- period_to_seconds(required.coaching.cols$Duration.of.Event)


required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == "Virtual w/ Video"
                                        | required.coaching.cols$Interaction.Type == "Virtual w/video"] <- "Virtual w/ Video"

required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == "In-Person"
                                        | required.coaching.cols$Interaction.Type == "In-person"] <- "In-Person"

required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == "Phone/conference call"
                                        | required.coaching.cols$Interaction.Type == "phone"
                                        | required.coaching.cols$Interaction.Type == "Phone"
                                        | required.coaching.cols$Interaction.Type == "Conference Call"
                                        | required.coaching.cols$Interaction.Type == "Phone/Conference Call"] <- "Phone/Conference Call"

required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == "In-Person & Virtual"
                                        | required.coaching.cols$Interaction.Type == "In-person and Virtual"] <- "In-person and Virtual"

required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == ""] <- 0

required.coaching.cols$Virtual.Video <- ""
required.coaching.cols$In.Person <- ""
required.coaching.cols$Phone.Conference.Call <- ""
required.coaching.cols$In.person.Virtual <- ""

for (i in 1:NROW(required.coaching.cols$Interaction.Type)) {
  if(required.coaching.cols$Interaction.Type[i] == "Virtual w/ Video"){
    required.coaching.cols$Virtual.Video[i] = 1
  }
  else if(required.coaching.cols$Interaction.Type[i] == "In-Person"){
    required.coaching.cols$In.Person[i] = 1
  }
  else if(required.coaching.cols$Interaction.Type[i] == "Phone/Conference Call"){
    required.coaching.cols$Phone.Conference.Call[i] = 1
  }
  else if(required.coaching.cols$Interaction.Type[i] == "In-person and Virtual"){
    required.coaching.cols$In.person.Virtual[i] = 1
  }
}

required.coaching.cols$Virtual.Video[required.coaching.cols$Virtual.Video == ""] <- 0
required.coaching.cols$In.Person[required.coaching.cols$In.Person == ""] <- 0
required.coaching.cols$Phone.Conference.Call[required.coaching.cols$Phone.Conference.Call == ""] <- 0
required.coaching.cols$In.person.Virtual[required.coaching.cols$In.person.Virtual == ""] <- 0

inttr.type <- required.coaching.cols[,c("Interaction.Type", "Virtual.Video", "In.Person", "Phone.Conference.Call", "In.person.Virtual")]

required.coaching.cols$Date.of.Event.Visit <- as.Date(required.coaching.cols$Date.of.Event.Visit, "%m/%d/%Y")
required.coaching.cols$Date.of.Event.Visit.Year <- as.numeric(format(required.coaching.cols$Date.of.Event.Visit, "%Y"))
required.coaching.cols$Date.of.Event.Visit.Month <- as.numeric(format(required.coaching.cols$Date.of.Event.Visit, "%m"))
required.coaching.cols = subset(required.coaching.cols, select = -c(Date.of.Event.Visit) )

coaching.aggregate.func <- function(data.to.aggregate){
  months.count <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  for(i in 1:nrow(data.to.aggregate)) {
    row <- data.to.aggregate[i,]
    months.count[row$Date.of.Event.Visit.Month] <- months.count[row$Date.of.Event.Visit.Month] + 1
  }
  
  total.Duration.of.Event <- sum(data.to.aggregate$Duration.of.Event)
  total.Duration.of.Event.mean <- mean(data.to.aggregate$Duration.of.Event)# mean
  
  data.to.aggregate$Building.level.Educators <- as.numeric(data.to.aggregate$Building.level.Educators)
  data.to.aggregate$Collaborative.teams <- as.numeric(data.to.aggregate$Collaborative.teams)
  data.to.aggregate$Effective.teaching.learning.practices <- as.numeric(data.to.aggregate$Effective.teaching.learning.practices)
  data.to.aggregate$Self.assessment.practice.profile...SAPP. <- as.numeric(data.to.aggregate$Self.assessment.practice.profile...SAPP.)
  data.to.aggregate$Learning.module.materials..i.e..power.points..handouts. <- as.numeric(data.to.aggregate$Learning.module.materials..i.e..power.points..handouts.)
  data.to.aggregate$DESE.virtual.learning.platform <- as.numeric(data.to.aggregate$DESE.virtual.learning.platform)
  data.to.aggregate$Virtual.Video <- as.numeric(data.to.aggregate$Virtual.Video)
  data.to.aggregate$In.Person <- as.numeric(data.to.aggregate$In.Person)
  data.to.aggregate$Phone.Conference.Call <- as.numeric(data.to.aggregate$Phone.Conference.Call)
  data.to.aggregate$In.person.Virtual <- as.numeric(data.to.aggregate$In.person.Virtual)
  data.to.aggregate$CWIS <- as.numeric(data.to.aggregate$CWIS)
  
  
  if(sum(data.to.aggregate$Building.level.Educators)>0){Building.level.Educators <- 1} else{Building.level.Educators <- 0}
  
  if(sum(data.to.aggregate$Collaborative.teams)>0){Collaborative.teams <- 1} else{Collaborative.teams <- 0}
  if(sum(data.to.aggregate$Effective.teaching.learning.practices)>0){Effective.teaching.learning.practices <- 1} else{Effective.teaching.learning.practices <- 0}
  if(sum(data.to.aggregate$Self.assessment.practice.profile...SAPP.)>0){Self.assessment.practice.profile...SAPP. <- 1} else{Self.assessment.practice.profile...SAPP. <- 0}
  if(sum(data.to.aggregate$Learning.module.materials..i.e..power.points..handouts.)>0){Learning.module.materials..i.e..power.points..handouts. <- 1} else{Learning.module.materials..i.e..power.points..handouts. <- 0}
  if(sum(data.to.aggregate$DESE.virtual.learning.platform)>0){DESE.virtual.learning.platform <- 1} else{DESE.virtual.learning.platform <- 0}
  if(sum(data.to.aggregate$Virtual.Video)>0){Virtual.Video <- 1} else{Virtual.Video <- 0}
  if(sum(data.to.aggregate$In.Person)>0){In.Person <- 1} else{In.Person <- 0}
  if(sum(data.to.aggregate$Phone.Conference.Call)>0){Phone.Conference.Call <- 1} else{Phone.Conference.Call <- 0}
  if(sum(data.to.aggregate$In.person.Virtual)>0){In.person.Virtual <- 1} else{In.person.Virtual <- 0}
  if(sum(data.to.aggregate$CWIS)>0){CWIS <- 1} else{CWIS <- 0}
  
  sum <- 0
  total.active.months <- 0
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
  
  ret.dt <- data.table(
    State.District.ID = data.to.aggregate$State.District.ID[1],
    Period = data.to.aggregate$Period[1],
    Sum.Duration.of.Event = total.Duration.of.Event,
    Mean.Duration.of.Event = total.Duration.of.Event.mean,
    Building.level.Educators = Building.level.Educators,
    Collaborative.teams = Collaborative.teams,
    Effective.teaching.learning.practices =Effective.teaching.learning.practices,
    Self.assessment.practice.profile...SAPP. = Self.assessment.practice.profile...SAPP.,
    Learning.module.materials..i.e..power.points..handouts. = Learning.module.materials..i.e..power.points..handouts.,
    DESE.virtual.learning.platform = DESE.virtual.learning.platform,
    Virtual.Video = Virtual.Video,
    In.Person = In.Person,
    Phone.Conference.Call = Phone.Conference.Call,
    In.person.Virtual = In.person.Virtual,
    Frequency.per.month = freq,
    Gap = gap,
    CWIS=CWIS,
    
    # Jan.Count = months.count[1],
    # Feb.Count = months.count[2],
    # Mar.Count = months.count[3],
    # Apr.Count = months.count[4],
    # May.Count = months.count[5],
    # Jun.Count = months.count[6],
    # Jul.Count = months.count[7],
    # Aug.Count = months.count[8],
    # Sep.Count = months.count[9],
    # Oct.Count = months.count[10],
    # Nov.Count = months.count[11],
    # Dec.Count = months.count[12],
    First.Half.Count.Aug.Period = months.count[8]+months.count[9]+months.count[10]+months.count[11],
    Second.Half.Count.Aug.Period = months.count[12]+months.count[1]+months.count[2],
    First.Half.Count.Mar.Period = months.count[3]+months.count[4]+months.count[5],
    Second.Half.Count.Mar.Period = months.count[6]+months.count[7]    
  )
  
  
  return(ret.dt)
}

aggregated.district.period.wise.list <- list()
unique.district.period <- unique(required.coaching.cols[, c("State.District.ID", "Period")])
for(i in 1:nrow(unique.district.period)) {
  row <- unique.district.period[i,]
  data.to.aggregate <- filter(required.coaching.cols, State.District.ID == row$State.District.ID & Period == row$Period)
  
  result <- coaching.aggregate.func(data.to.aggregate)
  
  aggregated.district.period.wise.list[[paste(row$State.District.ID, row$Period)]] <- result
}
aggregated.district.period.wise.data <- do.call(rbind, aggregated.district.period.wise.list)



cwis.data <- get_cwis_dataset("../../Data/csv/cwis_survey data updated.csv")
cwis.data$ETL.AVERAGE[is.na(cwis.data$ETL.AVERAGE)] <- 0
#cwis.data <- cwis.data[!(is.na(cwis.data$created_at) | cwis.data$created_at==""), ]
  
# write.csv(required.col,"C:\\Users\\sh2742\\Documents\\GRA\\Data\\Output.Data\\cwis_etl.csv", row.names = FALSE)

#cwis.data$Date <- sapply(strsplit(cwis.data$created_at,' '), "[", 1)

cwis.data$Year <- ""
cwis.data$Month <- ""

cwis.data$Year <- substr(cwis.data$session, 1, 4)
cwis.data$Month <- substr(cwis.data$session, 5, 6)

#cwis.data = subset(cwis.data, select = -c(created_at) )

#11/23/2020  	2021-03-02 y-m-d

cwis.data$Period <- ""
for(i in 1:nrow(cwis.data)){
  Month <- as.numeric(cwis.data$Month[i])
  Year <- as.numeric(cwis.data$Year[i])
  if(Month < 3){
    val <- paste0("Aug", Year-1 , "-" , "Feb", Year)
  }else if(Month > 3 & Month < 8){
    val <- paste0("Mar", Year , "-" , "July" , Year)
  }else{
    val <- paste0("Aug", Year , "-" , "Feb" , Year+1)
  }
  cwis.data$Period[i] <- val 
}

required.cwis.cols <- cwis.data[, c("State.District.ID", "Period", "ETL.AVERAGE", "experience", "session")]
required.cwis.cols$experience[is.na(required.cwis.cols$experience)] <- 0
unique(required.cwis.cols$Period)


cwis.aggregated.district.period <- aggregate(.~State.District.ID+Period, required.cwis.cols, mean) #sum

cwis.logs.merge <- merge(aggregated.district.period.wise.data, cwis.aggregated.district.period, by=c("State.District.ID", "Period"))


# 
# l <- table(cwis.logs.merge['ETL.AVERAGE'])

#write.csv(cwis.logs.merge,"C:\\Users\\sh2742\\Documents\\GRA\\Data\\Output.Data\\cwis_logs.csv", row.names = FALSE)


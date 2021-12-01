get_coaching_dataset <- function(location) {
  coachingdatecheck <- read.csv(location) #,header = TRUE,check.names=TRUE,format = "%m/%d/%Y"  )
  required.timeframe.cols <- coachingdatecheck[,c("Duration.of.Event", "Date.of.Event.Visit")]
  
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
  
  #must be present
  required.timeframe.cols$Date.of.Event.Visit[21] <- "10/12/2019"
  required.timeframe.cols$Date.of.Event.Visit[2837] <- "1/1/2019"
  required.timeframe.cols$Date.of.Event.Visit[1603] <- "4/14/2021"
  required.timeframe.cols$Date.of.Event.Visit[2083] <- "9/25/2020"
  required.timeframe.cols$Date.of.Event.Visit[2939] <- "12/9/2017"
  
  
  required.timeframe.cols$Period <- ""
  for(i in 1:nrow(required.timeframe.cols)){
    date <-required.timeframe.cols$Date.of.Event.Visit[i]
    Month <- as.numeric(sapply(strsplit(as.character(date),'/'), "[", 1))
    Year <- as.numeric(sapply(strsplit(as.character(date),'/'), "[", 3))
    
    if(Month < 3){
      val <- paste0("Aug", Year-1 , "-" , "Feb", Year)
    }else if(Month > 3 & Month < 8){
      val <- paste0("Mar", Year , "-" , "July" , Year)
    }else{
      val <- paste0("Aug", Year , "-" , "Feb" , Year+1)
    }
    required.timeframe.cols$Period[i] <- val
  }
  
  coachingdatecheck$Duration.of.Event <- required.timeframe.cols$Duration.of.Event
  coachingdatecheck$Date.of.Event.Visit <- required.timeframe.cols$Date.of.Event.Visit
  coachingdatecheck$Period <- required.timeframe.cols$Period
  return(coachingdatecheck)
}



get_cwis_dataset <- function(location) {
  cwis.data <- read.csv(location)
  return(cwis.data)
}
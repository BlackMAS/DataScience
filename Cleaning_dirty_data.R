install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)

dirty.data <- read.csv("data.csv",stringsAsFactors=FALSE)

summarise(dirty.data)
#code to convert the date variables into actual date values rather than their original string format

dirty.data$Req.Delv.Dt <- as.Date(dirty.data$Req.Delv.Dt,format="%m/%d/%Y")
dirty.data$Shipt.Orig.Appt.Date <- as.Date(dirty.data$Shipt.Orig.Appt.Date,format="%m/%d/%Y")
dirty.data$Shipt.Origin.Appt.Date.Orig <- as.Date(dirty.data$Shipt.Origin.Appt.Date.Orig,format="%m/%d/%Y")
dirty.data$Shipt.Dest.Appt.Date <- as.Date(dirty.data$Shipt.Dest.Appt.Date,format="%m/%d/%Y")
dirty.data$Shipt.Dest.Appt.Date.Orig <- as.Date(dirty.data$Shipt.Dest.Appt.Date.Orig,format="%m/%d/%Y")

dirty.data$Shipt.Planned.Departure.Date <- as.Date(dirty.data$Shipt.Planned.Departure.Date,format="%m/%d/%Y")
dirty.data$Shipt.Planned.Arrival.Date <- as.Date(dirty.data$Shipt.Planned.Arrival.Date,format="%m/%d/%Y")
dirty.data$Shipt.Arrive.at.Origin.Date <- as.Date(dirty.data$Shipt.Arrive.at.Origin.Date,format="%m/%d/%Y")
dirty.data$Shipt.Depart.Origin.Date <- as.Date(dirty.data$Shipt.Depart.Origin.Date,format="%m/%d/%Y")

dirty.data$Shipt.Arrive.at.Dest.Date <- as.Date(dirty.data$Shipt.Arrive.at.Dest.Date,format="%m/%d/%Y")
dirty.data$ACT_DEL_DATE_FN <- as.Date(dirty.data$ACT_DEL_DATE_FN,format="%m/%d/%Y")
dirty.data$WK_ENDING <- as.Date(dirty.data$WK_ENDING,format="%m/%d/%Y")


#Code to convert times variables into acutal times from their original string values
#"01/01/2010" is just dummy data that is needed because the lubridate function of "mdy_hms" requires a date to be included 
# with times, the "01/01/2010" value will be removed from the data in later steps
#the mdy_hms is used to convert the time values in the data file from strings to actual time (in military time) so that
#calcuations can be performed on the time data

dirty.data$Shipt.Orig.Appt.Time<-paste("01/01/2010 ", dirty.data$Shipt.Orig.Appt.Time)%>%
  mdy_hms()
dirty.data$Shipt.Origin.Appt.Time.Orig<-paste("01/01/2010 ", dirty.data$Shipt.Origin.Appt.Time.Orig)%>%
  mdy_hms()

dirty.data$Shipt.Dest.Appt.Time<-paste("01/01/2010 ", dirty.data$Shipt.Dest.Appt.Time)%>%
  mdy_hms()
dirty.data$Shipt.Dest.Appt.Time.Orig<-paste("01/01/2010 ", dirty.data$Shipt.Dest.Appt.Time.Orig)%>%
  mdy_hms()

dirty.data$Shipt.Planned.Departure.Time<-paste("01/01/2010 ", dirty.data$Shipt.Planned.Departure.Time)%>%
  mdy_hms()
dirty.data$Shipt.Planned.Arrival.Time<-paste("01/01/2010 ", dirty.data$Shipt.Planned.Arrival.Time)%>%
  mdy_hms()

dirty.data$Shipt.Arrive.at.Origin.Time<-paste("01/01/2010 ", dirty.data$Shipt.Arrive.at.Origin.Time)%>%
  mdy_hms()
dirty.data$Shipt.Depart.Origin.Time<-paste("01/01/2010 ", dirty.data$Shipt.Depart.Origin.Time)%>%
  mdy_hms()

dirty.data$Shipt.Arrive.at.Dest.Time<-paste("01/01/2010 ", dirty.data$Shipt.Arrive.at.Dest.Time)%>%
  mdy_hms()


#code to separate the dummy date values from the time
dirty.data%>%
  separate(Shipt.Orig.Appt.Time, c("Dummy.remove1", "Shipt.Orig.Appt.Time"), sep = " ")%>%
  separate(Shipt.Origin.Appt.Time.Orig, c("Dummy.remove2", "Shipt.Origin.Appt.Time.Orig"), sep = " ")%>%
  
  separate(Shipt.Dest.Appt.Time, c("Dummy.remove3", "Shipt.Dest.Appt.Time"), sep = " ")%>%
  separate(Shipt.Dest.Appt.Time.Orig, c("Dummy.remove4", "Shipt.Dest.Appt.Time.Orig"), sep = " ")%>%
  
  separate(Shipt.Planned.Departure.Time, c("Dummy.remove5", "Shipt.Planned.Departure.Time"), sep = " ")%>%
  separate(Shipt.Planned.Arrival.Time, c("Dummy.remove6", "Shipt.Planned.Arrival.Time"), sep = " ")%>%
  
  separate(Shipt.Arrive.at.Origin.Time, c("Dummy.remove7", "Shipt.Arrive.at.Origin.Time"), sep = " ")%>%
  separate(Shipt.Depart.Origin.Time, c("Dummy.remove8", "Shipt.Depart.Origin.Time"), sep = " ")%>%
  
  separate(Shipt.Arrive.at.Dest.Time, c("Dummy.remove9", "Shipt.Arrive.at.Dest.Time"), sep = " ")%>%
  
  
  #code to delete all the dummy data columns
  select(-c(Dummy.remove1,Dummy.remove2,Dummy.remove3,Dummy.remove4,Dummy.remove5,Dummy.remove6,Dummy.remove7,Dummy.remove8,Dummy.remove9))%>%
  
  
  #code to create new columns that translate times into parts of the day, i.e. morning, afternoon, night, late night
  mutate(Shipt.Orig.Appt.parts.of.day = ifelse(Shipt.Orig.Appt.Time >= "05:00:00" & Shipt.Orig.Appt.Time <= "12:00:00","morning",
                                               ifelse(Shipt.Orig.Appt.Time > "12:00:00" & Shipt.Orig.Appt.Time <= "17:00:00","afternoon",
                                                      ifelse(Shipt.Orig.Appt.Time > "17:00:00" & Shipt.Orig.Appt.Time <= "21:00:00","evening", 
                                                             ifelse(Shipt.Orig.Appt.Time > "21:00:00" & Shipt.Orig.Appt.Time <= "24:00:00","night", "late night") ) ) ) )%>%
  
  
  mutate(Shipt.Origin.Appt.parts.of.day.Orig = ifelse(Shipt.Origin.Appt.Time.Orig >= "05:00:00" & Shipt.Origin.Appt.Time.Orig <= "12:00:00","morning",
                                                      ifelse(Shipt.Origin.Appt.Time.Orig > "12:00:00" & Shipt.Origin.Appt.Time.Orig <= "17:00:00","afternoon",
                                                             ifelse(Shipt.Origin.Appt.Time.Orig > "17:00:00" & Shipt.Origin.Appt.Time.Orig <= "21:00:00","evening", 
                                                                    ifelse(Shipt.Origin.Appt.Time.Orig > "21:00:00" & Shipt.Origin.Appt.Time.Orig <= "24:00:00","night", "late night") ) ) ) )%>%
  
  mutate(Shipt.Dest.Appt.parts.of.day = ifelse(Shipt.Dest.Appt.Time >= "05:00:00" & Shipt.Dest.Appt.Time <= "12:00:00","morning",
                                               ifelse(Shipt.Dest.Appt.Time > "12:00:00" & Shipt.Dest.Appt.Time <= "17:00:00","afternoon",
                                                      ifelse(Shipt.Dest.Appt.Time > "17:00:00" & Shipt.Dest.Appt.Time <= "21:00:00","evening", 
                                                             ifelse(Shipt.Dest.Appt.Time > "21:00:00" & Shipt.Dest.Appt.Time <= "24:00:00","night", "late night") ) ) ) )%>%
  
  mutate(Shipt.Dest.Appt.parts.of.day.Orig = ifelse(Shipt.Dest.Appt.Time.Orig >= "05:00:00" & Shipt.Dest.Appt.Time.Orig <= "12:00:00","morning",
                                                    ifelse(Shipt.Dest.Appt.Time.Orig > "12:00:00" & Shipt.Dest.Appt.Time.Orig <= "17:00:00","afternoon",
                                                           ifelse(Shipt.Dest.Appt.Time.Orig > "17:00:00" & Shipt.Dest.Appt.Time.Orig <= "21:00:00","evening", 
                                                                  ifelse(Shipt.Dest.Appt.Time.Orig > "21:00:00" & Shipt.Dest.Appt.Time.Orig <= "24:00:00","night", "late night") ) ) ) )%>%
  
  
  mutate(Shipt.Planned.Departure.parts.of.day = ifelse(Shipt.Planned.Departure.Time >= "05:00:00" & Shipt.Planned.Departure.Time <= "12:00:00","morning",
                                                       ifelse(Shipt.Planned.Departure.Time > "12:00:00" & Shipt.Planned.Departure.Time <= "17:00:00","afternoon",
                                                              ifelse(Shipt.Planned.Departure.Time > "17:00:00" & Shipt.Planned.Departure.Time <= "21:00:00","evening", 
                                                                     ifelse(Shipt.Planned.Departure.Time > "21:00:00" & Shipt.Planned.Departure.Time <= "24:00:00","night", "late night") ) ) ) )%>%
  
  mutate(Shipt.Planned.Arrival.parts.of.day = ifelse(Shipt.Planned.Arrival.Time >= "05:00:00" & Shipt.Planned.Arrival.Time <= "12:00:00","morning",
                                                     ifelse(Shipt.Planned.Arrival.Time > "12:00:00" & Shipt.Planned.Arrival.Time <= "17:00:00","afternoon",
                                                            ifelse(Shipt.Planned.Arrival.Time > "17:00:00" & Shipt.Planned.Arrival.Time <= "21:00:00","evening", 
                                                                   ifelse(Shipt.Planned.Arrival.Time > "21:00:00" & Shipt.Planned.Arrival.Time <= "24:00:00","night", "late night") ) ) ) )%>%
  
  
  mutate(Shipt.Arrive.at.Origin.parts.of.day = ifelse(Shipt.Arrive.at.Origin.Time >= "05:00:00" & Shipt.Arrive.at.Origin.Time <= "12:00:00","morning",
                                                      ifelse(Shipt.Arrive.at.Origin.Time > "12:00:00" & Shipt.Arrive.at.Origin.Time <= "17:00:00","afternoon",
                                                             ifelse(Shipt.Arrive.at.Origin.Time > "17:00:00" & Shipt.Planned.Arrival.Time <= "21:00:00","evening", 
                                                                    ifelse(Shipt.Arrive.at.Origin.Time > "21:00:00" & Shipt.Arrive.at.Origin.Time <= "24:00:00","night", "late night") ) ) ) )%>%
  
  
  mutate(Shipt.Depart.Origin.parts.of.day = ifelse(Shipt.Depart.Origin.Time >= "05:00:00" & Shipt.Depart.Origin.Time <= "12:00:00","morning",
                                                   ifelse(Shipt.Depart.Origin.Time > "12:00:00" & Shipt.Depart.Origin.Time <= "17:00:00","afternoon",
                                                          ifelse(Shipt.Depart.Origin.Time > "17:00:00" & Shipt.Depart.Origin.Time <= "21:00:00","evening", 
                                                                 ifelse(Shipt.Depart.Origin.Time > "21:00:00" & Shipt.Depart.Origin.Time <= "24:00:00","night", "late night") ) ) ) )%>%
  
  
  mutate(Shipt.Arrive.at.Dest.parts.of.day = ifelse(Shipt.Arrive.at.Dest.Time >= "05:00:00" & Shipt.Arrive.at.Dest.Time <= "12:00:00","morning",
                                                    ifelse(Shipt.Arrive.at.Dest.Time > "12:00:00" & Shipt.Arrive.at.Dest.Time <= "17:00:00","afternoon",
                                                           ifelse(Shipt.Arrive.at.Dest.Time > "17:00:00" & Shipt.Arrive.at.Dest.Time <= "21:00:00","evening", 
                                                                  ifelse(Shipt.Arrive.at.Dest.Time > "21:00:00" & Shipt.Arrive.at.Dest.Time <= "24:00:00","night", "late night") ) ) ) )%>%
  
  
  #Code to add new variables that translante dates into days of the week
  mutate(requested.day = weekdays(Req.Delv.Dt))%>%
  mutate(Shipt.Orig.Appt.Day = weekdays(Shipt.Orig.Appt.Date))%>%
  mutate(Shipt.Origin.Appt.Day.Orig = weekdays(Shipt.Origin.Appt.Date.Orig))%>%
  mutate(Shipt.Dest.Appt.Day = weekdays(Shipt.Dest.Appt.Date))%>%
  mutate(Shipt.Dest.Appt.Day.Orig = weekdays(Shipt.Dest.Appt.Date.Orig))%>%
  mutate(Shipt.Planned.Departure.Day = weekdays(Shipt.Planned.Departure.Date))%>%
  mutate(Shipt.Planned.Arrival.Day = weekdays(Shipt.Planned.Arrival.Date))%>%
  mutate(Shipt.Arrive.at.Origin.Day = weekdays(Shipt.Arrive.at.Origin.Date))%>%
  mutate(Shipt.Depart.Origin.Day = weekdays(Shipt.Depart.Origin.Date))%>%
  mutate(Shipt.Arrive.at.Dest.Day = weekdays(Shipt.Arrive.at.Dest.Date))%>%
  mutate(Late.Delivery = ifelse(Shipt.Arrive.at.Dest.Date > Req.Delv.Dt,1,0))%>%
  mutate(Planned.As.Requested= ifelse(Req.Delv.Dt == Shipt.Planned.Arrival.Date,1,0))%>%
  #View()
  write.csv("Clean.Data/cleaner.data.csv")
  
  str(dirty.data)


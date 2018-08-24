install.packages("tidyverse")
install.packages("lubridate")


library(tidyverse)
library(lubridate)
library(ggplot2)

#amelia is the package to visualize missing values
install.packages("Amelia")
library(amelia)

#Pscl is the package to show the MCFadden R^2
install.packages("pscl")
library(pscl)

#cleaner.data<-read.csv("Clean.Data/cleaner.data.csv",stringsAsFactors=FALSE)
cleaner.data<-read.csv("Clean.Data/cleaner.data.csv")

#Create a unique identifier since there isn't one in the original data set (always ask for it)
cleaner.data <- cleaner.data%>%
  mutate(SID.Order.Num = paste0(cleaner.data$SID,cleaner.data$Order..))


#Output descriptive statistics for each column of data        
Desc.stats <- summary(cleaner.data)
Desc.stats
str(cleaner.data)

#output the number of missing values for each column
sapply(cleaner.data, function(x) sum(is.na(x)))


#quikcly check for how many different values for each feature
sapply(cleaner.data, function(x) length(unique(x)))

#A visual way to check for missing data
Amelia::missmap(cleaner.data, main='Missing values vs observed')


#------------------------------------------------------------------------------------------------------------------------
#create barplots for original data set
barplot(table(cleaner.data$Shipt.Orig.Appt.parts.of.day))
barplot(table(cleaner.data$Shipt.Origin.Appt.parts.of.day.Orig))

barplot(table(cleaner.data$Shipt.Dest.Appt.parts.of.day))
barplot(table(cleaner.data$Shipt.Dest.Appt.parts.of.day.Orig))

barplot(table(cleaner.data$Shipt.Planned.Departure.parts.of.day))
barplot(table(cleaner.data$Shipt.Planned.Arrival.parts.of.day))

barplot(table(cleaner.data$Shipt.Arrive.at.Origin.parts.of.day))
barplot(table(cleaner.data$Shipt.Depart.Origin.parts.of.day))

barplot(table(cleaner.data$Shipt.Arrive.at.Dest.parts.of.day))

#------------------------------------------------------------------------------------------------------------------------
#create new data sets for Planned as requested vs not planned as requested 
planned.requested.match <- select(filter(cleaner.data,Planned.As.Requested == 1),everything())
planned.requested.no.match <- select(filter(cleaner.data,Planned.As.Requested == 0),everything())
planned.requested.NA <- select(filter(cleaner.data,is.na(Planned.As.Requested)),everything())

#Show descriptive statistics on new data sets
summary(planned.requested.match)
summary(planned.requested.no.match)

#Group the late deliveries by SID+Order Number,filter for accepted, and create as a new dataset
accepted.planned.requested.match <-planned.requested.match%>%
  group_by(SID.Order.Num)%>%
  mutate(count.of.unique.id = n())%>%
  filter(Accepted==1)

  

summary(accepted.planned.requested.match)
str(accepted.planned.requested.match)


#bar plots for late deliveries (old code)
#par(las=2)  make label text perpendicular to axis
#barplot(sort (table(accepted.planned.requested.match$Shipt.Arrive.at.Dest.parts.of.day), increasing = TRUE),horiz=TRUE ,main = "Arrive.at.Dest.parts.of.day")


#-------------------------------------------------------------------------------------------------------------------------
#Plotting parts of the day bar chart with ggplot (better method)
ggplot(accepted.planned.requested.match, aes(x=Shipt.Arrive.at.Dest.parts.of.day))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()


ggplot(accepted.planned.requested.match, aes(x=Shipt.Origin.Appt.parts.of.day.Orig))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()

ggplot(accepted.planned.requested.match, aes(x=Shipt.Dest.Appt.parts.of.day))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()

ggplot(accepted.planned.requested.match, aes(x=Shipt.Dest.Appt.parts.of.day.Orig))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()


ggplot(accepted.planned.requested.match, aes(x=Shipt.Planned.Departure.parts.of.day))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()


ggplot(accepted.planned.requested.match, aes(x=Shipt.Planned.Arrival.parts.of.day))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()

ggplot(accepted.planned.requested.match, aes(x=Shipt.Arrive.at.Origin.parts.of.day))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()

ggplot(accepted.planned.requested.match, aes(x=Shipt.Depart.Origin.parts.of.day))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()

#-------------------------------------------------------------------------------------------------------------------------

#Plotting day of the week bar chart with ggplot (better method)

ggplot(accepted.planned.requested.match, aes(x=requested.day))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()

ggplot(accepted.planned.requested.match, aes(x=Shipt.Orig.Appt.Day))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()

ggplot(accepted.planned.requested.match, aes(x=Shipt.Origin.Appt.Day.Orig))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()

ggplot(accepted.planned.requested.match, aes(x=Shipt.Dest.Appt.Day))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()

ggplot(accepted.planned.requested.match, aes(x=Shipt.Dest.Appt.Day.Orig))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()

ggplot(accepted.planned.requested.match, aes(x=Shipt.Planned.Departure.Day))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()

ggplot(accepted.planned.requested.match, aes(x=Shipt.Planned.Arrival.Day))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()

ggplot(accepted.planned.requested.match, aes(x=Shipt.Arrive.at.Origin.Day))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()

ggplot(accepted.planned.requested.match, aes(x=Shipt.Depart.Origin.Day))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal()+ coord_flip()

ggplot(accepted.planned.requested.match, aes(x=Shipt.Arrive.at.Dest.Day))+ geom_bar(stat="count", width=0.7, fill="steelblue")+ theme_minimal() + coord_flip()

#-------------------------------------------------------------------------------------------------------------------------
ggplot(accepted.planned.requested.match, aes(x=reorder(Carrier.Name,Late.Delivery), y=Late.Delivery))+ geom_bar(stat="identity", width=0.7, fill="steelblue")+ theme_minimal() + coord_flip()



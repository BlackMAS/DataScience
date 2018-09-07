library(caret)
library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(mice)
library(ggplot2)
library(reshape)
install.packages("fBasics") #better statistics summaries than standard summary function
library(fBasics)

library(gbm)
library(randomForest)
library(car)
library(corrplot)
#new 
library(ROCR)
library(Hmisc)
library(knitr)
library(gridExtra)

#-------------------------------------------------------------------------------------------------------------------------
#read data and look at structure
train <- read.csv('train.csv',stringsAsFactors = FALSE, na.strings = c("NA",""))
test <- read.csv('test.csv',stringsAsFactors = FALSE, na.strings = c("NA",""))

glimpse(train)
glimpse(test)

# data dictionary
#Survived	         Survival	0 = No, 1 = Yes
#Pclass	           Passenger’s class	1 = 1st, 2 = 2nd, 3 = 3rd
#Name	             Passenger’s name	
#Sex	             Passenger’s sex	
#Age	             Passenger’s age	
#SibSp             Number of siblings/spouses aboard	
#Parch	           Number of parents/children aboard	
#Ticket 	         Ticket number	
#Fare	             Fare	
#Cabin             Cabin	
#Embarked	         Port of embarkation	C = Cherbourg, Q = Queenstown, S = Southampton

# look for missing values (0's and NAs) and unique values
sapply(train, function(x) unique(x))
sapply(train, function(x) sum(is.na(x)))
round(sapply(train, function(x) sum(is.na(x))/length(x)), digits = 3)
sapply(train, function(x) length(which(x==0)))
round(sapply(train, function(x) length(which(x==0))/length(x)), digits = 2)

#combine test and training for analysis, data cleaning, and feature engineering
test$Survived <-NA

all <-rbind(train,test)
# look for missing values (0's and NAs) and unique values
sapply(all, function(x) unique(x))
sapply(all, function(x) sum(is.na(x)))
round(sapply(all, function(x) sum(is.na(x))/length(x)), digits = 3)
sapply(all, function(x) length(which(x==0)))
round(sapply(all, function(x) length(which(x==0))/length(x)), digits = 2)

#convert values to factors
cols1 <- c("PassengerId","Survived","Pclass","Sex","Embarked")
all[cols1] <- lapply(all[cols1], factor)

#-------------------------------------------------------------------------------------------------------------------------
#plots and charts

# plot those who survived
ggplot(data = all[!is.na(all$Survived),], aes(x=Survived, fill=Survived))+ 
         geom_bar(stat ="count") +
         geom_label(stat = "count",aes(label=..count..), size=7) +
         theme_light(base_size = 18)+
         labs(x = 'How many people died and survived on the Titanic?')
  
         
p1<-ggplot(data = all, aes(x=Sex, fill =Sex))+
  geom_bar(stat="count")+
  geom_label(stat="count",aes(label=..count..),size=5)+
  theme_dark()+
  labs(x='Sex of the passengers')+
  scale_fill_manual("legend", values = c("female" = "pink", "male" = "blue"))

p2<-ggplot(data = all[!is.na(all$Survived),], aes(x=Sex, fill =Survived))+
  geom_bar(stat="count", position='dodge')+
  geom_label(stat="count",aes(label=..count..),size=5)+
  theme_dark()+
  labs(x='Training data only')

grid.arrange(p1,p2, nrow=1)

p3<-ggplot(data = all, aes(x=Pclass, fill =Pclass))+
  geom_bar(stat="count")+
  geom_label(stat="count",aes(label=..count..),size=5)+
  theme_dark()+
  labs(x='Class of the passengers')

p4<-ggplot(data = all[!is.na(all$Pclass),], aes(x=Pclass, fill =Pclass))+
  geom_bar(stat="count", position='dodge')+
  geom_label(stat="count",aes(label=..count..),size=5)+
  theme_dark()+
  labs(x='Training data only')

p5<-ggplot(data = all[!is.na(all$Survived),],aes(x=Pclass,fill=Survived)) +
  geom_bar(stat = 'count',position = 'stack')+
  labs(x='Training data only', y="Count")+facet_grid(.~Sex)+
  theme(legend.position = "none")+
  theme_grey()

p6<-ggplot(data = all[!is.na(all$Survived),],aes(x=Pclass, fill=Survived))+
  geom_bar(stat = 'count', position = 'fill')+ # position = fill and Y=percent go together
  labs(x="Training data only",y='Percent')+
  facet_grid(.~Sex)+
  theme(legend.position = "none")+
  theme_grey()

grid.arrange(p3,p4,p5,p6, nrow=2)

glimpse(all)
#-------------------------------------------------------------------------------------------------------------------

# combine sex and class to see if this new variable "PclassSex" adds value

all$PclassSex[all$Sex=='male' & all$Pclass =='1']<-'P1Male'
all$PclassSex[all$Sex=='male' & all$Pclass =='2']<-'P2Male'
all$PclassSex[all$Sex=='male' & all$Pclass =='3']<-'P3Male'

all$PclassSex[all$Sex=='female' & all$Pclass =='1']<-'P1Female'
all$PclassSex[all$Sex=='female' & all$Pclass =='2']<-'P2Female'
all$PclassSex[all$Sex=='female' & all$Pclass =='3']<-'P3Female'

all$PclassSex <- as.factor(all$PclassSex)

#-----------------------------------------------------------------------------------------------------------------------------------------
#Extracting Title and Surname (last name) from Name: This function splits the name by ", ." and then creates a vector with the split name, the square brackets grabs the first element of the vector which is the last name
all$Surname <- sapply(all$Name, function(x) {strsplit(x, split='[,.]')[[1]][1]})

#some surnames have maiden names i.e. "Nicola-Yarred" and I only want the first part after the hyphen
all$Surname <- sapply(all$Surname, function(x) {strsplit(x, split='[-]')[[1]][1]})

#get the title
all$Title <- sapply(all$Name, function(x) {strsplit(x, split='[,.]')[[1]][2]})

#removing spaces before title
all$Title <- sub(' ', '', all$Title) 

#regular table
table(all$Sex, all$Title)

#nicer table
kable(table(all$Sex, all$Title))

# combine similar titles together

all$Title[all$Title %in% c("Mss","Mlle")] <- "Miss"

all$Title[all$Title =="Mme"]<-"Mrs"

all$Title[!(all$Title %in% c("Mr","Miss","Mrs","Master"))] <-"Rare Title"

all$Title<-as.factor(all$Title)

kable(table(all$Sex,all$Title))

#---------------------------------------------------------------------------------------------------------------------------------------
#plotting title and who survived

ggplot(data = all[!is.na(all$Survived),],aes(x=Title,fill=Survived)) +
  geom_bar(stat = "count",position = position_stack(reverse = TRUE))+
  theme_light() +
  labs(x='Title of passengers')

#---------------------------------------------------------------------------------------------------------------------------------------
# finding groups of people traveling together

all$Fsize <- all$SibSp+all$Parch +1

all$FsizeName<- paste(as.character(all$Fsize),all$Surname, sep="")

ggplot(data = all[!is.na(all$Survived),], aes(x=Fsize, fill=Survived))+
  geom_bar(stat = 'count',position = 'dodge')+
  scale_x_continuous(breaks=c(1:11)) +
  theme_grey()+
  labs(x='Family Size')

#check the size of families in the data
SizeCheck <- all %>%
  group_by(FsizeName,Fsize)%>%
  summarise(NumObs=n())
SizeCheck$NumFam <- SizeCheck$NumObs/SizeCheck$Fsize
SizeCheck$modulo <- SizeCheck$NumObs %% SizeCheck$Fsize

SizeCheck <- SizeCheck[SizeCheck$modulo !=0,]
sum(SizeCheck$NumObs)


#display a few Fsize and NumObs inconsistencies
kable(SizeCheck[SizeCheck$FsizeName %in% c('3Davies', '5Hocking', '6Richards', '2Wilkes', '3Richards', '4Hocking'),])

#lets take a closer look at the 3Davies family for inconsistencies
kable(all[all$FsizeName=='3Davies',c(2,3,14,5,6,7,8,17,9,15)])

#The Davies’ on Tickets A/4 48871 and A/4 48873 are very likely a complete group. The error seems to be that Mrs Davies [1222] was supposed to travel with 2 children, but eventually only traveled with one son (Master Davies [550]). A quick internet search told me that a person with the name Davies cancelled his trip indeed due to illness. Let’s correct this info.

all$FsizeName[c(550, 1222)] <- '2Davies'
all$SibSp[550]<- 0
all$Parch[1222] <- 1
all$Fsize[c(550,1222)]<-2
kable(all[all$FsizeName=='2Davies',c(2,3,14,5,6,7,8,17,9,15)])

#-------------------------------------------------------------------------------------------------------------------------
#find counsin, aunts, uncles, and grandparents of female side

NC <- all[all$FsizeName %in% SizeCheck$FsizeName,] #create data frame with only relevant Fsizenames

#extracting maiden names
NC$Name <- sub("\\s$", "", NC$Name) #removing spaces at end Name
NC$Maiden <- sub(".*[^\\)]$", "", NC$Name) #remove when not ending with ')'
NC$Maiden <- sub(".*\\s(.*)\\)$", "\\1", NC$Maiden)
NC$Maiden[NC$Title!='Mrs'] <- "" #cleaning up other stuff between brackets (including Nickname of a Mr)
NC$Maiden <- sub("^\\(", '', NC$Maiden) #removing opening brackets (sometimes single name, no spaces between brackets)
#making an exceptions match
NC$Maiden[NC$Name=='Andersen-Jensen, Miss. Carla Christine Nielsine'] <- 'Jensen'

#take only Maiden names that also exist as surname in other Observations
NC$Maiden2[NC$Maiden %in% NC$Surname] <- NC$Maiden[NC$Maiden %in% NC$Surname] 
#create surname+maiden name combinations
NC$Combi[!is.na(NC$Maiden2)] <- paste(NC$Surname[!is.na(NC$Maiden2)], NC$Maiden[!is.na(NC$Maiden2)])

#create labels dataframe with surname and maiden merged into one column
labels1 <- NC[!is.na(NC$Combi), c('Surname','Combi')]
labels2 <- NC[!is.na(NC$Combi), c('Maiden','Combi')]
colnames(labels2) <- c('Surname', 'Combi')
labels1 <- rbind(labels1, labels2)

NC$Combi <- NULL
NC <- left_join(NC, labels1, by='Surname')

#Find the maximum Fsize within each newly found 'second degree' family
CombiMaxF <- NC[!is.na(NC$Combi),] %>%
  group_by(Combi) %>%
  summarise(MaxF=max(Fsize)) #summarise(MaxF=n())
NC <- left_join(NC, CombiMaxF, by = "Combi")

#create family names for those larger families
NC$FsizeCombi[!is.na(NC$Combi)] <- paste(as.character(NC$Fsize[!is.na(NC$Combi)]), NC$Combi[!is.na(NC$Combi)], sep="")

#find the ones in which not all Fsizes are the same
FamMaid <- NC[!is.na(NC$FsizeCombi),] %>%
  group_by(FsizeCombi, MaxF, Fsize) %>%
  summarise(NumObs=n())
FamMaidWrong <- FamMaid[FamMaid$MaxF!=FamMaid$NumObs,]

kable(unique(NC[!is.na(NC$Combi) & NC$FsizeCombi %in% FamMaidWrong$FsizeCombi, c('Combi', 'MaxF')]))

#-------------------------------------------------------------------------------------------------------------------------
#find them on the male side
NC$MaxF <- NULL #erasing MaxF column maiden combi's

#Find the maximum Fsize within remaining families (no maiden combi's)
FamMale <- NC[is.na(NC$Combi),] %>%
  group_by(Surname) %>%
  summarise(MaxF=max(Fsize))
NC <- left_join(NC, FamMale, by = "Surname")

NCMale <- NC[is.na(NC$Combi),] %>%
  group_by(Surname, FsizeName, MaxF) %>%
  summarise(count=n()) %>%
  group_by(Surname, MaxF) %>%
  filter(n()>1) %>%
  summarise(NumFsizes=n())

NC$Combi[NC$Surname %in% NCMale$Surname] <- NC$Surname[NC$Surname %in% NCMale$Surname]

kable(NCMale[, c(1,2)])

#This means that altogether, there are 9 families (37 passengers) that include ‘second degree’ family members. What I want to do is give each member in such family the same Fsize (which gives everybody in these families the same survival chances with regards to the group variable). I have chosen to make this the average of the Fsize (which are based on siblings/spouse/parents/children only)

NC <- NC[(NC$FsizeCombi %in% FamMaidWrong$FsizeCombi)|(NC$Surname %in% NCMale$Surname),]

#calculating the average Fsize for those 9 families
NC1 <- NC %>%
  group_by(Combi) %>%
  summarise(Favg=mean(Fsize))
kable(NC1)


#A result is that for instance the Fsize is 4 for all 6 people in the Richards-Hockings family. This exactly what I wanted, as I wanted to combine those people into a group with all members having the same Fsize (to give equal survival chances to all members within the group) but also not the maximum size as they are less likely to stay together than first degree families.

NC <- left_join(NC, NC1, by = "Combi") #adding Favg to NC dataframe 
NC$Favg <- round(NC$Favg) #rounding those averages to integers
NC <- NC[, c('PassengerId', 'Favg')]
all <- left_join(all, NC, by='PassengerId')

#replacing Fsize by Favg
all$Fsize[!is.na(all$Favg)] <- all$Favg[!is.na(all$Favg)]

#creating a variable with almost the same ticket numbers (only last 2 digits varying)
all$Ticket2 <- sub("..$", "xx", all$Ticket)

library(dplyr)
rest <- all %>%
  select(PassengerId, Title, Age, Ticket, Ticket2, Surname, Fsize) %>%
  filter(Fsize =='1') %>%
  group_by(Ticket2, Surname) %>%
  summarise(count=n())



rest <- select(all,PassengerId, Title, Age, Ticket, Ticket2, Surname, Fsize)
rest <- rest[rest$Fsize!='1',]

rest <- rest %>%
  group_by(Ticket2, Surname) %>%
  summarise(count=n())

rest <- rest[rest$count>1,]
rest1 <- all[(all$Ticket2 %in% rest$Ticket2 & all$Surname %in% rest$Surname & all$Fsize=='1'), c('PassengerId', 'Surname', 'Title', 'Age', 'Ticket', 'Ticket2', 'Fsize', 'SibSp', 'Parch')]
rest1 <- left_join(rest1, rest, by = c("Surname", "Ticket2"))
rest1 <- rest1[!is.na(rest1$count),]
rest1 <- rest1 %>%
  arrange(Surname, Ticket2)
kable(rest1[1:12,])

all <- left_join(all, rest1)

for (i in 1:nrow(all)){
  if (!is.na(all$count[i])){
    all$Fsize[i] <- all$count[i]
  }
}

#composing data frame with group size for each Ticket
TicketGroup <- all %>%
  select(Ticket) %>%
  group_by(Ticket) %>%
  summarise(Tsize=n())
all <- left_join(all, TicketGroup, by = "Ticket")


ggplot(all[!is.na(all$Survived),], aes(x = Tsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Ticket Size') + theme_grey()



#taking the max of family and ticket size as the group size
all$Group <- all$Fsize
for (i in 1:nrow(all)){
  all$Group[i] <- max(all$Group[i], all$Tsize[i])
}

all$GroupSize[all$Group ==1] <- 'solo'
all$GroupSize[all$Group ==2] <- 'duo'
all$GroupSize[all$Group>=3 & all$Group <=4] <- 'group'
all$GroupSize[all$Group >=5] <- 'large group'


g1<-ggplot(data=all[!is.na(all$Survived),], aes(x=Group,fill=Survived))+
  geom_bar(stat = 'count',position='dodge')+
  labs(x = 'Final Group Sizes')+
  scale_x_continuous(breaks = c(1:11))+
  theme_grey()

g2 <- ggplot(all[!is.na(all$Survived),], aes(x = GroupSize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Final Group Categories') + theme_grey() +
  scale_x_discrete (limits = c('solo', 'duo', 'group', 'large group'))
grid.arrange(g2, g1)

#-------------------------------------------------------------------------------------------------------------------------
#Now to deal with the fare

#display passengers with missing Embarked

kable(all[which(is.na(all$Embarked)),c('Surname', 'Title', 'Survived', 'Pclass', 'Age', 'SibSp', 'Parch', 'Ticket', 'Fare', 'Cabin', 'Embarked', 'Group','FarePP')])

#I want to impute the missing embarkement city with the median Fare Per Person for each Embarkement city, and per Pclass.

all$FarePP <- all$Fare/all$Tsize

tab<- all[(!is.na(all$Embarked) & !is.na(all$Fare)),] %>%
  group_by(Embarked,Pclass)%>%
  summarise(FarePP=median(FarePP))
kable(tab)


#|Embarked |Pclass |  FarePP|
  #|:--------|:------|-------:|
  #|C        |1      | 34.6500|
  #|C        |2      | 13.8583|
  #|C        |3      |  7.2271|
  #|Q        |1      | 30.0000|
  #|Q        |2      | 12.3500|
  #|Q        |3      |  7.7500|
  #|S        |1      | 26.5500|
  #|S        |2      | 11.5000|
  #|S        |3      |  7.7958|

#As the FarePP of those two women is 40, they most likely embarked at Cherbourgh.
# impute the missing value for embarked

all$Embarked[all$Ticket=='113572'] <- 'C'

#display passengers with missing Fare

kable(all[which(is.na(all$Fare)), c('Surname', 'Title', 'Survived', 'Pclass', 'Age', 'SibSp', 'Parch', 'Ticket', 'Fare', 'Cabin', 'Embarked', 'Group')])

#|     |Surname |Title |Survived |Pclass |  Age| SibSp| Parch|Ticket | Fare|Cabin |Embarked | Group|
#|:----|:-------|:-----|:--------|:------|----:|-----:|-----:|:------|----:|:-----|:--------|-----:|
#|1044 |Storey  |Mr    |NA       |3      | 60.5|     0|     0|3701   |   NA|NA    |S        |     1|

#since Mr. Storey embarked from S and his Pclass was 3 I will impute his fare price as 7.79 or 7.8

#imputing FarePP (as the Fare will be dropped later on anyway)
all$FarePP[1044] <- 7.8

# there are zero-Fares within the 1st class passengers. To avoid this possible confusion, I am replacing these values by the median FarePP’s for each Pclass.

tab3 <- all[(!is.na(all$FarePP)),]%>%
  group_by(Pclass)%>%
  summarise(MedianFarePP=median(FarePP))

all <-left_join(all,tab3, by="Pclass")

all$FarePP[which(all$FarePP==0)] <- all$MedianFarePP[which(all$FarePP==0)]


#have to use a histogram because there are not discrete values for the x axis
ggplot(all[!is.na(all$Survived),], aes(x=FarePP)) +
  geom_histogram(binwidth = 5, fill='blue') + theme_grey() +
  scale_x_continuous(breaks= seq(0, 150, by=10))


# from the plot above the data is skewed which is not desirable for certain algorithms so we can either transform the 
#variables for put them in bins. Let's put them in bins
#Note Hmisc needs to be loaded before dplyr, as the other way around errors occured due to the kernel using the Hmisc summarize function instead of the dplyr summarize function

all$FareBins <- cut2(all$FarePP,g=5)

#plot the farebins
ggplot(data = all[(!is.na(all$Survived)),], aes(x=FareBins, fill=Survived))+
  geom_bar(stat = 'count')+
  theme_grey()+
  facet_grid(.~Pclass)+
  labs(x='Bins of Fare Per Person')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#density plot of age vs. survival





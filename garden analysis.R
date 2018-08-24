library(caret)
library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(mice)
library(ggplot2)
library(reshape)
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

Tdata <-read_excel("STAllData 8-2-18.xlsx", sheet = "Mplus Dataset")

sapply(Tdata, function(x) sum(is.na(x)))
sapply(Tdata, function(x) length(which(x==0)))
sapply(Tdata, function(x) length(which(x==0))/length(x))
sapply(Tdata[,96:98], function(x) length(unique(x)))



cols1 <- c("Edu","Income","Reg")
Tdata[cols1] <- lapply(Tdata[cols1], factor)

Tdata[,85:94]<-NULL

#colaps less than high school into high school

Tdata$Edu[Tdata$Edu=='Less than High School']<-'High School Graduate (or GED equivalent)'
warnings()


summary(Tdata$Edu)


  


#education
#Tdata$Edu1 <- ifelse(Tdata$Edu == 'High School Graduate (or GED equivalent)', 1, 0)
Tdata$Edu1 <- ifelse(Tdata$Edu == 'Vocational/Technical School', 1, 
                     ifelse(is.na(Tdata$Edu),9,0))
Tdata$Edu2 <- ifelse(Tdata$Edu == 'Some College to Completion of College', 1, 
                     ifelse(is.na(Tdata$Edu),9,0))
Tdata$Edu3 <- ifelse(Tdata$Edu == 'Graduate School or Higher', 1, 
                     ifelse(is.na(Tdata$Edu),9,0))



#income
Tdata$Income1 <- ifelse(Tdata$Income == '$25,000 to $49,999', 1, 
                     ifelse(is.na(Tdata$Income),9,0))

Tdata$Income2 <- ifelse(Tdata$Income == '$50,000 to $99,999', 1, 
                        ifelse(is.na(Tdata$Income),9,0))

Tdata$Income3 <- ifelse(Tdata$Income == '$100,000 or more', 1, 
                        ifelse(is.na(Tdata$Income),9,0))

Tdata$Income4 <- ifelse(Tdata$Income == 'Decline to state', 1, 
                        ifelse(is.na(Tdata$Income),9,0))
# region
Tdata$Reg1 <- ifelse(Tdata$Reg == 'South', 1, 
                        ifelse(is.na(Tdata$Reg),9,0))
Tdata$Reg2 <- ifelse(Tdata$Reg == 'Midwest', 1, 
                     ifelse(is.na(Tdata$Reg),9,0))
Tdata$Reg3 <- ifelse(Tdata$Reg == 'West', 1, 
                     ifelse(is.na(Tdata$Reg),9,0))


 Tdata[is.na(Tdata)]<-99




Tdata$Edu2[Tdata$Edu=='High School Graduate (or GED equivalent)']<-2
Tdata$Edu3[Tdata$Edu=='Vocational/Technical School']<-3
Tdata$Edu4[Tdata$Edu=='Some College to Completion of College']<-4
Tdata$Edu5[Tdata$Edu=='Graduate School or Higher']<-5
Tdata$Edu6[is.na(Tdata$Edu)]<-6


Tdata$Income.levels[Tdata$Income=='Less than $24,999']<-1
Tdata$Income.levels[Tdata$Income=='$25,000 to $49,999']<-2
Tdata$Income.levels[Tdata$Income=='$50,000 to $99,999']<-3
Tdata$Income.levels[Tdata$Income=='$100,000 or more']<-4
Tdata$Income.levels[Tdata$Income=='Decline to state']<-5
Tdata$Income.levels[is.na(Tdata$Income)]<-6

Tdata$Reg.levels[Tdata$Reg=='Midwest']<-1
Tdata$Reg.levels[Tdata$Reg=='Northeast']<-2
Tdata$Reg.levels[Tdata$Reg=='South']<-3
Tdata$Reg.levels[Tdata$Reg=='West']<-4
Tdata$Reg.levels[is.na(Tdata$Reg)]<-5


write.csv(Tdata, file="Tdata3.csv")


results <- fastDummies::dummy_cols(Tdata[,96:98], remove_first_dummy = TRUE)

clean.results<- results[,-c('Edu_NA')]


Vdata <-cbind(Tdata,results[,4:ncol(results)])
Vdata$

install.packages("fastDummies")



Tdata$Reg.levels

Tdata$Income.levels<-
Tdata$Reg.levels <-

  all$PclassSex[all$Sex=='male' & all$Pclass =='1']<-'P1Male'



glimpse(data)

TestATT3
TestN1
TestPBC1

Gender1
Age1
Age2
Age3
Age4

Income 1,2,3,4,5

AI,Asian,BIRAC,Black,Pacisl,White
Edu1,2,3,4,5

Gov,health,Neigb,GOTher,Park,School,SeniorCt
RiskL1
#------------------------------------------------------------------------------------------------------------------
#plot of Ages
p1<-ggplot(data = Tdata, aes(x=Age1))+
         geom_bar(stat="count",na.rm = FALSE)
  
p2 <-ggplot(data = Tdata, aes(x=Age2)) + 
         geom_bar(stat="count",na.rm = FALSE) 
             
p3 <- ggplot(data = Tdata, aes(x=Age3))+ 
         geom_bar(stat="count",na.rm = FALSE)             

p4 <- ggplot(data = Tdata, aes(x=Age4))+ 
         geom_bar(stat="count",na.rm = FALSE)                

grid.arrange(p1, p2, p3, p4, ncol=2)
#---------------------------------------------------------------------------------------------------------------
#plot of Income
p5<-ggplot(data = Tdata, aes(x=Income1))+
  geom_bar(stat="count",na.rm = FALSE)

p6 <-ggplot(data = Tdata, aes(x=Income2)) + 
  geom_bar(stat="count",na.rm = FALSE) 

p7 <- ggplot(data = Tdata, aes(x=Income3))+ 
  geom_bar(stat="count",na.rm = FALSE)             

p8 <- ggplot(data = Tdata, aes(x=Income4))+ 
  geom_bar(stat="count",na.rm = FALSE)                

grid.arrange(p5, p6, p7, p8, ncol=2)
#------------------------------------------------------------------------------------------------------------------
#plot of education
p9<-ggplot(data = Tdata, aes(x=Edu1))+
  geom_bar(stat="count",na.rm = FALSE)

p10 <-ggplot(data = Tdata, aes(x=Edu2)) + 
  geom_bar(stat="count",na.rm = FALSE) 

p11 <- ggplot(data = Tdata, aes(x=Edu3))+ 
  geom_bar(stat="count",na.rm = FALSE)             

p12 <- ggplot(data = Tdata, aes(x=Edu4))+ 
  geom_bar(stat="count",na.rm = FALSE)                

grid.arrange(p9, p10, p11, p12, ncol=2)
#-------------------------------------------------------------------------------------------------------------------
#run linear models with all variables
set.seed(100)


   SData <- Tdata[c("TestAtt3","TestN1","TestPBC1","Gender1","Age1","Age2","Age3","Age4","Income1","Income2","Income3","Income4","Income5","Edu1",
                      "Edu2",
                      "Edu3",
                      "Edu4",
                      "Edu5",
                      "RiskL1")]




  cor(SData,method = )
   

model <- glm(Intent ~ 
               TestAtt3 + 
               TestN1 +
               TestPBC1 +
               Gender1 +
               Age1+
               Age2+
               Age3+
               Age4+
               Income1+
               Income2+
               Income3+
               Income4+
               Income5+
               #AI+
               #Asian+
               #Birac+
               #Black+
               #PacIsl+
               #White+
               #Gov+
               #Health+
               #Neighb+
               #GOther+
               #Park+
               #School+
               #SeniorCt+
               Edu1+
               Edu2+
               Edu3+
               Edu4+
               Edu5+
               RiskL1,family=binomial(link = 'logit'),data=Tdata )
               
options(scipen=999)

summary(model)

odds.ratio<-data.frame(exp(coefficients(model)))
vif.model<-vif(model)




coef

BStep.model<- step(model, direction = "backward", trace = 1)

summary(BStep.model)





set.seed(100)


model2 <- glm(Intent ~ 
               TestAtt3 + 
               TestN1 +
               TestPBC1 +
               #Gender1 +
               Age1+
               Age2+
               Age3+
               #Age4+
               Income1+
               Income2+
               #Income3+
               Income4+
               #Income5+
               #AI+
               #Asian+
               #Birac+
               #Black+
               #PacIsl+
               #White+
               #Gov+
               #Health+
               #Neighb+
               #GOther+
               #Park+
               #School+
               #SeniorCt+
               #Edu1+
               Edu2,family=binomial(link = 'logit'),data=Tdata )

options(scipen=999)

summary(model2)

odds.ratio2<-data.frame(exp(coefficients(model2)))


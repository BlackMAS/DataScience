
install.packages("reshape") 
install.packages("gridExtra")

#Loading libraries
#data manipulation
library(tidyverse)
library(lubridate)
library(reshape)# use transpose and melt
library(dplyr)
library(mice)# imputations

#machine learning
library(caret)
library(gbm)
library(randomForest)

#graphics
library(ggplot2)
library(gridExtra)#plot multiple graphs on one plot
library(corrplot)
library(readxl)


data1<-read_excel('Odds Ratios 11-12-18.xlsx', sheet = 1, range = cell_rows(2:26))

data1$`[95% Conf. Interval]`<-str_replace_all(data1$`[95% Conf. Interval]`, "\\[|\\]", " ")

data1$ORa<-str_replace_all(data1$ORa, "[*]", " ")


data1 <- separate(data1, col = `[95% Conf. Interval]`, into = c('LowerLimit','UpperLimit'), sep = ",")

cols3 <- c('ORa','LowerLimit','UpperLimit')
data1[cols3] <- lapply(data1[cols3], as.numeric)



data1$cond.order <-factor(data1$Condition, levels=c("Attitude*","Subjective Norms*","Perceived Behavioral Control*","Garden Context","Income",	"Region",	"Age",	"Race",	"Education*","Conventional *", "USDA Certified Organic",	"Garden Site History"))

data1$Condition
data1$cond.order



p<- ggplot(data=data1, aes(x=Behavior,y=ORa))+
  geom_point(aes(col=Behavior))+
  geom_hline(aes(fill=Behavior), yintercept = 1, linetype=2)+
  xlab('Variable')+ylab("aOR (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=LowerLimit, ymax=UpperLimit,col=Behavior),width=0.5, cex=1)+
  theme_minimal()+
  facet_wrap(~cond.order, strip.position = "left",nrow = 24, scales = "free_y")+
  theme(plot.title = element_text(size = 16,face='bold'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title = element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust =1,angle=180,face="bold"))+
  coord_flip()

p

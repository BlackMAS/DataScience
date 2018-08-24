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

#read in data files
train <- read.csv('train.csv')
test <- read.csv('test.csv')
glimpse(train)
glimpse(test)

str(train)
summary(train)

summary(test)

summary(t(train))

# look for missing values (0's and NAs) and unique values
sapply(train, function(x) unique(x))
sapply(train, function(x) sum(is.na(x)))
sapply(train, function(x) length(which(x==0)))
round(sapply(train, function(x) length(which(x==0))/length(x)), digits = 2)

#identify different columns
setdiff(colnames(train),colnames(test))


#---------------------------------------------------------------------------------------------------------------------------
#transpose the data, look at descriptive statistics, and graph the statistics


# Transpose the data with "t" function
train.T <- as.data.frame(t(train[1:nrow(train),]))
train.T<-train.T[-(1:2),]

#get rid of scientific notation
options(scipen=999)


# Convert columns from factors to numeric because transposing turns data into factors
cols <- colnames(train.T)
train.T[,cols] <-lapply(train.T, function(x) as.numeric(as.character(x)))

#replace 0's with NA
train.T[train.T =="0"]<- NA

train.M <-melt(train,id.vars = "ID")

train.MM <- subset(train.M,value !=0 & variable !="target")


head(train.M)

mode(train.M$value)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

result <- getmode(train.MM$value)
print(result)

sum(train.MM$value==2000000)

glimpse(train.M)

basicStats(train.M$value)
#look at descriptive statistic of train.T
z<-basicStats(train.T)

#lookt at just the mean values
min.Train <- melt(z[3,])
max.Train <- melt(z[4,])

mean.Train<- melt(z[7,])
median.Train <- melt(z[8,])

#find average of averages
newdata <-mean.Train[which(mean.Train$value <50000000),]
avg.mean.train <-mean(newdata$value)
sum(mean.Train$value >= 25000000 & mean.Train$value <=50000000 )   /nrow(mean.Train)

sum(mean.Train$value >= 10000000 & mean.Train$value <=50000000 )   /nrow(mean.Train)

sum(mean.Train$value < 6000000)   /nrow(mean.Train) #50% below 6M, 55% below 7M, 92% below 25M

# potential categories < 6M = Standard client, 6M> & <25M = Super client, >25M = Mega client,
# percentage of transactions in the above categories

# avg.mean.train = 8,528,475


sum.Train <- melt(z[9,])

stdev.Train <- melt(z[14,])
glimpse(mean.Train$value)

#plotting the descriptive statistics
ggplot(data=min.Train)+ aes(x=variable, y=value)+ geom_col() +  coord_cartesian(ylim = c(0, 100000000)) +ggtitle("Min Trans") 
ggplot(data=max.Train)+ aes(x=variable, y=value)+ geom_col()+  coord_cartesian(ylim = c(0, 150000000))

ggplot(data=mean.Train)+ aes(x=variable, y=value)+ geom_col()+  coord_cartesian(ylim = c(0, 12000000))+ggtitle("Mean Trans")
ggplot(data=median.Train)+ aes(x=variable, y=value)+ geom_col()+  coord_cartesian(ylim = c(0, 150000000))+ggtitle("Median Trans")
ggplot(data=sum.Train)+ aes(x=variable, y=value)+ geom_col()+  coord_cartesian(ylim = c(0, 150000000))+ggtitle("Sum of Trans")

ggplot(data=stdev.Train)+ aes(x=variable, y=value)+ geom_col()+  coord_cartesian(ylim = c(0, 150000000))+ggtitle("Stdev of Trans")



boxplot(clean.train$X48df886f9)
boxplot(z[,3:ncol(clean.train)])




ggplot(data = clean.train, aes(x = "", y = X48df886f9)) + 
  geom_boxplot() + 
  theme(axis.title.x = element_blank())

#most columns have mostly 0's
#--------------------------------------------------------------------------------------------------------------------------
# plotting the original data to see trends

ggplot(train) + aes(target) + 
  geom_histogram()

#
b<-t(train[4031,])



# grab the first 20 rows

train.sample <- train[1:3,]
train.sample <- train.sample[,-c(2)]

m.train.sample <-melt(train.sample,id.vars = "ID")

m.train.sample2 <- filter(m.train.sample,ID=="000fbd867")

m<-group_by(m.train.sample,variable)

ggplot(m.train.sample2, aes(x=variable, y=value))+
  geom_col() 

#--------------------------------------------------------------------------------------------------------------------------

#look at descriptive statistics of data with the 0 replaced by NAs

clean.train <- train

a <- round(sapply(clean.train, function(x) length(which(x==0))/length(x)), digits = 2) 

clean.train <- clean.train[,!a==1]

clean.train <-  train[, colSums(train != 0) > 0]


sapply(clean.train, function(x) unique(x))


clean.train[ clean.train == "0" ] <- NA


a <- basicStats(clean.train[,-1])

#--------------------------------------------------------------------------------------------------------------------------
#add a column to show the average transaction value for each record
clean.train$mean.trans = rowMeans(clean.train[,3:ncol(clean.train)], na.rm = TRUE)
clean.train$mean.trans = ifelse(is.na(clean.train$mean.trans), 0, clean.train$mean.trans)

#add column to show the median transaction value for each record
clean.train$median.trans<-apply(clean.train[,-(1:2)],1, median, na.rm = TRUE)
clean.train$median.trans = ifelse(is.na(clean.train$median.trans), 0, clean.train$median.trans)

#potential new columns to add to data
clean.train$num.missing<-rowSums(clean.train[,3:ncol(clean.train)]==0)
clean.train$num.trans <-rowSums (clean.train[,3:ncol(clean.train)]!=0)

clean.train$stdevs<-apply(clean.train[,-(1:2)],1, sd, na.rm = TRUE)


clean.train$max.trans<-apply(clean.train[,-(1:2)],1, max, na.rm = TRUE)


clean.train$kurtosis<-apply(clean.train[,-(1:2)],1, kurtosis, na.rm = TRUE)
clean.train$Skewness<-apply(clean.train[,-(1:2)],1, skewness, na.rm = TRUE)


clean.train$percent.micro.client <-round(rowSums(clean.train [,3:ncol(clean.train)] < 50000,na.rm = TRUE)/rowSums(!is.na(clean.train[,3:ncol(clean.train)])), digits = 2)

clean.train$percent.small.client <-round(rowSums(clean.train [,3:ncol(clean.train)] >=50000 & clean.train [,3:ncol(clean.train)]<=1000000,na.rm = TRUE)/rowSums(!is.na(clean.train[,3:ncol(clean.train)])), digits = 2)

clean.train$percent.std.client <-round(rowSums(clean.train [,3:ncol(clean.train)] >1000000 & clean.train [,3:ncol(clean.train)]<6000000,na.rm = TRUE)/rowSums(!is.na(clean.train[,3:ncol(clean.train)])), digits = 2)


clean.train$percent.super.client<-round(rowSums(clean.train[,3:ncol(clean.train)] >= 6000000 & clean.train[,3:ncol(clean.train)] <= 25000000,na.rm = TRUE)/rowSums(!is.na(clean.train[,3:ncol(clean.train)])),digits = 2)

clean.train$percent.mega.client <-round(rowSums(clean.train[,3:ncol(clean.train)] > 25000000,na.rm = TRUE)/rowSums(!is.na(clean.train[,3:ncol(clean.train)])), digits = 2)

colMaxs(clean.train[,15])
colMins(clean.train[,15])


# potential categories < 6M = Standard client, 6M> & <25M = Super client, >25M = Mega client,
# percentage of transactions in the above categories



glimpse(clean.train$num.trans)
#---------------------------------------------------------------------------------------------------------------------------
# Get baseline linear regression results with no data transformation
train$ln_target <-log(train$target)
mtrain <- train[-1,]

glimpse(train$ID)

lg.model <- lm(formula = ln_target ~. -ID,-target, data = train )

s1<-summary(lg.model)
train$ln.results<-predict(lg.model,newdata = train)
library(car)
RMSE(pred=train$ln.results,obs = train$ln_target)


#Residual standard error: 1.071 on 11 degrees of freedom
#Multiple R-squared:  0.9991,	Adjusted R-squared:  0.6256 
#F-statistic: 2.675 on 4447 and 11 DF,  p-value: 0.03355

# see if the ID's repeat in test and train data set
intersect(train$ID,test$ID)
glimpse(test$ID)

#--------------------------------------------------------------------------------------------------------------------------
#linear regression on clean data

clean.train$ln_target <- log(clean.train$target)

clean.train[is.na(clean.train)]<- 0
clean.train$target <- NULL
clean.train$ID <- NULL
clean.train$ln.results<-NULL



glimpse(clean.train)

lg.clean.model <- lm(formula = ln_target ~., data = clean.train )


s1<-summary(lg.clean.model)

clean.train$ln.results<-predict(lg.clean.model,newdata = clean.train)
library(car)
Ln.RMSE <-RMSE(pred=clean.train$ln.results,obs = clean.train$ln_target)


# check to see if there are duplicate IDs

n_occur <- data.frame(table(clean.train$ID))
n_occur[n_occur$Freq > 1,]
# There are no duplicate IDs

#get rid of scientific notation
options(scipen=999)


#--------------------------------------------------------------------------------------
#test data
glimpse(cclean.train$X88458cb21)

sapply(test, function(x) unique(x))
sapply(test, function(x) sum(is.na(x)))
sapply(test, function(x) length(which(x==0)))
round(sapply(test, function(x) length(which(x==0))/length(x)), digits = 2)
#----------------------------------------------------------------------------------------
# clean the test data
clean.test <-test
clean.test[ clean.test == "0" ] <- NA


#add a column to show the average transaction value for each record
clean.test$mean.trans = rowMeans(clean.test[,3:ncol(clean.test)], na.rm = TRUE)
clean.test$mean.trans = ifelse(is.na(clean.test$mean.trans), 0, clean.test$mean.trans)

#add column to show the median transaction value for each record
clean.test$median.trans<-apply(clean.test[,-(1:2)],1, median, na.rm = TRUE)
clean.test$median.trans = ifelse(is.na(clean.test$median.trans), 0, clean.test$median.trans)

#potential new columns to add to data
clean.test$num.missing<-rowSums(is.na(clean.test))
clean.test$num.trans <-rowSums(!is.na(clean.test[,3:ncol(clean.test)]))

clean.test$stdevs<-apply(clean.test[,-(1:2)],1, sd, na.rm = TRUE)


clean.test$max.trans<-apply(clean.test[,-(1:2)],1, max, na.rm = TRUE)


clean.test$kurtosis<-apply(clean.test[,-(1:2)],1, kurtosis, na.rm = TRUE)
clean.test$Skewness<-apply(clean.test[,-(1:2)],1, skewness, na.rm = TRUE)


clean.test$percent.micro.client <-round(rowSums(clean.test [,3:ncol(clean.test)] < 50000,na.rm = TRUE)/rowSums(!is.na(clean.test[,3:ncol(clean.test)])), digits = 2)

clean.test$percent.small.client <-round(rowSums(clean.test [,3:ncol(clean.test)] >=50000 & clean.test [,3:ncol(clean.test)]<=1000000,na.rm = TRUE)/rowSums(!is.na(clean.test[,3:ncol(clean.test)])), digits = 2)

clean.test$percent.std.client <-round(rowSums(clean.test [,3:ncol(clean.test)] >1000000 & clean.test [,3:ncol(clean.test)]<6000000,na.rm = TRUE)/rowSums(!is.na(clean.test[,3:ncol(clean.test)])), digits = 2)


clean.test$percent.super.client<-round(rowSums(clean.test[,3:ncol(clean.test)] >= 6000000 & clean.test[,3:ncol(clean.test)] <= 25000000,na.rm = TRUE)/rowSums(!is.na(clean.test[,3:ncol(clean.test)])),digits = 2)

clean.test$percent.mega.client <-round(rowSums(clean.test[,3:ncol(clean.test)] > 25000000,na.rm = TRUE)/rowSums(!is.na(clean.test[,3:ncol(clean.test)])), digits = 2)

clean.test[is.na(clean.test)]<- 0

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Linear regression with test data and min correlation data used in model


corr.matrix <- cor(clean.train[,3:ncol(clean.train)],method="pearson")
high_cor <- findCorrelation(corr.matrix, cutoff = 0.2,names = FALSE)

min_cor <- clean.train[,-c(high_cor)]
min_cor$target <- NULL
min_cor$ID <- NULL
min_cor$ln.results<-NULL

lg.clean.model2 <- lm(formula = ln_target ~.,data = min_cor )


s1<-summary(lg.clean.model2)

test$ln.results<-predict(lg.clean.model2,newdata = clean.test)
test$target <- round(exp(test$ln.results),digits = 2)

summary(test$target)

library(car)
Ln.RMSE <-RMSE(pred=clean.train$ln.results,obs = clean.train$ln_target)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------










train$ln_target <-log(train$target)

#plotting the data

#num_data <- complete.train %>%
#dplyr::select(-c(ID,target))

#Collapsing the mulitiple columns of data into one column (going from wide to long)
d <- melt(complete.train[,c(3:20)])

#getting rid of rows with 0 in the value column
d2 <- subset(d,value !=0)

#plotting multiple histograms on one image and scaling the y-axis to zoom in
ggplot(d2,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram() + 
  coord_cartesian(ylim = c(0, 100))




ggplot(clean.train) + aes(clean.train$X48df886f9) + 
  geom_histogram()

ggplot(train) + aes(train$ln_target) + 
  geom_histogram()


#correlations
num_data <- clean.train %>%
  dplyr::select(-c(ID,target))

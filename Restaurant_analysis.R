install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(caret)
library(dplyr)

#read in data files test and train
train <- read.csv('Train.csv')
test <- read.csv('Test.csv')
glimpse(train)
glimpse(test)


summary(train)
summary(test)

# look for missing values (0's and NAs) and unique values
sapply(train, function(x) unique(x))
sapply(train, function(x) sum(is.na(x)))
sapply(train, function(x) length(which(x==0)))
sapply(train, function(x) length(which(x==0))/length(x))

sapply(train.nulls, function(x) sum(is.na(x)))



sapply(test, function(x) unique(x))
sapply(test, function(x) sum(is.na(x)))
sapply(test, function(x) length(which(x==0)))
sapply(test, function(x) length(which(x==0))/length(x))

train.nulls <- train
train.nulls[ train.nulls == "0" ] <- NA

#---------------------------------------------------------------------------------------------------
#look for missing data pattern
library(mice)
y<-md.pattern(train.nulls)

#imputing missing data with the mice function using the random forest method
methods(mice)
tempData <- mice(train.nulls,m=5,maxit=2,meth='cart',where = is.na(train.nulls),seed=500)
summary(tempData)

#view imputed data for column P14
tempData$imp$P14

#Use the imputed data from a particular imputation data set
train <- complete(tempData,4)

#see how well the imputation performs vs the real data
xyplot(tempData,P14 ~ P15+P16+P17,pch=18,cex=1)
densityplot(tempData)

#---------------------------------------------------------------------------------------------------
#finding mismatches between the data sets for categorical variables

setdiff(levels(test$Type), levels(train$Type))
setdiff(levels(test$City), levels(train$City))


#city and type do not have the same levels for the train and test dataset

#change certain columns to factors
cols1 <- c("City","City.Group","Type")
train[cols] <- lapply(train[cols1], factor)
train$Open.Date<-mdy(train$Open.Date)


cols2 <- c("City","City.Group","Type")
test[cols2] <- lapply(test[cols2], factor)
test$Open.Date<-mdy(test$Open.Date)

#split the date column into three columns for day month and year
train <-train %>%
  separate(Open.Date,c("Year","Month","Day"), sep="-")

cols3 <- c("Year","Month","Day")
train[cols3] <- lapply(train[cols3], factor)


test <-test %>%
  separate(Open.Date,c("Year","Month","Day"), sep="-")

cols4 <- c("Year","Month","Day")
test[cols4] <- lapply(test[cols4], factor)


#create new column for natural log of revenue
train$Ln_revenue <- log(train$revenue)

#----------------------------------------------------------------------------------------------------
#plot histogram of revenue and natural log of revenue
library(reshape2)

ggplot(train) + aes(train$revenue,fill=train$Type) + 
  geom_histogram()

ggplot(test) + aes(test$pred.revenue) + 
  geom_histogram()


ggplot(train) + aes(x=train$Year,y=train$revenue) + 
  geom_point() 

ggplot(train) + aes(x=train$Month,y=train$revenue) + 
  geom_line( aes(colour = Year) )  
  

ggplot(train) + aes(x=train$City,y=train$revenue) + 
  geom_point()

ggplot(test) + aes(x=test$City,y=test$pred.revenue) + 
  geom_point()


ggplot(train) + aes(x=train$Type,y=train$revenue) + 
  geom_point()

ggplot(train) + aes(x=train$City.Group,y=train$revenue) + 
  geom_point()

ggplot(train) + aes(x=train$P2,y=train$revenue) + 
  geom_point()


ggplot(train) + aes(train$Ln_revenue) + 
  geom_histogram()


#------------------------------------------------------------------------------------------------------------
#plots of barplot of city and city group
ggplot(train) + aes(train$P5) + 
  geom_histogram()

# barplot of City groups
ggplot(train) + aes(x=train$City.Group) + 
  geom_bar()

ggplot(test) + aes(x=test$City.Group) + 
  geom_bar()



# barplot of Cities
count(train, City) %>% 
  ggplot(aes(x = reorder(City, -n), y =n )) +
  geom_col()

# barplot of Cities
count(test, City) %>% 
  ggplot(aes(x = reorder(City, -n), y =n )) +
  geom_col()


#-------------------------------------------------------------------------------------------------------------------
#create dataframe of numerica data

num_data <- train %>%
  dplyr::select(-c(Id, Year, Month, Day, City, City.Group,Type))

#create dataframe of everthing but date and ID
filtered_data <- train %>%
  dplyr::select(-c(Id,City.Group))


#-------------------------------------------------------------------------------------------------------------------
#look at correlations
corr.matrix <- cor(num_data,method="pearson")
  
high_cor <- findCorrelation(corr.matrix, cutoff = 0.5,names = FALSE)
  
#high_cor <- high_cor[high_cor != 186] don't know what this does 
high_cor  

min_cor <- num_data[,-c(high_cor)]

model <- lm(Ln_revenue ~. , data=min_cor)
summary(model)






#-------------------------------------------------------------------------------------------------------------------
#random forest method

set.seed(seed = 100)

library(randomForest)
bestmtry <-tuneRF(min_cor,min_cor$Ln_revenue,stepFactor = 1.2, improve = 0.01,trace = T, plot=T)

RFmodel1 <- randomForest(Ln_revenue~., data = min_cor,metric ="RMSE")
RFmodel1

plot(RFmodel1)

min_cor$revenue <-NULL
min_cor$RFresults <- NULL
min_cor$RFrevenue <-NULL

min_cor$RFresults<- predict(RFmodel1,newdata = min_cor)
min_cor$revenue <- exp(min_cor$Ln_revenue)
min_cor$RFrevenue <-exp(min_cor$RFresults)
RF_RMSE <-RMSE(pred = min_cor$RFrevenue,obs = min_cor$revenue)



test$RFresults<- predict(RFmodel1,newdata = test)

test$pred.revenue <- NULL
test$RFresults<- NULL

test$pred.revenue<-exp(test$RFresults)
write.csv("test.csv")

#-------------------------------------------------------------------------------------------------------------------
#Gradient booted method


library(gbm)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=10)
gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 6, 9, 10),
                        n.trees = (3:7)*10, 
                        shrinkage = seq(0.05),
                        n.minobsinnode = 10)

gbm_data$revenue <-NULL
gbm_data$gbm_results <- NULL
gbm_data$GBMrevenue <-NULL


# train the GBM model
set.seed(100)

gbm_data <- min_cor %>%
  dplyr::select(-c(revenue, RFresults, RFrevenue))

modelGbm <- train(Ln_revenue~., data=gbm_data, method="gbm", trControl=control,tuneGrid=gbmGrid, verbose=FALSE, metric='RMSE')
modelGbm

gbm_data$gbm_results<- predict(modelGbm,newdata = gbm_data)

gbm_data$revenue <- exp(gbm_data$Ln_revenue)
gbm_data$GBMrevenue <-exp(gbm_data$gbm_results)
GBM_RMSE<-RMSE(pred = gbm_data$GBMrevenue,obs = gbm_data$revenue)

GBM_RMSE
#-------------------------------------------------------------------------------------------------------------------------------------------------
#Cart model

cart_data <- min_cor %>%
  dplyr::select(-c(revenue, RFresults, RFrevenue))

set.seed(seed = 100)

cartControl <- trainControl(method="repeatedcv", number=10, repeats=10)


modelCart <- train(Ln_revenue~., data=cart_data, method = 'rpart',trControl=cartControl, metric = 'RMSE')

modelCart

cart_data$cart_results<- predict(modelCart,newdata = cart_data)

cart_data$revenue <- exp(cart_data$Ln_revenue)
cart_data$CARTrevenue <-exp(cart_data$cart_results)
CART_RMSE<-RMSE(pred = cart_data$CARTrevenue,obs = cart_data$revenue)

cart_data$revenue <-NULL
cart_data$cart_results <- NULL
cart_data$CARTrevenue <-NULL



#-------------------------------------------------------------------------------------------------------------------
#Correlation plots
corr.matrix %>%
  write.csv("corr.matrix.csv")

install.packages("ggcorrplot")
library(ggcorrplot)

p <-ggcorrplot(corr.matrix, title = "Correlation matrix for test data", lab=TRUE)

install.packages("corrplot")
library(corrplot)
corrplot(corr.matrix,type="upper", method = "number")

corrplot(corr.matrix, type="upper", order="hclust")

corrplot(corr.matrix2, type="upper", order="hclust")


install.packages("usdm")
library(usdm)

#-------------------------------------------------------------------------------------------------------------------
#run linear models with all variables

model <- lm(Ln_revenue ~. , data=filtered_data)
summary(model)



#test the normalicy of the residuals
a <-residuals(model)  
a<- as.numeric(a)
shapiro.test(a)

library(car)

vif.results1<-vif(model)

# run linear model without multicolinear variables on the Log of revenue since regular revenue did not pass the Shapiro Wilks normality test
model.2 <- lm(Ln_revenue ~
                #Year +
                Month + 
                #Day +
                City +
                Type +
                P1 +
                P2 +
                P3 +
                #P4 +
                #P5 +
                #P6 +
                P7 +
                P8 +
                #P9 +
                #P10 +
                #P11 +
                P12 +
                P13 +
                #P14 +
                #P15 +
                P16 +
                P17 +
                #P18 +
                P19 +
                P20 +
                P21 +
                P22 +
                #P23 +
                P24 +
                #P25 +
                P26 +
                P27 +
                P28 +
                P29 +
                P30 +
                #P31 +
                P32 +
                P33 +
                P34 +
                #P35 +
                P36 +
                P37, data=filtered_data)
summary(model.2)


#test the normalicy of the residuals
b <-residuals(model.2)  
b<- as.numeric(b)
shapiro.test(b)
vif(model.2)

library(car)

#show the predictions in the training data set
filtered_data$predicted <- predict(model.2,newdata=filtered_data,type='response', se.fit=FALSE)

#convert log of revenue to revenue
filtered_data$rev.predicted <- exp(filtered_data$predicted)                    

#calculate the Root Mean Square Average
RMSE(pred = filtered_data$rev.predicted,obs = filtered_data$revenue)





shapiro.test(filtered_data$Ln_revenue)
#----------------------------------------------------------------------------------------------------------------


# remove these becaue of multicolinearity P2

Month 3.180927e+07 11        2.192932
Day   1.127219e+15 29        1.817680
City  2.293784e+18 33        1.897537
Type  3.696290e+01  2        2.465707
P1    7.551446e+01  1        8.689906
P2    3.986575e+01  1        6.313933
P3    3.470540e+01  1        5.891129
P4    2.201308e+01  1        4.691810
P5    2.640254e+01  1        5.138340
P6    2.592415e+01  1        5.091576
P7    5.997888e+01  1        7.744603
P8    1.079691e+02  1       10.390818
P9    4.901861e+02  1       22.140147
P10   1.681469e+03  1       41.005718
P11   6.009370e+01  1        7.752013
P12   1.413483e+02  1       11.888997
P13   6.008861e+02  1       24.512979
P14   1.782279e+01  1        4.221705
P15   1.830852e+01  1        4.278846
P16   3.669454e+01  1        6.057602
P17   1.481324e+01  1        3.848797
P18   2.738765e+01  1        5.233321
P19   5.108312e+01  1        7.147245
P20   5.495681e+01  1        7.413286
P21   4.633520e+01  1        6.806996
P22   1.010781e+01  1        3.179278
P23   2.361159e+01  1        4.859176
P24   8.217144e+00  1        2.866556
P25   1.845954e+01  1        4.296456
P26   1.470265e+01  1        3.834403
P27   8.530943e+00  1        2.920778
P28   4.202562e+01  1        6.482717
P29   2.730407e+01  1        5.225330
P30   3.366945e+01  1        5.802539
P31   1.539646e+01  1        3.923832
P32   8.729220e+01  1        9.343029
P33   1.148909e+01  1        3.389556
P34   6.199513e+01  1        7.873699
P35   2.419168e+01  1        4.918504
P36   4.782521e+01  1        6.915577
P37   7.918665e+00  1        2.814012





#-----------------------------------------------------------------------
# Stepwise regression model
library(MASS)
step.model <- stepAIC(model.2, direction = "both", 
                      trace = FALSE)
summary(step.model)

#show the predictions in the training data set
step.predicted <- predict(step.model,newdata=filtered_data,type='response', se.fit=FALSE)

#convert log of revenue to revenue
rev.step.predicted <- exp(step.predicted)                    

#calculate the Root Mean Square Average
RMSE(pred = rev.step.predicted,obs = filtered_data$revenue)

#Use machine learning
set.seed(500)
library(nnet)
nn1 <- train(revenue ~
               #Year +
               Month + 
               #Day +
               City +
               Type +
               P1 +
               P2 +
               P3 +
               P4 +
               P5 +
               P6 +
               P7 +
               P8 +
               P9 +
               P10 +
               P11 +
               P12 +
               P13 +
               P14 +
               #P15 +
               #P16 +
               P17 +
               #P18 +
               P19 +
               P20 +
               P21 +
               P22 +
               P23 +
               #P24 +
               #P25 +
               P26 +
               #P27 +
               P28 +
               P29 +
               P30 +
               P31 +
               #P32 +
               P33 +
               P34 +
               P35 +
               P36 +
               P37, data=filtered_data, method='rf', ntree=10000)

summary(nn1)

nnet.predicted <- predict(nn1,newdata=filtered_data, type = "raw")

results<- as.data.frame(t(nnet.predicted))
results<-t(results)



#calculate the Root Mean Square Average
RMSE(pred = results,obs = filtered_data$revenue)













detach("package:usdm", unload=TRUE)
library(car)

vif(step.model)

alias( lm(revenue ~. , data=filtered_data) )
#----------------------------------------------------------------
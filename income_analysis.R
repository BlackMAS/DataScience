install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)
library(caret)

#------------------------------------------------------------------------------------------------------------------------------
#read and format data

#read in data file
adult.data <- read_csv('adult.data.csv')

#add column names to data file
colnames(adult.data) <- c("age","workingclass","fnlwgt", "education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours.per.week","native.country","earnings")

#see number of missing values
sapply(adult.data, function(x) sum(is.na(x)))

#quikcly check for how many different values for each feature
sapply(adult.data, function(x) length(unique(x)))

summary(adult.data)

glimpse(adult.data)

#change certain columns to factors
cols <- c("workingclass","education","marital.status","occupation","relationship","race","sex","native.country","earnings")
adult.data[cols] <- lapply(adult.data[cols], factor)

#replace "?" with NA in the data set
adult.data[ adult.data == "?" ] <- NA

#add new level called "unknown"
levels(adult.data$workingclass)[10] <- 'unknown'

#replace all NAs with "unknown"
adult.data$workingclass[which(is.na(adult.data$workingclass))] <-'unknown'

#drop the "?" level
adult.data$workingclass<-droplevels(adult.data$workingclass)
summary(adult.data$earnings)

glimpse(adult.data$earnings)
#----------------------------------------------------------------------------------------------------------------------------------
#visualize the data
summary(adult.data)

ggplot(adult.data) + aes(x=as.numeric(age), group=earnings, fill=earnings) + 
  geom_histogram(binwidth=1, color='black')

ggplot(adult.data) + aes(x=as.numeric(age), group=sex, fill=sex) + 
  geom_histogram(binwidth=1, color='black')


#-----------------------------------------------------------------------------------------------------------------
#create better groupings of the data
adult.data$workingclass <- as.factor(adult.data$workingclass)

# combine into Self-Employed job
adult.data$workingclass <- gsub('^Self-emp-inc', 'Self-Employed', adult.data$workingclass)
adult.data$workingclass <- gsub('^Self-emp-not-inc', 'Self-Employed', adult.data$workingclass)

adult.data$workingclass <- gsub('^Never-worked', 'Other', adult.data$workingclass)
adult.data$workingclass <- gsub('^Without-pay', 'Other', adult.data$workingclass)
adult.data$workingclass <- gsub('^Other', 'Other/Unknown', adult.data$workingclass)
adult.data$workingclass <- gsub('^unknown', 'Other/Unknown', adult.data$workingclass)

adult.data$workingclass <- as.factor(adult.data$workingclass)
summary(adult.data$workingclass)
#--------------------------------------------------------------------------------------------------------

# barplot of job type by earnings group
# get the counts by industry and earnings group
# THE FOLLOWING CODE MUST BE ORDER IN THE SAME WAY AS THE LEVELS ARE LISTED WHEN YOUR RUN "levels(adult.data$workingclass)"
count <- table(adult.data[adult.data$workingclass == 'Federal-gov',]$earnings)["<=50K"]
count <- c(count, table(adult.data[adult.data$workingclass == 'Federal-gov',]$earnings)[">50K"])

count <- c(count, table(adult.data[adult.data$workingclass == 'Local-gov',]$earnings)["<=50K"])
count <- c(count, table(adult.data[adult.data$workingclass == 'Local-gov',]$earnings)[">50K"])

count <- c(count, table(adult.data[adult.data$workingclass == 'Other/Unknown',]$earnings)["<=50K"])
count <- c(count, table(adult.data[adult.data$workingclass == 'Other/Unknown',]$earnings)[">50K"])

count <- c(count, table(adult.data[adult.data$workingclass == 'Private',]$earnings)["<=50K"])
count <- c(count, table(adult.data[adult.data$workingclass == 'Private',]$earnings)[">50K"])

count <- c(count, table(adult.data[adult.data$workingclass == 'Self-Employed',]$earnings)["<=50K"])
count <- c(count, table(adult.data[adult.data$workingclass == 'Self-Employed',]$earnings)[">50K"])

count <- c(count, table(adult.data[adult.data$workingclass == 'State-gov',]$earnings)["<=50K"])
count <- c(count, table(adult.data[adult.data$workingclass == 'State-gov',]$earnings)[">50K"])
count <- as.numeric(count)


# create a dataframe
levels(adult.data$workingclass)
industry <- rep(levels(adult.data$workingclass), each = 2)
earnings <- rep(c('<=50K', '>50K'), 6)
df <- data.frame(industry, earnings, count)
df

# calculate the percentages
library(plyr)
df <- ddply(df, .(industry), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df <- ddply(df, .(industry), transform, pos = (cumsum(count) - 0.5 * count))
df$label <- paste0(sprintf("%.0f", df$percent), "%")

# bar plot of counts by industry with in group proportions 
ggplot(df, aes(x = industry, y = count)) +
  geom_bar(aes(fill=earnings),stat = "identity",position = position_stack(reverse = TRUE)) +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income by Industry') 
#-----------------------------------------------------------------------------------------------------------------------------
# Perform the same analysis for income by education
head(adult.data)
summary(adult.data$education.num)
glimpse(adult.data$education.num)

# create a dataframe for earnings and education

earn.vs.edu <- data.frame(table(adult.data$earnings, adult.data$education.num))
names(earn.vs.edu)<- c('earnings','education.num','count')

#calculate the percentages
earn.vs.edu <- ddply(earn.vs.edu, .(education.num), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
earn.vs.edu <- ddply(earn.vs.edu, .(education.num), transform, pos = (cumsum(count) - 0.5 * count))
earn.vs.edu$label <- paste0(sprintf("%.0f", earn.vs.edu$percent), "%")

#plot
ggplot(earn.vs.edu, aes(x = education.num, y = count)) +
  geom_bar(aes(fill=earnings),stat = "identity",position = position_stack(reverse = TRUE)) +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income by Education')
  

#-----------------------------------------------------------------------------------------------------------------------------
# Perform the same analysis for income by occupation

summary(adult.data$occupation)

levels(adult.data$occupation)



#replace all NAs with "unknown"

adult.data$occupation.type <- adult.data$occupation

#add new level called "unknown"
levels(adult.data$occupation)[10] <- 'Unknown'

adult.data$occupation[which(is.na(adult.data$occupation))] <-'Unknown'

adult.data$occupation <- gsub('Adm-clerical', 'White-Collar', adult.data$occupation)
adult.data$occupation  <- gsub('Craft-repair', 'Blue-Collar', adult.data$occupation)
adult.data$occupation  <- gsub('Exec-managerial', 'White-Collar', adult.data$occupation)
adult.data$occupation  <- gsub('Farming-fishing', 'Blue-Collar', adult.data$occupation)
adult.data$occupation  <- gsub('Handlers-cleaners', 'Blue-Collar', adult.data$occupation)
adult.data$occupation  <- gsub('Machine-op-inspct', 'Blue-Collar', adult.data$occupation)
adult.data$occupation  <- gsub('Other-service', 'Service', adult.data$occupation)
adult.data$occupation  <- gsub('Priv-house-serv', 'Service', adult.data$occupation)
adult.data$occupation  <- gsub('Prof-specialty', 'Professional', adult.data$occupation)
adult.data$occupation  <- gsub('Protective-serv', 'Service', adult.data$occupation)
adult.data$occupation  <- gsub('Tech-support', 'Service', adult.data$occupation)
adult.data$occupation  <- gsub('Transport-moving', 'Blue-Collar', adult.data$occupation)
adult.data$occupation  <- gsub('Unknown', 'Other/Unknown', adult.data$occupation)
adult.data$occupation  <- gsub('Armed-Forces', 'Other/Unknown', adult.data$occupation)
adult.data$occupation  <- as.factor(adult.data$occupation )
summary(adult.data$occupation )

#--------------------------------------------------------------------------------------------------------

# barplot of job type by earnings group
# get the counts by industry and earnings group
# THE FOLLOWING CODE MUST BE ORDER IN THE SAME WAY AS THE LEVELS ARE LISTED WHEN YOUR RUN "levels(adult.data$workingclass)"



count <- table(adult.data[adult.data$occupation == 'Blue-Collar',]$earnings)["<=50K"]
count <- c(count, table(adult.data[adult.data$occupation == 'Blue-Collar',]$earnings)[">50K"])

count <- c(count, table(adult.data[adult.data$occupation == 'Other/Unknown',]$earnings)["<=50K"])
count <- c(count, table(adult.data[adult.data$occupation == 'Other/Unknown',]$earnings)[">50K"])

count <- c(count, table(adult.data[adult.data$occupation == 'Professional',]$earnings)["<=50K"])
count <- c(count, table(adult.data[adult.data$occupation == 'Professional',]$earnings)[">50K"])

count <- c(count, table(adult.data[adult.data$occupation == 'Sales',]$earnings)["<=50K"])
count <- c(count, table(adult.data[adult.data$occupation == 'Sales',]$earnings)[">50K"])

count <- c(count, table(adult.data[adult.data$occupation == 'Service',]$earnings)["<=50K"])
count <- c(count, table(adult.data[adult.data$occupation == 'Service',]$earnings)[">50K"])

count <- c(count, table(adult.data[adult.data$occupation == 'White-Collar',]$earnings)["<=50K"])
count <- c(count, table(adult.data[adult.data$occupation == 'White-Collar',]$earnings)[">50K"])

count <- as.numeric(count)

# create a dataframe
levels(adult.data$occupation)
occupation <- rep(levels(adult.data$occupation), each = 2)
earnings <- rep(c('<=50K', '>50K'), 6)
dff <- data.frame(occupation, earnings, count)
dff

# alternate way to create a dataframe
df2 <- data.frame(table(adult.data$earnings, adult.data$occupation))
names(df2) <- c('earnings', 'occupation', 'count')
df2
#------------------------------------------------------------------------------------------------------------------------
#plot the data

# calculate the percentages
df2 <- ddply(df2, .(occupation), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df2 <- ddply(df2, .(occupation), transform, pos = (cumsum(count) - 0.5 * count))
df2$label <- paste0(sprintf("%.0f", df2$percent), "%")

#plot
ggplot(df2, aes(x = occupation, y = count)) +
  geom_bar(aes(fill=earnings),stat = "identity",position = position_stack(reverse = TRUE)) +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income by Occupation')

#---------------------------------------------------------------------------------------------------------------------------
#Perform same analysis on marital status
summary(adult.data$marital.status)

adult.data$marital.status <- gsub('Married-AF-spouse', 'Married', adult.data$marital.status)
adult.data$marital.status <- gsub('Married-civ-spouse', 'Married', adult.data$marital.status)
adult.data$marital.status <- gsub('Married-spouse-absent', 'Married', adult.data$marital.status)
adult.data$marital.status <- gsub('Never-married', 'Single', adult.data$marital.status)
adult.data$marital.status <- as.factor(adult.data$marital.status)
summary(adult.data$marital.status)

#create the data frame
df3 <- data.frame(table(adult.data$earnings, adult.data$marital.status))
names(df3) <- c('earnings', 'marital.status', 'count')
df3

# calculate the percentages
df3 <- ddply(df3, .(marital.status), transform, percent = count/sum(count) * 100)
df3

# format the labels and calculate their positions
df3 <- ddply(df3, .(marital.status), transform, pos = (cumsum(count) - 0.5 * count))
df3$label <- paste0(sprintf("%.0f", df3$percent), "%")

#plot
ggplot(df3, aes(x = marital.status, y = count)) +
  geom_bar(aes(fill=earnings),stat = "identity",position = position_stack(reverse = TRUE)) +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income by Marital Status')


#-----------------------------------------------------------------------------------------------------------------------
#same analysis by race
df4 <- data.frame(table(adult.data$earnings, adult.data$race))
names(df4) <- c('earnings', 'race', 'count')
df4

# format the labels and calculate their positions
df4 <- ddply(df4, .(race), transform, pos = (cumsum(count) - 0.5 * count))
df4$label <- paste0(sprintf("%.0f", df3$percent), "%")

#plot
ggplot(df4, aes(x = race, y = count)) +
  geom_bar(aes(fill=earnings),stat = "identity",position = position_stack(reverse = TRUE)) +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income by Race')

#-------------------------------------------------------------------------------------------------------------------------
#Model fitting: logistic regression
#80% of the original data is used as the training set, while the rest 20% is used as test set.


adult.data$education <- NULL
adult.data$fnlwgt <- NULL
adult.data$relationship <- NULL

dim(adult.data)[1]
sz <- round(.8 * dim(adult.data)[1])  # training set size
training_set <- na.omit(adult.data[1:sz,])
testing_set <- adult.data[-(1:sz),]


#logistic regression
m1 <- glm(earnings ~ ., data = training_set, family = binomial('logit'))
summary(m1)


#To explore the possibility of a parsimonious model, both the forward and backward stepwise selection algorithms using AIC are performed.

m_full <- m1  # full model is the model just fitted
m_null <- glm(earnings ~ 1, data = training_set, family = binomial('logit'))

# backward selection
step(m_full, trace = F, scope = list(lower=formula(m_null), upper=formula(m_full)),
     direction = 'backward')



step(m_null, trace = F, scope = list(lower=formula(m_null), upper=formula(m_full)),
     direction = 'forward')
warnings()

summary(adult.data$education.num)
glimpse(adult.data$education.num)

hist(residuals(m1))
shapiro.test(residuals(m1))
boxplot(residuals(m1))


# create a data frame to store information regarding deviance residuals
index <- 1:dim(training_set)[1]
dev_resid <- residuals(m1)
income <- training_set$earnings
df5 <- data.frame(index, dev_resid, earnings)

ggplot(df5, aes(x = index, y = dev_resid, color = earnings)) +
  geom_point() + 
  geom_hline(yintercept = 3, linetype = 'dashed', color = 'blue') +
  geom_hline(yintercept = -3, linetype = 'dashed', color = 'blue')
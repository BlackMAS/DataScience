install.packages("caret", dependencies=c("Depends", "Suggests"))
install.packages("ellipse")
library(caret)
library(ellipse)
library(tidyverse)
library(plyr)

#Steps for machine learning
#Installing the R platform.
#Loading the dataset.
#Summarizing the dataset.
#Visualizing the dataset.
#Evaluating some algorithms.
#Making some predictions


#-------------------------------------------------------------------------------------------------------
#Loading the data
#1. Load the iris data the easy way.
#2. Load the iris data from CSV (optional, for purists).
#3. View missing values and number of unique values
#4. Separate the data into a training dataset and a validation dataset.

dataset <- read.csv('iris.csv', header=FALSE)


# set the column names in the dataset
colnames(dataset) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")

#see number of missing values
sapply(dataset, function(x) sum(is.na(x)))

#quikcly check for how many different values for each feature
sapply(dataset, function(x) length(unique(x)))



# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)

# select 20% of the data for validation
validation <- dataset[-validation_index,]

# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]

#--------------------------------------------------------------------------------------------------------------------------
#Summarize the data
#1.Dimensions of the dataset.
#2.Types of the attributes.
#3.Peek at the data itself.
#4.Levels of the class attribute.
#5.Breakdown of the instances in each class.
#6.Statistical summary of all attributes.

# dimensions of dataset (number of rows and columns)
dim(dataset)

#identify the types of the attributes (integers, characters, strings etc)
glimpse(dataset)

summary(dataset$Species)

# summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)


ggplot(data=dataset,mapping =aes(x=Species,y=Sepal.Length))+
  geom_boxplot()

ggplot(data = dataset, aes(x = "", y = Sepal.Length)) + 
  geom_boxplot()




# split input and output
x <- dataset[,1:4]
y <- dataset[,5]

# boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")

# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

#----------------------------------------------------------------------------------------------------------------------------------
#Evaluate models
#1. Set-up the test harness to use 10-fold cross validation.
#2. Build 5 different models to predict species from flower measurements
#3. Select the best model.

# We will 10-fold crossvalidation to estimate accuracy.

#This will split our dataset into 10 parts, train in 9 and test on 1 and release for all combinations of train-test splits. We will also repeat the #process 3 times for each algorithm with different splits of the data into 10 groups, in an effort to get a more accurate estimate.

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#We are using the metric of “Accuracy” to evaluate models. This is a ratio of the number of correctly predicted instances in divided by the total number of instances in the dataset multiplied by 100 to give a percentage (e.g. 95% accurate). We will be using the metric variable when we run build and evaluate each model next

#Build models
#Let’s evaluate 5 different algorithms:

#Linear Discriminant Analysis (LDA)
#Classification and Regression Trees (CART).
#k-Nearest Neighbors (kNN).
#Support Vector Machines (SVM) with a linear kernel.
#Random Forest (RF)

# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)

# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)

# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)

# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)


#summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
print(fit.lda)


# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)

validation$predictions <-predictions

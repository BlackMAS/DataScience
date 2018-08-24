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
#-------------------------------------------------------------------------------------------------------------------------

# split the data 80% train/20% test
#ungroup(accepted.planned.requested.match)
ind <- sample(2, nrow(accepted.planned.requested.match),replace=TRUE, prob = c(0.9,0.1))
training_data <-accepted.planned.requested.match[ind==1,]
Validation_data <-accepted.planned.requested.match[ind==2,]
colnames(training_data)


#output the number of missing values for each column
sapply(accepted.planned.requested.match, function(x) sum(is.na(x)))
sapply(training_data, function(x) sum(is.na(x)))
sapply(Validation_data, function(x) sum(is.na(x)))


#quikcly check for how many different values for each feature
sapply(accepted.planned.requested.match, function(x) length(unique(x)))
sapply(training_data, function(x) length(unique(x)))
sapply(Validation_data, function(x) length(unique(x)))


#ungroup(accepted.planned.requested.match)
#training_data <-accepted.planned.requested.match[1:45000,]
#Validation_data <-accepted.planned.requested.match[45001:46628,]
colnames(training_data)
#--------------------------------------------------------------------------------------------------------------
remove_missing_levels <- function(fit, test_data) {
  library(magrittr)
  
  # https://stackoverflow.com/a/39495480/4185785
  
  # drop empty factor levels in test data
  test_data %>%
    droplevels() %>%
    as.data.frame() -> test_data
  
  # 'fit' object structure of 'lm' and 'glmmPQL' is different so we need to
  # account for it
  if (any(class(fit) == "glmmPQL")) {
    # Obtain factor predictors in the model and their levels
    factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",
                     names(unlist(fit$contrasts))))
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }
    
    map(fit$contrasts, function(x) names(unmatrix(x))) %>%
      unlist() -> factor_levels
    factor_levels %>% str_split(":", simplify = TRUE) %>%
      extract(, 1) -> factor_levels
    
    model_factors <- as.data.frame(cbind(factors, factor_levels))
  } else {
    # Obtain factor predictors in the model and their levels
    factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",
                     names(unlist(fit$xlevels))))
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }
    
    factor_levels <- unname(unlist(fit$xlevels))
    model_factors <- as.data.frame(cbind(factors, factor_levels))
  }
  
  # Select column names in test data that are factor predictors in
  # trained model
  
  predictors <- names(test_data[names(test_data) %in% factors])
  
  # For each factor predictor in your data, if the level is not in the model,
  # set the value to NA
  
  for (i in 1:length(predictors)) {
    found <- test_data[, predictors[i]] %in% model_factors[
      model_factors$factors == predictors[i], ]$factor_levels
    if (any(!found)) {
      # track which variable
      var <- predictors[i]
      # set to NA
      test_data[!found, predictors[i]] <- NA
      # drop empty factor levels in test data
      test_data %>%
        droplevels() -> test_data
      # issue warning to console
      message(sprintf(paste0("Setting missing levels in '%s', only present",
                             " in test data but missing in train data,",
                             " to 'NA'."),
                      var))
    }
  }
  return(test_data)
}


#--------------------------------------------------------------------------------------------------------------
#Create and run logit model
is.this.late <- glm(Late.Delivery ~ Shipment.Weight..lb. + 
                      Shipment.Loaded.Miles + 
                      Shipment.Volume..cubic.ft.+ 
                      Stop.Number+requested.day +
                      Carrier.Name +
                      Total.Tenders +
                      Carrier.Type +
                      Shipt.Orig.Appt.parts.of.day +
                      Shipt.Dest.Appt.parts.of.day +
                      Shipt.Dest.Appt.parts.of.day.Orig +
                      Shipt.Planned.Departure.parts.of.day +
                      Shipt.Planned.Arrival.parts.of.day +
                      Shipt.Arrive.at.Origin.parts.of.day +
                      Shipt.Depart.Origin.parts.of.day +
                      Shipt.Arrive.at.Dest.parts.of.day +
                      Shipt.Orig.Appt.Day +
                      Shipt.Origin.Appt.Day.Orig +
                      Shipt.Dest.Appt.Day +
                      Shipt.Planned.Departure.Day +
                      Shipt.Arrive.at.Origin.Day +
                      Shipt.Depart.Origin.Day +
                      Shipt.Arrive.at.Dest.Day +
                      X1st.Tender.Accepted +
                      Destination.Loc.ID +
                      Origin.Loc.ID +
                      Cust.Hier1 +
                      Shipment.Destination.Appt.Reason +
                      Shipment.Origin.Appt.Reason +
                      ARRIVE_DEST_REA_CODE_FN +
                      TRANSPORT_MODE +
                      Shipment.Arrive.Destination.Reason +
                      Shipment.Arrive.Origin.Reason,family=binomial(link = 'logit'),training_data )


#training_data$predicted <- predict(is.this.late,newdata=training_data,type='response', se.fit=FALSE)
training_data$predicted <-predict(is.this.late,newdata=remove_missing_levels (fit=is.this.late, test_data=training_data),type='response', se.fit=FALSE)

training_data$Late.Prob <- ifelse(training_data$predicted > 0.6,"High",
                                  ifelse (training_data$predicted <0.6 & training_data$predicted >0.5, "Medium",
                                          ifelse(training_data$predicted <0.5,"Low","NA"
                                          )))

#show coefficients of logit model and other parameters
summary(is.this.late)
confusionMatrix(data=fitted.results, reference=Validation_data$Late.Delivery)

ggplot(training_data, aes(x=Carrier.Name, y=Late.Delivery))+ geom_bar(stat="identity", width=0.7, fill="steelblue")+ theme_minimal() + coord_flip()


#---------------------------------------------------------------------------------------------------------------------
#filter out data from training set where the predicted values are NA
training_data_without_NA <-training_data  %>%
  filter(!is.na(predicted))

install.packages("InformationValue")
library(InformationValue)
optCutOff <- optimalCutoff(training_data_without_NA$Late.Delivery, training_data_without_NA$predicted)[1] 


#Now to analyze the anova function of the model (i.e. analysis of deviance) You want the "Df Resid. Dev" to get smaller as you add more variables to the model
anova(is.this.late, test = "Chisq")

#Display McFadden R^2 to show overall significance of the model of capturing variability
pR2(is.this.late)

#-------------------------------------------------------------------------------------------------------------------
# MEASURING THE PREDICTIVE ABILITY OF THE MODEL


Validation_data$predicted <-predict(is.this.late,newdata=remove_missing_levels (fit=is.this.late, test_data=Validation_data),type='response', se.fit=FALSE)

Validation_data$Late.Prob <- ifelse(Validation_data$predicted > 0.6,"High",
                                  ifelse (Validation_data$predicted <0.6 & Validation_data$predicted >0.5, "Medium",
                                          ifelse(Validation_data$predicted <0.5,"Low","NA"
                                          )))



#---------------------------------------------------------------------------------------------------------------------
#filter out data from validation set where the predicted values are NA
Validation_data_without_NA <-Validation_data  %>%
  filter(!is.na(predicted))

# If prob > 0.7 then 1, else 0. Threshold can be set for better results
fitted.results <- ifelse(Validation_data_without_NA$predicted > 0.7,1,0)

misClasificError <- mean(fitted.results != Validation_data_without_NA$Late.Delivery)
print(paste('Accuracy',1-misClasificError))

#Accuracy was shown to be 94.7%
#--------------------------------------------------------------------------------------------------------------------
# Plot the ROC curve: The ROC is a curve generated by plotting the true positive rate (TPR) against the false positive rate (FPR) at various threshold settings

install.packages("ROCR")
library(ROCR)
p <- predict(is.this.late, newdata=Validation_data_without_NA, type="response")
pr <- prediction(p, Validation_data_without_NA$Late.Delivery)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

#the AUC is the area under the ROC curve. As a rule of thumb, a model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5.

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
#AUC was calculated at 84.4%





#-----------------------------------------------------------------------------------------------------------------
#sensitivity and specificity calculations
sensitivity(as.factor(Validation_data_without_NA$Late.Delivery), as.factor(fitted.results), threshold = 0.7)
specificity(as.factor(Validation_data_without_NA$Late.Delivery), as.factor(fitted.results), threshold = 0.7)
# Sensitivity was calculated at 95%
# Specificity was calculated at 83.7%

#-------------------------------------------------------------------------------------------------------------------

install.packages("caret")
library(caret)
confusionMatrix(data=fitted.results, reference=Validation_data_without_NA$Late.Delivery)
str(fitted.results)

str(Validation_data_without_NA$Late.Delivery)

#code to make sure categorical values are factors
is.factor(training_data$Shipt.Arrive.at.Dest.parts.of.day)
contrasts(training_data$Shipt.Arrive.at.Dest.parts.of.day)



str(training_data)

#-----------------------------------------------------------------------------------------------

#use decision trees
install.packages("FFTrees")
library(FFTrees)

training_data_without_NA_v <-select(training_data_without_NA,-c(predicted))

Validation_data_without_NA_V <- select(Validation_data_without_NA,-c(predicted))

late_FFT <- FFTrees(formula = Late.Delivery ~ 
                      Shipment.Weight..lb. + 
                      Shipment.Loaded.Miles + 
                      Shipment.Volume..cubic.ft.+ 
                      Stop.Number+requested.day +
                      Total.Tenders +
                      Carrier.Type +
                      Shipt.Orig.Appt.parts.of.day +
                      Shipt.Dest.Appt.parts.of.day +
                      Shipt.Dest.Appt.parts.of.day.Orig +
                      Shipt.Planned.Departure.parts.of.day +
                      Shipt.Planned.Arrival.parts.of.day +
                      Shipt.Arrive.at.Origin.parts.of.day +
                      Shipt.Depart.Origin.parts.of.day +
                      Shipt.Arrive.at.Dest.parts.of.day +
                      Shipt.Orig.Appt.Day +
                      Shipt.Origin.Appt.Day.Orig +
                      Shipt.Dest.Appt.Day +
                      Shipt.Planned.Departure.Day +
                      Shipt.Arrive.at.Origin.Day +
                      Shipt.Depart.Origin.Day +
                      Shipt.Arrive.at.Dest.Day +
                      X1st.Tender.Accepted +
                      Destination.Loc.ID +
                      Origin.Loc.ID +
                      Cust.Hier1 +
                      Shipment.Destination.Appt.Reason +
                      Shipment.Origin.Appt.Reason +
                      TRANSPORT_MODE +
                      Shipment.Arrive.Origin.Reason,           # The variable we are predicting
                     data = training_data_without_NA_v,                    # Training data
                     data.test = Validation_data_without_NA_V,                # Testing data
                     main = "Late Delivery",                # Main label
                     decision.labels = c("On Time", "Late")) # Label for decisions

late_FFT

plot(late_FFT,
     data = "test")

FFTrees.guide()

summary(training_data_without_NA_v)


sapply(training_data_without_NA_v, function(x) length(unique(x)))

#-----------------------------------------------------------------------------------------------------------------------------------------
#Use machine learning
library(caret)

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
fit.lda <- train(Late.Delivery ~
                   Shipment.Weight..lb. + 
                   Shipment.Loaded.Miles + 
                   Shipment.Volume..cubic.ft.+ 
                   Stop.Number+requested.day +
                   Total.Tenders +
                   Carrier.Type +
                   Shipt.Orig.Appt.parts.of.day +
                   Shipt.Dest.Appt.parts.of.day +
                   Shipt.Dest.Appt.parts.of.day.Orig +
                   Shipt.Planned.Departure.parts.of.day +
                   Shipt.Planned.Arrival.parts.of.day +
                   Shipt.Arrive.at.Origin.parts.of.day +
                   Shipt.Depart.Origin.parts.of.day +
                   Shipt.Arrive.at.Dest.parts.of.day +
                   Shipt.Orig.Appt.Day +
                   Shipt.Origin.Appt.Day.Orig +
                   Shipt.Dest.Appt.Day +
                   Shipt.Planned.Departure.Day +
                   Shipt.Arrive.at.Origin.Day +
                   Shipt.Depart.Origin.Day +
                   Shipt.Arrive.at.Dest.Day +
                   X1st.Tender.Accepted +
                   Destination.Loc.ID +
                   Origin.Loc.ID +
                   Cust.Hier1 +
                   Shipment.Destination.Appt.Reason +
                   Shipment.Origin.Appt.Reason +
                   TRANSPORT_MODE +
                   Shipment.Arrive.Origin.Reason, , data=training_data_without_NA, method="lda", metric=metric, trControl=control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Late.Delivery ~
                    Shipment.Weight..lb. + 
                    Shipment.Loaded.Miles + 
                    Shipment.Volume..cubic.ft.+ 
                    Stop.Number+requested.day +
                    Total.Tenders +
                    Carrier.Type +
                    Shipt.Orig.Appt.parts.of.day +
                    Shipt.Dest.Appt.parts.of.day +
                    Shipt.Dest.Appt.parts.of.day.Orig +
                    Shipt.Planned.Departure.parts.of.day +
                    Shipt.Planned.Arrival.parts.of.day +
                    Shipt.Arrive.at.Origin.parts.of.day +
                    Shipt.Depart.Origin.parts.of.day +
                    Shipt.Arrive.at.Dest.parts.of.day +
                    Shipt.Orig.Appt.Day +
                    Shipt.Origin.Appt.Day.Orig +
                    Shipt.Dest.Appt.Day +
                    Shipt.Planned.Departure.Day +
                    Shipt.Arrive.at.Origin.Day +
                    Shipt.Depart.Origin.Day +
                    Shipt.Arrive.at.Dest.Day +
                    X1st.Tender.Accepted +
                    Destination.Loc.ID +
                    Origin.Loc.ID +
                    Cust.Hier1 +
                    Shipment.Destination.Appt.Reason +
                    Shipment.Origin.Appt.Reason +
                    TRANSPORT_MODE +
                    Shipment.Arrive.Origin.Reason, , data=training_data_without_NA, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
glimpse(Validation_data_without_NA$Late.Delivery)

Validation_data_without_NA$Late.Delivery <-as.factor(Validation_data_without_NA$Late.Delivery)
fit.knn <- train(Late.Delivery ~
                   Shipment.Weight..lb. + 
                   Shipment.Loaded.Miles + 
                   Shipment.Volume..cubic.ft.+ 
                   Stop.Number+requested.day +
                   Total.Tenders +
                   Carrier.Type +
                   Shipt.Orig.Appt.parts.of.day +
                   Shipt.Dest.Appt.parts.of.day +
                   Shipt.Dest.Appt.parts.of.day.Orig +
                   Shipt.Planned.Departure.parts.of.day +
                   Shipt.Planned.Arrival.parts.of.day +
                   Shipt.Arrive.at.Origin.parts.of.day +
                   Shipt.Depart.Origin.parts.of.day +
                   Shipt.Arrive.at.Dest.parts.of.day +
                   Shipt.Orig.Appt.Day +
                   Shipt.Origin.Appt.Day.Orig +
                   Shipt.Dest.Appt.Day +
                   Shipt.Planned.Departure.Day +
                   Shipt.Arrive.at.Origin.Day +
                   Shipt.Depart.Origin.Day +
                   Shipt.Arrive.at.Dest.Day +
                   X1st.Tender.Accepted +
                   Destination.Loc.ID +
                   Origin.Loc.ID +
                   Cust.Hier1 +
                   Shipment.Destination.Appt.Reason +
                   Shipment.Origin.Appt.Reason +
                   TRANSPORT_MODE +
                   Shipment.Arrive.Origin.Reason, , data=training_data_without_NA, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)





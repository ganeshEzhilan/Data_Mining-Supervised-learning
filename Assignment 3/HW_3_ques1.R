##########################################################################################
## This code is to fit the classification models in Boston data set  
## Boobalaganesh Ezhilan 
## Created: October21, 2018
## Edited: 
#################################################################################
rm(list = ls())

# set the current working directory
setwd("/Users/Dell/Desktop/Data Mining codes Samples/Assignment 3")

# install some CRAN libararies 
install.packages("MASS")
install.packages("ggplot2")
install.packages("devtools")
library(devtools)
library(easyGgplot2)
install_github("easyGgplot2", "kassambara")

#load the packages
library(MASS)
library(ggplot2)


#Calculating the median for the Boston crime rate
crime_median <- median(Boston$crim)

############# Modelling 1 with Full Subset #########################

###############################################################################
# Classification based upon median value of crime data
#####################################################################################
Boston$resp <- 0
Boston$resp[Boston$crim > crime_median] <- 1
bostonHousingData <- Boston[,-1]

###########################################################################
# Create a training and test set
###########################################################################
set.seed(123)
train <- sample(1:nrow(bostonHousingData), .66*nrow(bostonHousingData))
Boston_train <- bostonHousingData[train,]
Boston_test <- bostonHousingData[-train,]

#################################################################################
# Logistic Regression
###################################################################################
Boston_train$resp <- as.factor(Boston_train$resp)
Boston_test$resp <- as.factor(Boston_test$resp)
glm.fit <- glm(resp ~., data = Boston_train, family = "binomial")
summary(glm.fit)
names(glm.fit)

# Predict
glm.probs.train <- predict(glm.fit, newdata = Boston_train, type = "response")
y_hat_train <- as.numeric(round(glm.probs.train))
glm.probs.test <- predict(glm.fit, newdata = Boston_test, type = "response")
y_hat_test <- as.numeric(round(glm.probs.test))
y_true_train <- as.numeric(Boston_train$resp)-1
y_true_test <- as.numeric(Boston_test$resp)-1


logistic_train_error <- sum(abs(y_hat_train - y_true_train))/length(y_true_train)
logistic_test_error<- sum(abs(y_hat_test - y_true_test))/length(y_true_test)
logistic_train_error
logistic_test_error


###################################################################################
# Linear Discriminant Analysis
############################################################################
Boston_train$resp <- as.factor(Boston_train$resp)
Boston_test$resp <- as.factor(Boston_test$resp)
lda.fit <- lda(resp~., data = Boston_train)
lda.pred.train <- predict(lda.fit, newdata = Boston_train)
y_hat_train <- as.numeric(lda.pred.train$class)-1
lda.pred.test <- predict(lda.fit, newdata = Boston_test)
y_hat_test <- as.numeric(lda.pred.test$class)-1
y_true_train <- as.numeric(Boston_train$resp)-1
y_true_test <- as.numeric(Boston_test$resp)-1

# Compute the error
lda_train_error <- sum(abs(y_true_train - y_hat_train))/length(y_true_train)
lda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)
lda_train_error
lda_test_error

###################################################################################
# Knn Classification
############################################################################
require(class)
knn_test_error <- rep(NA,9)
for(i in 1:10){
  knn.pred <- knn(train=Boston_train, test=Boston_test, cl=Boston_train$resp, k=i)
  knn_test_error[i-1] <- mean(knn.pred!=Boston_test$resp)
}
knn_test_error
knn_test_error <- min(knn_test_error)
knn_test_error

#calculating the corelation matrix
bostonCorelationMatrix <- cor(bostonHousingData)
round(bostonCorelationMatrix, 2)

summary(glm.fit)

FirstModelling = data.frame("method"=c("LR", "LDA","KNN"), test.err=c(logistic_test_error,lda_test_error,knn_test_error))
FirstModelling

# Modelling 2 Removing 3 varaibles from the boston housing data set

#Removing the vabostonWorstHousingDatariables dis,rad,nox from the boston housing data set
bostonWorstHousingData <- bostonHousingData
bostonWorstHousingData <- bostonHousingData[,-8]
bostonWorstHousingData <- bostonWorstHousingData[,-4]
bostonWorstHousingData <- bostonWorstHousingData[,-6]

###########################################################################
# Create a training and test set
###########################################################################
set.seed(123)
train <- sample(1:nrow(bostonWorstHousingData), .66*nrow(bostonWorstHousingData))
Boston_train <- bostonWorstHousingData[train,]
Boston_test <- bostonWorstHousingData[-train,]

#################################################################################
# Logistic Regression
###################################################################################
Boston_train$resp <- as.factor(Boston_train$resp)
Boston_test$resp <- as.factor(Boston_test$resp)
glm.fit <- glm(resp ~., data = Boston_train, family = "binomial")
summary(glm.fit)
names(glm.fit)

# Predict
glm.probs.train <- predict(glm.fit, newdata = Boston_train, type = "response")
y_hat_train <- as.numeric(round(glm.probs.train))
glm.probs.test <- predict(glm.fit, newdata = Boston_test, type = "response")
y_hat_test <- as.numeric(round(glm.probs.test))
y_true_train <- as.numeric(Boston_train$resp)-1
y_true_test <- as.numeric(Boston_test$resp)-1

logistic_train_error <- sum(abs(y_hat_train - y_true_train))/length(y_true_train)
logistic_test_error<- sum(abs(y_hat_test - y_true_test))/length(y_true_test)
logistic_train_error
logistic_test_error
###################################################################################
# Linear Discriminant Analysis
############################################################################
Boston_train$resp <- as.factor(Boston_train$resp)
Boston_test$resp <- as.factor(Boston_test$resp)
lda.fit <- lda(resp~., data = Boston_train)
lda.pred.train <- predict(lda.fit, newdata = Boston_train)
y_hat_train <- as.numeric(lda.pred.train$class)-1
lda.pred.test <- predict(lda.fit, newdata = Boston_test)
y_hat_test <- as.numeric(lda.pred.test$class)-1
y_true_train <- as.numeric(Boston_train$resp)-1
y_true_test <- as.numeric(Boston_test$resp)-1

# Compute the error
lda_train_error <- sum(abs(y_true_train - y_hat_train))/length(y_true_train)
lda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)
lda_train_error
lda_test_error

###################################################################################
# Knn Classification
############################################################################
require(class)
knn_test_error <- rep(NA,9)
for(i in 1:10){
  knn.pred <- knn(train=Boston_train, test=Boston_test, cl=Boston_train$resp, k=i)
  knn_test_error[i-1] <- mean(knn.pred!=Boston_test$resp)
}
knn_test_error <- min(knn_test_error)
knn_test_error
summary(glm.fit)
SecondModelling = data.frame("method"=c("LR", "LDA","KNN"), test.err=c(logistic_test_error,lda_test_error,knn_test_error))
SecondModelling


#calculating the corelation matrix
bostonCorelationMatrix <- cor(Boston)
round(bostonCorelationMatrix, 2)


#######################   Third Modelling ########################

###########################################################################
# Create a training and test set
###########################################################################
attach(Boston)
boxplot(eval(parse(text="rm")) ~ resp, ylab="rm", col=c("green", "yellow"), varwidth=T)

chas_resp <- table(chas, resp)
barplot(chas_resp, beside = T, legend=rownames(chas_resp), col=c("green", "yellow"))
bostonBestHousingData <- bostonHousingData
bostonBestHousingData <- bostonHousingData[,-5]
bostonBestHousingData <- bostonBestHousingData[,-3]
bostonBestHousingData <- bostonBestHousingData[,-10]

###########################################################################
# Create a training and test set
###########################################################################
set.seed(123)
train <- sample(1:nrow(bostonBestHousingData), .66*nrow(bostonBestHousingData))
Boston_train <- bostonBestHousingData[train,]
Boston_test <- bostonBestHousingData[-train,]

#################################################################################
# Logistic Regression
###################################################################################
Boston_train$resp <- as.factor(Boston_train$resp)
Boston_test$resp <- as.factor(Boston_test$resp)
glm.fit <- glm(resp ~., data = Boston_train, family = "binomial")
summary(glm.fit)
names(glm.fit)

# Predict
glm.probs.train <- predict(glm.fit, newdata = Boston_train, type = "response")
y_hat_train <- as.numeric(round(glm.probs.train))
glm.probs.test <- predict(glm.fit, newdata = Boston_test, type = "response")
y_hat_test <- as.numeric(round(glm.probs.test))
y_true_train <- as.numeric(Boston_train$resp)-1
y_true_test <- as.numeric(Boston_test$resp)-1


logistic_train_error <- sum(abs(y_hat_train - y_true_train))/length(y_true_train)
logistic_test_error<- sum(abs(y_hat_test - y_true_test))/length(y_true_test)
logistic_train_error
logistic_test_error


###################################################################################
# Linear Discriminant Analysis
############################################################################
Boston_train$resp <- as.factor(Boston_train$resp)
Boston_test$resp <- as.factor(Boston_test$resp)
lda.fit <- lda(resp~., data = Boston_train)
lda.pred.train <- predict(lda.fit, newdata = Boston_train)
y_hat_train <- as.numeric(lda.pred.train$class)-1
lda.pred.test <- predict(lda.fit, newdata = Boston_test)
y_hat_test <- as.numeric(lda.pred.test$class)-1
y_true_train <- as.numeric(Boston_train$resp)-1
y_true_test <- as.numeric(Boston_test$resp)-1

# Compute the error
lda_train_error <- sum(abs(y_true_train - y_hat_train))/length(y_true_train)
lda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)
lda_train_error
lda_test_error

###################################################################################
# Knn Classification
############################################################################
knn_test_error <- rep(NA,9)
for(i in 1:10){
  knn.pred <- knn(train=Boston_train, test=Boston_test, cl=Boston_train$resp, k=i)
  knn_test_error[i-1] <- mean(knn.pred!=Boston_test$resp)
}
knn_test_error <- min(knn_test_error)
knn_test_error

thirdModelling = data.frame("method"=c("LR", "LDA","KNN"), test.err=c(logistic_test_error,lda_test_error,knn_test_error))
thirdModelling
ggplot(data=FirstModelling, aes(x=method,y=test.err)) + geom_bar(stat="identity", aes(fill=method))+ggtitle("First Modelling")
ggplot(data=SecondModelling, aes(x=method,y=test.err)) + geom_bar(stat="identity", aes(fill=method))+ggtitle("Second Modelling")
ggplot(data=thirdModelling, aes(x=method,y=test.err)) + geom_bar(stat="identity", aes(fill=method))+ggtitle("Third Modelling")

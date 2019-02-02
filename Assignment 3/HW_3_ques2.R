##########################################################################################
## This code performs Linear Discriminant Analysis and Quadratic Discriminant Analysis
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
install.packages("Hmisc")
install.packages("MVN")
install.packages("corrplot")


#load the packages
library(MASS)
library("ggplot2")
library(Hmisc)
library(MVN)
library(corrplot)



pima_Sample <- read.table("Diabetes.txt",header = FALSE)

pima <- pima_Sample[4:10]
colnames(pima) <- c("obser_no","glucose.area","insulin.area","SSDG","relative.weight","fasting.plasma.glucose","class_no")

###################################################################################
# Plotting Pairwise Scatterplot for different classes of Classes
############################################################################
my_cols <- c("yellow", "green", "purple") 
pairs(pima[,1:5],col =my_cols[pima$class_no],  pch = 19)

class.one <- subset(pima , class_no == 1)
class.two <- subset(pima , class_no == 2)
class.three <- subset(pima , class_no == 3)

class_1<-pima[which(pima$class_no==1),]
corrplot(cor(as.matrix(class_1[,-6])),title = "Class 1",mar=c(0,0,1,0))

class_2<-pima[which(pima$class_no==2),]
corrplot(cor(as.matrix(class_2[,-6])),title = "Class 2",mar=c(0,0,1,0))

class_3<-pima[which(pima$class_no==3),]
corrplot(cor(as.matrix(class_3[,-6])),title = "Class 3",mar=c(0,0,1,0))


###################################################################################
# Creating covariance matrix for different classes
############################################################################
Pima.corr_one <- rcorr(as.matrix(subset( class.one, select = -c(1, 7 ))))
Pima.corr_two <- rcorr(as.matrix(subset( class.two, select = -c(1, 7 ))))
Pima.corr_three <- rcorr(as.matrix(subset( class.three, select = -c(1, 7 ))))
Pima.corr_one
Pima.corr_two
Pima.corr_three


###################################################################################
# Checking Multivariate Analysis
############################################################################
attach(pima)
diabetes.glucose <- table(glucose.area, class_no)

ggplot(pima, aes(x = scale(glucose.area),y=class_no)) +geom_histogram(aes(y = ..count..),binwidth = 0.3,colour = "goldenrod2", fill = "green")


diabetes.insulin <- table(insulin.area, class_no)
ggplot(pima, aes(x = scale(insulin.area),y=class_no)) +geom_histogram(aes(y = ..count..),binwidth = 0.3,colour = "goldenrod2", fill = "green")


diabetes.SSDG <- table(SSDG, class_no)
ggplot(pima, aes(x = scale(SSDG),y=class_no)) +geom_histogram(aes(y = ..count..),binwidth = 0.3,colour = "goldenrod2", fill = "green")


diabetes.relative.weight <- table(relative.weight, class_no)
ggplot(pima, aes(x = scale(relative.weight),y=class_no)) +geom_histogram(aes(y = ..count..),binwidth = 0.3,colour = "goldenrod2", fill = "green")


ggplot(pima, aes(x = scale(fasting.plasma.glucose),y=class_no)) +geom_histogram(aes(y = ..count..),binwidth = 0.3,colour = "goldenrod2", fill = "green")


result <- mvn(data = pima, mvnTest = "mardia")
result$multivariateNormality

result <- mvn(data = subset(class.one,select= -c(class_no), mvnTest = "mardia"))
result$multivariateNormality

result <- mvn(data = subset(class.two,select= -c(class_no), mvnTest = "mardia"))
result$multivariateNormality

result <- mvn(data = subset(class.three,select= -c(class_no), mvnTest = "mardia"))
result$multivariateNormality


###########################################################################
# Create a training and test set
###########################################################################
#seed(123)
pima <- pima[,-1]
train <- sample(1:nrow(pima), .66*nrow(pima))
pima_train <- pima[train,]
pima_test <- pima[-train,]

###################################################################################
# Creating the correlation matrix
############################################################################
Pima.corr <- rcorr(as.matrix(pima_train))
round(Pima.corr$r, 2)

###################################################################################
# Linear Discriminant Analysis
############################################################################
str(pima)
pima_train$class_no <- as.factor(pima_train$class_no)
pima_test$class_no <- as.factor(pima_test$class_no)

lda.fit <- lda(class_no~., data = pima_train)
lda.pred.train <- predict(lda.fit, newdata = pima_train)
y_hat_train <- as.numeric(lda.pred.train$class)
lda.pred.test <- predict(lda.fit, newdata = pima_test)
y_hat_test <- as.numeric(lda.pred.test$class)
y_true_train <- as.numeric(pima_train$class_no)
y_true_test <- as.numeric(pima_test$class_no)

# Compute the error
lda_train_error <- sum(abs(y_true_train - y_hat_train))/length(y_true_train)
lda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)
lda_train_error
lda_test_error

###################################################################################
# Quadractic Discriminant Analysis
############################################################################
pima_train$class_no <- as.factor(pima_train$class_no)
pima_test$class_no <- as.factor(pima_test$class_no)

qda.fit <- qda(class_no~., data = pima_train)

qda.pred.train <- predict(qda.fit, newdata = pima_train)
y_hat_train <- as.numeric(qda.pred.train$class)
qda.pred.test <- predict(qda.fit, newdata = pima_test)
y_hat_test <- as.numeric(qda.pred.test$class)
y_true_train <- as.numeric(pima_train$class_no)
y_true_test <- as.numeric(pima_test$class_no)

# Compute the error
qda_train_error <- sum(abs(y_true_train - y_hat_train))/length(y_true_train)
qda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)
qda_train_error
qda_test_error


###################################################################################
# Predicting the Class Number for given Values
############################################################################
glucose.area=c(0.98)
insulin.area=c(122)
SSDG=c(544)
relative.weight=c(186)
fasting.plasma.glucose=c(184)
prdict_row=data.frame(glucose.area,insulin.area,SSDG,relative.weight,fasting.plasma.glucose)
predict(lda.fit,newdata = prdict_row)
predict(qda.fit,newdata = prdict_row)

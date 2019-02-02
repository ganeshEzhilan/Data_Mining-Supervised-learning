############################################################################################################
## This code is to compare classification performance of linear regression and k-Nearest neighbour
## Boobalaganesh Ezhilan 
## Created: September09, 2018
## Edited: 
################################################################################

# set the current working directory
setwd("/Users/Dell/Desktop/Data Mining codes Samples/Assignment 1")

# install some CRAN libararies 
install.packages("ElemStatLearn")
install.packages("ggplot2")

#load the packages
library("ElemStatLearn")
library("ggplot2")

ls("package:ElemStatLearn")
zip.test
?zip.train

#linear regression model

#Making data frames
zipCodeTrain <- as.data.frame(zip.train)
zipCodeTest <- as.data.frame(zip.test)
dim(zipCodeTrain)
dim(zipCodeTest)
summary(zipCodeTrain)

#Taking the classified Data
classZipCodeTrain1 <- subset(zipCodeTrain ,V1==2)
classZipCodeTrain2 <- subset(zipCodeTrain,V1 == 3)
classZipCodeTrain <- rbind(classZipCodeTrain1,classZipCodeTrain2)
classZipCodeTrain
ClassZipCodeTest1 <- subset(zipCodeTest,V1==2)
ClassZipCodeTest2 <- subset(zipCodeTest,V1==3)
ClassZipCodeTest <- rbind(ClassZipCodeTest1,ClassZipCodeTest2)
ClassZipCodeTest

#linear model
fit <- lm(V1~.,data=classZipCodeTrain)

#calculate linear training error and test error
predictZipTrain <- predict(fit,classZipCodeTrain)
View(predictZipTrain)
predictZipTrain<- round(predictZipTrain, 0)
View(predictZipTrain)
predictZipTest <- predict(fit,ClassZipCodeTest)
predictZipTest<-round(predictZipTest, 0)
predictZipTest
lnTrainFrame<-data.frame(cbind(actual_digits=classZipCodeTrain$V1, predicted_digits=predictZipTrain))
View(lnTrainFrame)
sum(lnTrainFrame$actual_digits == lnTrainFrame$predicted_digits)
nrow(classZipCodeTrain)
lnTrainError <- 1 - sum(lnTrainFrame$actual_digits == lnTrainFrame$predicted_digits)/nrow(classZipCodeTrain)
lnTrainError
lnTestFrame <- data.frame(cbind(actual_digits=ClassZipCodeTest$V1, predicted_digits=predictZipTest))
lnTestFrame
lnTestError <- 1 - sum(lnTestFrame$actual_digits == lnTestFrame$predicted_digits)/nrow(ClassZipCodeTest)
lnTestError

#calculate KNN training error and test error
require(class)
View(classZipCodeTrain[, 1])
classZipCodeTrain[, 1] <- as.factor(classZipCodeTrain[, 1])
View(classZipCodeTrain[, 1])
classZipCodeTrain[, 1]
?knn()
ClassZipCodeTest[, 1] <- as.factor(ClassZipCodeTest[, 1])
charknn <- function(i, j, kk){
  knnTrainFrame <- knn(i, i, i$V1, kk, l=0, prob = FALSE, use.all = TRUE)   ## stack over flow
  KnnTrainError <-  1 - sum(knnTrainFrame==i$V1)/nrow(i)
  knnTestFrame <- knn(i, j, i$V1, kk)
  KnnTestError <- 1 - sum(knnTestFrame==j$V1)/nrow(j)
  errorValue <- data.frame(k = c(kk), i = c(KnnTrainError), j = c(KnnTestError))
  return(errorValue)
}

complexFactor <- list(1,3,5,7,9,11,13,15)

knnerror <- data.frame(k=c(), i = c(), j = c())
for (i in complexFactor){
  complexFactor[i]
  tmperror <- charknn(classZipCodeTrain, ClassZipCodeTest, as.numeric(i))         
  knnerror <- rbind(knnerror, tmperror)
}

lnTrainError
lnTestError


g <- ggplot(knnerror, aes(k))
g <- g + geom_line(aes(y=i), colour="blue")                     ##Stack over flow
g <- g + geom_line(aes(y=j), colour="green")
g

?knn()

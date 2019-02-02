##########################################################################################
## This code is to fit the random forest explore sensitivity for randomly selected inputs
## Boobalaganesh Ezhilan 
## Created: December16, 2018
## Edited: 
#################################################################################
rm(list = ls())

#install some CRAN libararies 
library(ElemStatLearn)
install.packages("randomForest")
install.packages("ggplot2")

#load the packages
library(randomForest)
library(ggplot2)

spamdata<-spam
###########################################################################
# Create a training and test set
###########################################################################
train=sample(1:nrow(spamdata),0.75*nrow(spamdata))
traindata<-spamdata[train,]
testdata <- spamdata[-train,]
mtrylist = c()
accuracyList = c()

###########################################################################
# checking the sensitivity for different values of m
###########################################################################
for(i in seq(1,15,2)){
  rf.fit = randomForest(spam~.,data = traindata, ntree =100, mtry = i)
  rf.fit.pred_test <- predict(rf.fit, newdata = testdata, type='class')
  rf_test_err <- mean(rf.fit.pred_test != testdata$spam)
  mtrylist = c(mtrylist,i)
  accuracyList = c(accuracyList,rf_test_err)
}
plotDataFrame = data.frame(Model_Name = mtrylist,Test_Error = accuracyList)
plot(plotDataFrame$Model_Name,plotDataFrame$Test_Error,type='o')

OOBlist = rep(0, 100)
for(i in seq(1,15,2)){
  rf.fit = randomForest(spam~., data  = traindata, ntree = 100, mtry = i)
  rf.predictions = predict(rf.fit, testdata, type = "class")
  OOBlist = cbind(OOBlist, rf.fit$err.rate[,c(1)])
}

#out of bag errors.
OOBlist_plot = data.frame(OOBlist)[,-c(1)]
names(OOBlist_plot) = c("mtry_2", "mtry_4","mtry_6","mtry_8","mtry_10")
OOBlist_plot$NTrees = seq(1, 100)

#plotting for different values of m
ggplot(OOBlist_plot, aes(NTrees)) + 
  geom_line(aes(y = mtry_2, colour = "mtry_2")) + geom_point(aes(y = mtry_2), size = 0.3) + 
  geom_line(aes(y = mtry_4, colour = "mtry_4")) + geom_point(aes(y = mtry_4), size = 0.3) + 
  geom_line(aes(y = mtry_6, colour = "mtry_6")) + geom_point(aes(y = mtry_6), size = 0.3) + 
  geom_line(aes(y = mtry_8, colour = "mtry_8")) + geom_point(aes(y = mtry_8), size = 0.3) + 
  geom_line(aes(y = mtry_10, colour = "mtry_10")) + geom_point(aes(y = mtry_10), size = 0.3) + 
  ggtitle("Out of Bag Errors.") + xlab("Features") + ylab("OOB Error") +
  theme(plot.title = element_text(hjust = 0.5))



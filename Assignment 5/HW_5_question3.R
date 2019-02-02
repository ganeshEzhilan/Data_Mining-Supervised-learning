##########################################################################################
## This code is to fit the Fit separate singlehidden-layer neural networks to the original learning-set data and to the learningset data with the outlier
## Boobalaganesh Ezhilan 
## Created: December16, 2018
## Edited: 
#################################################################################
rm(list = ls())

#install some CRAN libararies 
install.packages("gam")
library(neuralnet)
library(ElemStatLearn)
library(gam)

library(neuralnet)
library(ElemStatLearn)
library(gam)


#Normalizing the values
formula = "spam~ A.1"
for (colname in colnames(spam)){
  if(colname != "spam" & colname != "A.1" )
  {
    formula = paste(formula, colname , sep = " + ")    
  }
}

formula = as.formula(formula)

Spam = spam

Spam$spam = as.numeric((Spam$spam))-1
Spam = as.data.frame(scale(Spam))
Spam$spam = spam$spam
Spam$spam = as.numeric((spam$spam))-1


train = sample(1:nrow(Spam), nrow(Spam)*0.80)
test = -train
trainData = Spam[train, ]
testData = Spam[test, ]

neural <- neuralnet(formula , data = trainData, hidden = 7, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(neural, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
mean(predicted_class_new_data != testData$spam)
plot(neural)

#2.390700435355
trainOutLier = trainData

#calculate error for original value
trainOutLier[1,4] = 300 
neural <- neuralnet(formula , data = trainOutLier, hidden = 7, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(neural, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
mean(predicted_class_new_data != testData$spam)


#calculate error for reduced value 100
trainOutLier[1,4] = 100 
neural <- neuralnet(formula , data = trainOutLier, hidden = 7, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(neural, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
mean(predicted_class_new_data != testData$spam)


#calculate error for reduced value 30
trainOutLier[1,4] = 30 
neural <- neuralnet(formula , data = trainOutLier, hidden = 7, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(neural, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
mean(predicted_class_new_data != testData$spam)


#calculate error for reduced value 3
trainOutLier[1,4] = 3
neural <- neuralnet(formula , data = trainOutLier, hidden = 7, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(neural, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
mean(predicted_class_new_data != testData$spam)


#calculate error for reduced value 0.1
trainOutLier[1,4] = 0.1
neural <- neuralnet(formula , data = trainOutLier, hidden = 7, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(neural, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
mean(predicted_class_new_data != testData$spam)

#calculate error for reduced value -10
trainOutLier[1,4] = -10
neural <- neuralnet(formula , data = trainOutLier, hidden = 7, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(neural, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
mean(predicted_class_new_data != testData$spam)


#calculate error for reduced value -1
trainOutLier[1,4] = -1
neural <- neuralnet(formula , data = trainOutLier, hidden = 7, act.fct = "logistic",err.fct = 'sse', linear.output = FALSE)
new.output <- compute(neural, covariate = testData[,1:57])
predicted_class_new_data <- round(new.output$net.result)
mean(predicted_class_new_data != testData$spam)
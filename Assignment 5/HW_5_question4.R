##########################################################################################
## This code is to analyze support vector machine
## Boobalaganesh Ezhilan 
## Created: December16, 2018
## Edited: 
#################################################################################
rm(list = ls())

#install some CRAN libararies 
install.packages("Metrics")
library(ISLR)
library(e1071)
library(Metrics)
install.packages("plotly")

OJ = data.frame(OJ)
#changing the type of the features. 
OJ$StoreID = as.factor(as.character(OJ$StoreID))
OJ$SpecialCH = as.factor(as.character(OJ$SpecialCH))
OJ$SpecialMM = as.factor(as.character(OJ$SpecialMM))
OJ$STORE = as.factor(as.character(OJ$STORE))

#Spliting into test and train data
set.seed(1234)
train_index = sample(1:nrow(OJ), 0.7*nrow(OJ))
train = OJ[train_index,]
test = OJ[-train_index,]



###########################################################################
# Create a training and test set
###########################################################################
train_errors = c()
test_errors = c()
cost_Parameter = seq(0.01, 10, 0.1)
cost_Parameter = append(cost_Parameter, 10)
for(i in cost_Parameter){
  svm_model = svm(Purchase~., train, cost = i)
  prediction = predict(svm_model, test)
  table = table(test$Purchase, prediction)
  test_error = nrow(test) - (table[1,1] + table[2,2])
  test_errors = append(test_errors, test_error)
  prediction = predict(svm_model, train)
  table = table(train$Purchase, prediction)
  train_error = nrow(train) - (table[1,1] + table[2,2])
  train_errors = append(train_errors, train_error)
}

windows()
plot_results = data.frame(cost_Parameter, train_errors, test_errors)
names(plot_results) = c("cost", "train_error", "test_error")

library(plotly)
p = plot_ly(plot_results, x = ~cost, y = ~train_error, name = 'Train_Error', type = 'scatter', 
            mode = 'lines+markers') %>%
  add_trace(y = ~test_error, name = 'Test_Error', mode = 'lines+markers') %>%
  layout(title = "Train Vs Test Error Performance",
         xaxis = list(title = "Cost Parmater"),
         yaxis = list (title = "Error"))
p
#####


#radial kernel
#####
train_errors = c()
test_errors = c()
cost_Parameter = seq(0.01, 10, 0.1)
cost_Parameter = append(cost_Parameter, 10)
count = 0
for(i in cost_Parameter){
  svm_model = svm(Purchase~., train, cost = i, kernel = "radial")
  predictions = predict(svm_model, test)
  table = table(test$Purchase, predictions)
  test_error = nrow(test) - (table[1,1] + table[2,2])
  test_errors = append(test_errors, test_error)
  
  predictions = predict(svm_model, train)
  table = table(train$Purchase, predictions)
  train_error = nrow(train) - (table[1,1] + table[2,2])
  train_errors = append(train_errors, train_error)
}


plot_results = data.frame(cost_Parameter, train_errors, test_errors)
names(plot_results) = c("cost", "train_error", "test_error")

library(plotly)
p = plot_ly(plot_results, x = ~cost, y = ~train_error, name = 'Train_Error', type = 'scatter', 
            mode = 'lines+markers') %>%
  add_trace(y = ~test_error, name = 'Test_Error', mode = 'lines+markers') %>%
  layout(title = "Train Vs Test Error Performance. Radial Kernel.",
         xaxis = list(title = "Cost"),
         yaxis = list (title = "Error"))
p
#####


#polynomial kernel. degree = 2
#####
train_errors = c()
test_errors = c()
cost_list = seq(0.01, 10, 0.1)
cost_list = append(cost_list, 10)
count = 0
for(i in cost_list){
  svm_model = svm(Purchase~., train, cost = i, kernel = "polynomial", degree = 2)
  predictions = predict(svm_model, test)
  table = table(test$Purchase, predictions)
  test_error = nrow(test) - (table[1,1] + table[2,2])
  test_errors = append(test_errors, test_error)
  
  predictions = predict(svm_model, train)
  table = table(train$Purchase, predictions)
  train_error = nrow(train) - (table[1,1] + table[2,2])
  train_errors = append(train_errors, train_error)
}


plot_results = data.frame(cost_list, train_errors, test_errors)
names(plot_results) = c("cost", "train_error", "test_error")

library(plotly)
p = plot_ly(plot_results, x = ~cost, y = ~train_error, name = 'Train_Error', type = 'scatter', 
            mode = 'lines+markers') %>%
  add_trace(y = ~test_error, name = 'Test_Error', mode = 'lines+markers') %>%
  layout(title = "Train Vs Test Error Performance. Polynomial Kernel.",
         xaxis = list(title = "Cost"),
         yaxis = list (title = "Error"))
p
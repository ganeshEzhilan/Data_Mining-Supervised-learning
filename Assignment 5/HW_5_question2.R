##########################################################################################
## This code is to determine number of neurons to use in the layers
## Boobalaganesh Ezhilan 
## Created: December16, 2018
## Edited: 
#################################################################################
rm(list = ls())

set.seed(123)
data(spam)
spam$spam <- ifelse(spam$spam == "spam",1,0)
spam<- spam[1:4600,]

## cv error 
n <- names(spam)
f <- as.formula(paste("spam ~", paste(n[!n %in% "spam"], collapse = " + ")))
crossvalidate <- function(spam, hidden_1)
{  
  
  ### splitting data into train and test set ###
  data.sample <- sample(1:nrow(spam), round(nrow(spam)*0.75))
  train.data<- spam[data.sample,]
  cv.data <- spam[-data.sample,]
  
  ## neural network fit ##
  network <- neuralnet(f, data=train.data, hidden=hidden_1, act.fct = "logistic",err.fct = 'sse', linear.output=FALSE,threshold=0.15)
  predict.network <- compute(network,cv.data[,1:57])
  
  ###### specifying and predicting the class ####
  predict <- round(predict.network$net.result)
  error_cv <- mean(predict != cv.data$spam)
  return(error_cv)
}

##CREATING TRAINING AND TEST SET
data.sample <- sample(1:nrow(spam), round(nrow(spam)*0.80))
train.data<- spam[data.sample,]
test.data <- spam[-data.sample,]

## selecting the hidden neural networks 
error_test <- NULL
error_cv <- NULL
set.seed(100)

## using cross validation
for(i in 1:5)
{
  error_cv[i] = crossvalidate(spam,hidden=c(i))
}

## plotting cv errors
plot(error_cv,main='Mean vs hidden neurons',xlab="Hidden neurons", ylab='error MSE of test',type='l4',col='green',lwd=2)

min_error_neuron = min(which(min(error_cv) == error_cv))
network <- neuralnet(f, data=train.data, hidden=c(min_error_neuron), act.fct = "logistic",err.fct = 'sse', linear.output=FALSE, threshold=0.15)
predict.network <- compute(network, test.data[,1:57])

## Calculating test error
error_test <- mean(round(predict.network$net.result) != test.data$spam)
cat("The final test error is : ", error_test)
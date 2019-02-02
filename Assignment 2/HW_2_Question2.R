############################################################################################################
## This code is to compute the OLS estimates and compare them with Forward,Backward,Lasso,Ridge regression 
## Boobalaganesh Ezhilan 
## Created: October04, 2018
## Edited: 
#################################################################################Importing and Merging the Data
# set the current working directory
setwd("/Users/Dell/Desktop/Data Mining codes Samples/Assignment 2")

# install some CRAN libararies 
install.packages("leaps")
install.packages("ggplot2")
install.packages("MASS")
install.packages("caret")
install.packages("lda")
install.packages('e1071', dependencies=TRUE)
install.packages("glmnet", dependencies=TRUE)

#load the packages
library(leaps)
library(ggplot2)
library(lda)
library(glmnet)

#Importing the Data
customer_train=read.delim("ticdata2000Train.txt", header = FALSE, sep = "\t", dec = ".")
customer_test=read.delim("ticeval2000Test.txt", header = FALSE, sep = "\t", dec = ".")
customer_test[,'V86']=read.delim("tictgts2000output.txt", header = FALSE, sep = "\t", dec = ".")


##################################################################################
######### Knowing My Data##########
#############################################################################
#a) Splitting the College Data set into train set and test set 
row_number.train <- nrow(customer_train)
row_number.test <- nrow(customer_test)
shuffled_cus.train <- customer_train[sample(row_number.train), ]
shuffled_cus.test <- customer_test[sample(row_number.test), ]
train_indices.train <- 1:round(1.0 * row_number.train)
cust_train <- shuffled_cus.train[train_indices.train, ]
test_indices.test <- 1:round(1.0 * row_number.test)
cust_test <- shuffled_cus.test[test_indices.test, ]

#Fitting the linear model using least squares on the training data set
fit.lm <- lm(V86 ~ ., data = cust_train)
summary(fit.lm)
pred.lm <- predict(fit.lm, cust_test)
pred.lm <- round(pred.lm, digits=0)
ols.lm <- mean((pred.lm - cust_test$V86)^2)
print(paste("MSE:",ols.lm))

#Forward Subset Selection
train_matrix <- model.matrix(V86 ~ ., data = cust_train)
test_matrix <- model.matrix(V86 ~ ., data = cust_test)
regfit.fwd <- regsubsets(V86 ~ ., data = cust_train, nvmax = 85, method = "forward")
fwd_sum <- summary(regfit.fwd)
which(fwd_sum$cp == min(fwd_sum$cp))
which(fwd_sum$bic == min(fwd_sum$bic))

#Calculating the train error for different varaibles
train.errors.fwd = c()
for (i in 1:85) {
  coefi <- coef(regfit.fwd, id = i)
  pred <- train_matrix[, names(coefi)] %*% coefi
  train.errors.fwd[i] <- mean((pred - cust_train$V86)^2)
}
print(paste("MSE: ",train.errors.fwd))
min(train.errors.fwd)
plot(train.errors.fwd, xlab = "Number of features", ylab = "Train MSE",type= 'b')

#Calculating the test error for different varaibles
test.errors.fwd = c()
for (i in 1:85) {
  coefi <- coef(regfit.fwd, id = i)
  pred <- test_matrix[, names(coefi)] %*% coefi
  test.errors.fwd[i] <- mean((pred - cust_test$V86)^2)
}
min(test.errors.fwd)
which.min(test.errors.fwd)
coef(regfit.fwd, id = which.min(test.errors.fwd))
plot(test.errors.fwd, xlab = "Number of features", ylab = "Test MSE",type= 'b')


# Backward Subset Selection
regfit.bwd <- regsubsets(V86 ~., data = cust_train, nvmax = 85, method = "backward")
bwd_sum <- summary(regfit.bwd)
which(bwd_sum$cp == min(bwd_sum$cp))
which(bwd_sum$bic == min(bwd_sum$bic))

#Calculating the test error for different varaibles
train.errors.bwd = c()
for (i in 1:85) {
  coefi <- coef(regfit.bwd, id = i)
  pred <- train_matrix[, names(coefi)] %*% coefi
  train.errors.bwd[i] <- mean((pred - cust_train$V86)^2)
}
min(train.errors.bwd)
plot(train.errors.bwd, xlab = "Number of features", ylab = "Test MSE",type= 'b')

#Calculating the test error for different varaibles
test.errors.bwd = c()
for (i in 1:85) {
  coefi <- coef(regfit.bwd, id = i)
  pred <- test_matrix[, names(coefi)] %*% coefi
  test.errors.bwd[i] <- mean((pred - cust_test$V86)^2)
}
min(test.errors.bwd)
plot(test.errors.bwd, xlab = "Number of features", ylab = "Test MSE",type= 'b')


#fitting the Ridge regression model on the training set
cust_train_matrix <- model.matrix(V86 ~ ., data = cust_train)
cust_test_matrix <- model.matrix(V86 ~ ., data = cust_test)
lambda_seq <- 10 ^ seq(4, -2, length = 100)
cross_valid.ridge <- cv.glmnet(cust_train_matrix, cust_train$V86, alpha = 0, lambda = lambda_seq)
plot(cross_valid.ridge)
lam.best.ridge <- cross_valid.ridge$lambda.min
lam.best.ridge
pred.ridge <- predict(cross_valid.ridge, cust_test_matrix, s=lam.best.ridge)
summary(pred.ridge)

#Calculating the Mean Square Loss for Ridge regression model
rmse.ridge <- mean((pred.ridge - cust_test$V86)^2)
print(paste("MSE: ", rmse.ridge, " RMSE: ", rmse.ridge))


#fitting the Lasso regression model on the training set
lambda_seq <- 10 ^ seq(4, -2, length = 100)
cross_valid.lasso <- cv.glmnet(cust_train_matrix, cust_train$V86, alpha = 1, lambda = lambda_seq)
plot(cross_valid.lasso)
lam.best.lasso <- cross_valid.lasso$lambda.min
pred.lasso <- predict(cross_valid.lasso, cust_test_matrix, s=lam.best.lasso)
summary(pred.lasso)

#Calculating the Mean Square Loss for Ridge regression model
rmse.lasso <- mean((pred.lasso - cust_test$V86)^2)
print(paste("MSE: ", rmse.lasso, " RMSE: ", rmse.ridge))


#plot for number of 0's and 1's in target variable
ggplot(cust_train, aes(x = cust_train$V86))+ geom_histogram(binwidth = 0.1)
ggplot(cust_test, aes(x = cust_test$V86))+ geom_histogram(binwidth = 0.1)

#Number of 1's in Target Varible
sum(cust_train$V86)
sum(cust_test$V86)

#probability of getting one
sum(cust_train$V86)/row_number.train
sum(cust_test$V86)/row_number.test

library(MASS)
library(caret)
sample.fit.logi <- lda(V86 ~ .,data = cust_train,na.action= "na.omit")
sample_pred.logi <- predict(sample.fit.logi,cust_test[,-86])$class
confusionMatrix(sample_pred.logi,as.factor(cust_test$V86))



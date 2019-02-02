############################################################################################################
## This code is to predict the number of applications received using the other variables in the college data set
## Boobalaganesh Ezhilan 
## Created: October04, 2018
## Edited: 
################################################################################
# set the current working directory
setwd("/Users/Dell/Desktop/Data Mining codes Samples/Assignme?nt 2")

# install some CRAN libararies 
#install.packages("ISLR")
#install.packages("glmnet", dependencies=TRUE)
#install.packages("pls")

#load the packages
library("ISLR")
library("glmnet")
library("pls")
ls("package:ISLR")

#a) Splitting the College Data set into train set and test set 
row_number <- nrow(College)
shuffled_college <- College[sample(row_number), ]
train_indices <- 1:round(0.5 * row_number)
coll_train <- shuffled_college[train_indices, ]
test_indices <- (round(0.5 * row_number) + 1):row_number
coll_test <- shuffled_college[test_indices, ]

# How many observations in the training and testing data?
nrow(coll_train)  
nrow(coll_test)

#Fitting the linear model using least squares on the training data set
fit.lm <- lm(Apps ~ ., data = coll_train)
pred.lm <- predict(fit.lm, coll_test)
set.seed(500)

#Calculating the Mean Square Loss for Linear model
rmse.lm <- sqrt(mean((pred.lm - coll_test$Apps)^2))
print(paste("MSE: ", round((rmse.lm)^2), " RMSE: ", round(rmse.lm)))


#b)fitting the Ridge regression model on the training set
coll_train_matrix <- model.matrix(Apps ~ ., data = coll_train)
coll_test_matrix <- model.matrix(Apps ~ ., data = coll_test)
lambda_seq <- 10 ^ seq(4, -2, length = 100)
cross_valid.ridge <- cv.glmnet(coll_train_matrix, coll_train$Apps, alpha = 0, lambda = lambda_seq)
plot(cross_valid.ridge)
lam.best.ridge <- cross_valid.ridge$lambda.min
lam.best.ridge
pred.ridge <- predict(cross_valid.ridge, coll_test_matrix, s=lam.best.ridge)

#Calculating the Mean Square Loss for Ridge regression model
rmse.ridge <- sqrt(mean((pred.ridge - coll_test$Apps)^2))
print(paste("MSE: ", round((rmse.ridge)^2), " RMSE: ", round(rmse.ridge)))


#d)fitting the Lasso regression model on the training set
lambda_seq <- 10 ^ seq(4, -2, length = 100)
cross_valid.lasso <- cv.glmnet(coll_train_matrix, coll_train$Apps, alpha = 1, lambda = lambda_seq)
summary(cross_valid.lasso)
plot(cross_valid.lasso)
lam.best.lasso <- cross_valid.lasso$lambda.min
pred.lasso <- predict(cross_valid.lasso, coll_test_matrix, s=lam.best.lasso)

#Calculating the Mean Square Loss for Lasso regression model
rmse.lasso <- sqrt(mean((pred.lasso - coll_test$Apps)^2))
print(paste("MSE: ", round((rmse.lasso)^2), " RMSE: ", round(rmse.lasso)))

#e)fitting the PCR model on the training set 
fit.pcr <- pcr(Apps ~ ., data = coll_train, scale = TRUE, validation = "CV")
validationplot(fit.pcr)
head(fit.pcr)
summary(fit.pcr)
validationplot(fit.pcr, val.type = "MSEP")
pred.pcr <- predict(fit.pcr, coll_test,ncomp=11)

#Calculating the Mean Square Loss for PLS model
rmse.pcr <- sqrt(mean((pred.pcr - coll_test$Apps)^2))
print(paste("MSE: ", round((rmse.pcr)^2), " RMSE: ", round(rmse.pcr)))

#f)fitting the PLS model on the training set
fit.pls <- plsr(Apps ~ ., data = coll_train, scale = TRUE, validation = "CV")
summary(fit.pls)
head(fit.pls)
validationplot(fit.pls, val.type = "MSEP")
pred.pls <- predict(fit.pls, coll_test,ncomp=14)

#Calculating the Mean Square Loss for PLS model
rmse.pls <- sqrt(mean((pred.pls - coll_test$Apps)^2))
print(paste("MSE: ", round((rmse.pls)^2), " RMSE: ", round(rmse.pls)))

#g)Different types of errors
print(paste("MSE_OLS: ", round((rmse.lm)^2), " RMSE_OLS: ", round(rmse.lm)))
print(paste("MSE_Ridge: ", round((rmse.ridge)^2), " RMSE_Ridge: ", round(rmse.ridge)))
print(paste("MSE_Lasso: ", round((rmse.lasso)^2), " RMSE_Lasso: ", round(rmse.lasso)))
print(paste("MSE_Pcr: ", round((rmse.pcr)^2), " RMSE_Pcr: ", round(rmse.pcr)))
print(paste("MSE_Pls: ", round((rmse.pls)^2), " RMSE_Pls: ", round(rmse.pls)))

#Calculate R^2 for different models
test.avg <- mean(coll_test$Apps)
lm.r2 <- 1 - mean((pred.lm - coll_test$Apps)^2) / mean((test.avg - coll_test$Apps)^2)
ridge.r2 <- 1 - mean((pred.ridge - coll_test$Apps)^2) / mean((test.avg - coll_test$Apps)^2)
lasso.r2 <- 1 - mean((pred.lasso - coll_test$Apps)^2) / mean((test.avg - coll_test$Apps)^2)
pcr.r2 <- 1 - mean((pred.pcr - coll_test$Apps)^2) / mean((test.avg - coll_test$Apps)^2)
pls.r2 <- 1 - mean((pred.pls - coll_test$Apps)^2) / mean((test.avg - coll_test$Apps)^2)

#Box plot of Test R square for five different model
barplot(c(rmse.lm, rmse.ridge, rmse.lasso, rmse.pcr, rmse.pls), col="purple", 
        names.arg=c("OLS","Ridge", "Lasso", "PCR", "PLS"), main = "Test MSE",
        ylab = "Test MSE")

#Box plot of Test R square for five different model
barplot(c(lm.r2, ridge.r2, lasso.r2, pcr.r2, pls.r2), col="red", 
        names.arg=c("OLS","Ridge", "Lasso", "PCR", "PLS"), main = "Test R squared",
        ylab = "Test R Square")
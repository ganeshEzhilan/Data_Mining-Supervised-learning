#######################################################
# This code computes AIC,BIC,boot-strap prediction error for a a bestsubset linear regression analysis
#
# Boobalaganesh Ezhilan
# Created: 20/11/2018
#######################################################
rm(list = ls())
#install.packages("ElemStatLearn")
#install.packages("leaps")
#install.packages("boot")
#install.packages("bootstrap")
install.packages("caret")


library("ElemStatLearn")
library("leaps")
require(boot)
library(MASS)
library(bootstrap)
library(boot)
library(caret)

# Load data and get a quick overview
data("prostate", package = "ElemStatLearn")
prostateData <- prostate[,-10] #97 x 10
str(prostate)

#Splitting the prostate dataset into test and train
set.seed(12345)
row_number <- nrow(prostateData)
shuffled_prostate <- prostateData[sample(row_number), ]
train_indices <- 1:round(0.5 * row_number)
pros_train <- shuffled_prostate[train_indices, ]
test_indices <- (round(0.5 * row_number) + 1):row_number
pros_test <- shuffled_prostate[test_indices, ]

#Performing linear regression on the original dataset
fit <- lm(lpsa ~ ., data = shuffled_prostate)
pred.test <- predict(fit, newdata = pros_train)
pred.train <- predict(fit, newdata = pros_test)

test.error <- (1/length(pros_test))*sum((pred.test - pros_test)^2)
train.error <- (1/length(pros_train))*sum((pred.train - pros_train)^2)
test.error
train.error

#performing best subset selection
train_matrix <- model.matrix(lpsa ~ ., data = pros_train)
test_matrix <- model.matrix(lpsa ~ ., data = pros_test)
regfit.fwd <- regsubsets(lpsa ~ ., data = pros_train, nvmax = 9, method = "exhaustive")
fwd_sum <- summary(regfit.fwd)
which(fwd_sum$cp == min(fwd_sum$cp))
which(fwd_sum$bic == min(fwd_sum$bic))
fwd_sum$cp
fwd_sum$bic

#ploting for AIC and BIC error
windows()
plot(fwd_sum$cp, type = "o", lty = 2, col = "blue" , xlab = "k", ylab = "error", main = "AIC error for different models")
windows()
plot(fwd_sum$bic, type = "o", lty = 1, col = "green", xlab = "k", ylab = "error", main = "BIC error for different models")


predict.regsubsets =function (object ,newdata ,id ,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

#10-fold cross validation
k = 10
set.seed(123)
folds = sample(1:k,nrow(prostateData),replace=TRUE)
table(folds)
cv.errors=matrix(NA,k,8, dimnames=list(NULL, paste(1:8)))
for(j in 1:k){
  best.fit = regsubsets(lpsa ~., data=prostateData[folds != j,], nvmax = 8)
  for (i in 1:8){
    pred = predict.regsubsets(best.fit, prostateData[folds == j, ], id = i)
    cv.errors[j, i] = mean((prostateData$lpsa[folds == j] - pred)^2)
  }
}
mean.cv.errors = apply(cv.errors ,2,mean)
mean.cv.errors
which(mean.cv.errors == min(mean.cv.errors))
plot(mean.cv.errors, pch=19, type="b",col = "green", main = "10 Fold Cross Validation", xlab = "k", ylab = "error")




#5-fold cross validation
k = 5
set.seed(1)
folds = sample(1:k,nrow(prostateData),replace=TRUE)
table(folds)
cv.errors=matrix(NA,k,8, dimnames=list(NULL, paste(1:8)))
for(j in 1:k){
  best.fit = regsubsets(lpsa ~., data=prostateData[folds != j,], nvmax = 8)
  
  for (i in 1:8){
    pred = predict.regsubsets(best.fit, prostateData[folds == j, ], id = i)
    cv.errors[j, i] = mean((prostateData$lpsa[folds == j] - pred)^2)
  }
}
mean.cv.errors = apply(cv.errors ,2,mean)
mean.cv.errors
which(mean.cv.errors == min(mean.cv.errors))
windows()
plot(mean.cv.errors, pch=19, type="b",col = "green", main = "5 Fold Cross Validation", xlab = "k", ylab = "error")



# create functions that feed into "bootpred"
best_sub = regsubsets(lpsa~.,data = prostateData,method="exhaustive")
reg_summary=summary(best_sub)

beta.fit <- function(X,Y){
  lsfit(X,Y)	
}
beta.predict <- function(fit, X){
  cbind(1,X)%*%fit$coef
}
sq.error <- function(Y,Yhat){
  (Y-Yhat)^2
}

# Create X and Y
X <- prostateData[,1:8]
Y <- prostateData[,9]

# Practice, WLOG lets look at a single model
select = reg_summary$outmat

# Generalize it, and search over the best possible subsets of size "k"
error_store <- c()
for (i in 1:8){
  # Pull out the model
  temp <- which(select[i,] == "*")
  res <- bootpred(X[,temp], Y, nboot = 50, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 
  error_store <- c(error_store, res[[3]])
}

error_store
which(error_store == min(error_store))
windows()
plot(error_store, type = "o", lty = 2, col = "blue" , xlab = "k", ylab = "error", main = ".632 Bootstrap")


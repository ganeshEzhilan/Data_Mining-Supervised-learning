##########################################################################################
## This code is to compare the bagging,boosting and Random forest
## Boobalaganesh Ezhilan 
## Created: October21, 2018
## Edited: 
#################################################################################
rm(list = ls())

# set the current working directory
setwd("/Users/Dell/Desktop/Data Mining codes Samples/Assignment 4")

#install some CRAN libararies 
install.packages("MASS")
install.packages("randomForest")
install.packages("gbm")

#load the packages
library(MASS)
library(randomForest)
library(gbm)

#Calculating the median for the Boston crime rate
crime_median <- median(Boston$crim)

###############################################################################
# Classification based upon median value of crime data
#####################################################################################
Boston$resp <- 0
Boston$resp[Boston$crim > crime_median] <- 1
bostonHousingData <- Boston[,-1]

###########################################################################
# Create a training and test set
###########################################################################
#set.seed(123)
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


#########################################################################
# Growing a single tree
#########################################################################
model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
boston_model <- rpart(resp~., data = Boston_train, method = "class", control = model.control)
tree_pred = predict(boston_model, Boston_test, type = "class")
mean(tree_pred != Boston_test$resp)

min_cp = which.min(boston_model$cptable[,4])
pruned_fit_dig <- prune(boston_model, cp = boston_model$cptable[min_cp,1])
tree_pred = predict(pruned_fit_dig, Boston_test, type = "class")
y_hat <- as.numeric(tree_pred)-1
mis_class.tree <-  sum(abs(y_true_test - y_hat))/length(y_hat)
mis_class.tree

plot(boston_model$cptable[,4],main = "Cp for model selection", ylab = "cp error")
summary(boston_model)

## plot the full tree and the pruned tree
x11()
plot(pruned_fit_dig, compress=T, main = "Pruned Tree")
text(pruned_fit_dig, use.n = T, all = T, cex = .8)
windows()
plot(boston_model, uniform = T, compress = T,main = "Full Tree")
text(boston_model, use.n = T, all = T, cex = 0.8)

#########################################################################
# Random forest
#########################################################################
rf.fit<- randomForest(resp~., data = Boston_train, n.tree = 10000)
varImpPlot(rf.fit , main = "Random forest")
importance(rf.fit)

y_hat <- predict(rf.fit, newdata = Boston_test, type = 'response')
y_hat <- as.numeric(y_hat) - 1
misclass1 <- sum(abs(y_true_test - y_hat))/length(y_hat)
misclass1

#########################################################################
# Bagging
#########################################################################
rf.fit<- randomForest(resp~., data = Boston_train, n.tree = 10000,mtry = 13)
varImpPlot(rf.fit ,main = "Bagging")
importance(rf.fit)

y_hat <- predict(rf.fit, newdata = Boston_test, type = 'response')
y_hat <- as.numeric(y_hat) - 1
misclass1 <- sum(abs(y_true_test - y_hat))/length(y_hat)
misclass1

#########################################################################
# Boosting
#########################################################################
Boost.train <- Boston_train
Boost.train$resp<- as.numeric(Boston_train$resp)-1
Boost.test <- Boston_test
Boost.test$resp<- as.numeric(Boston_test$resp)-1
boost.fit1 = gbm(Boost.train$resp ~., data = Boost.train, n.trees = 300, 
                 shrinkage = .1, interaction.depth = 3, distribution = "adaboost")

boost.fit2 = gbm(Boost.train$resp~., data = Boost.train, n.trees = 300, 
                 shrinkage = .6, interaction.depth = 3, distribution = "adaboost")
summary(boost.fit1,main = "Boosting for 0.1 shrinkage")
summary(boost.fit2,main = "Boosting for 0.6 shrinkage")

### Error for shrinkage 0.1
y_hat <- predict(boost.fit1, newdata = Boost.test, n.trees = 300, type = 'response')
misclass_boost <- sum(abs(y_hat- y_true_test))/length(y_true_test)
misclass_boost
### Error for shrinkage 0.6
y_hat <- predict(boost.fit2, newdata = Boost.test, n.trees = 300, type = 'response')
misclass_boost <- sum(abs(y_hat- y_true_test))/length(y_true_test)
misclass_boost

shrink <- c(.1,.4,.6,.8)
max_iter <- 1000
store_error <- c()
for(i in 1:length(shrink)){
  boost.fit <- gbm(resp~.,data = Boost.train,n.trees =max_iter,shrinkage = shrink[i],interaction.depth = 3,distribution = "adaboost")
  temp<-c()
  for(j in 1:max_iter){
    y_hat <- predict(boost.fit,newdat  =Boost.test,n.trees = j,type= "response")
    misclass_boost <- sum(abs(y_true_test-y_hat))/length(y_true_test)
    temp <- c(temp,misclass_boost)
  }
  store_error<- cbind(store_error,temp)
}
colnames(store_error) <- paste("shrinkage",shrink,sep = ":")

windows()
plot(store_error[,1],main = "Error Profiles",ylab = "error",xlab = "boosting",)
lines(store_error[,2],col="red")
lines(store_error[,3],col="blue")
lines(store_error[,4],col="green")
legend(750, 0.4, legend=c("0.1 shrikage", "0.4 shrikage","0.6 shrikage","0.8 shrikage"),
       col=c("black","red","blue","green"),lty=1:1, cex=0.8)

windows()
pairs(bostonHousingData,col = "purple" )



############################################################################################################
## This code is to Analyze training error decreases as the feature increase while test data  
## Boobalaganesh Ezhilan 
## Created: October04, 2018
## Edited: 
#################################################################################

# install some CRAN libararies 
#install.packages("leaps")

#load the packages
#library(leaps)

#creating the 1000 X 20  Matrix random matrix 
random.data <- data.frame(replicate(20,sample(rnorm(1000*20),1000,rep=TRUE)))
random.matrix<- as.matrix(random.data)
beta <- rnorm(20)
beta.zeros<- sample(1:20,5)
beta[beta.zeros]<- 0 
epsilon <- rnorm(1000)
y <- as.vector(random.matrix %*% beta + epsilon)

#a) Splitting the College Data set into train set and test set 
train <- sample(seq(1000), 100, replace = FALSE)
test <- -train
random.train <- random.matrix[train, ]
random.test <- random.matrix[test, ]
predict.train <- y[train]
predict.test <- y[test]

#column binding the random matrix and output variable y
data.train=cbind(data.frame(y=predict.train),random.train)
data.test=cbind(data.frame(y=predict.test),random.test)

#performing the best subset selection
regfit.best <- regsubsets(y~., data = data.train, nvmax = 20)
train_matrix <- model.matrix(y ~ ., data = data.train)
test_matrix <- model.matrix(y ~ ., data = data.test)
my_sum <- summary(regfit.best)

par(mfrow = c(2,2))
plot(my_sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(my_sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
plot(my_sum$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(my_sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")

#They both agree model with 14 variables is the best.
which(my_sum$cp == min(my_sum$cp))
which(my_sum$bic == min(my_sum$bic))

#Calculate the training error for each predictors
training_errors <-c()
for (columns in 1:20) {
  train.coefi <- coefficients(regfit.best, id = columns)
  pred <- train_matrix[, names(train.coefi)] %*% train.coefi              
  training_errors[columns] <- mean((pred - predict.train)^2)
}
print(paste("Training Error: ",training_errors))
plot(training_errors, xlab = "Number of features", ylab = "Training MSE",type = 'b')


#Calculate the test errors for each predictor
test_errors <-c()
for (columns in 1:20) {
  test.coefi <- coefficients(regfit.best, id = columns)
  pred <- test_matrix[, names(test.coefi)] %*% test.coefi                                
  test_errors[columns] <- mean((pred - predict.test)^2)
}

#plot the Test mean square error with number of predictors
plot(test_errors, xlab = "Number of features", ylab = "Test MSE",type= 'b')
print(paste("Test Error: ",test_errors))

#Finding Model size for Minimum MSE in test set
which.min(test_errors)
which.min(training_errors)
print(paste("Minimum Test Error: ",which.min(test_errors)))

#Finding the coefficient value for best Model Size
coef(regfit.best, id = which.min(training_errors))
print(paste("Coefficients for best model: ",test_errors))
beta[]


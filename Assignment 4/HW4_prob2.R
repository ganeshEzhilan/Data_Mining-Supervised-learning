#######################################################
# This code computes AIC,BIC,boot-strap prediction error for a a bestsubset linear regression analysis
#
# Boobalaganesh Ezhilan
# Created: 20/11/2018
#######################################################
install.packages("rpart")
install.packages("caret")
install.packages("rpart.plot")

#library(tree)
library("rpart")
library(MASS)
library("caret")
library(rpart.plot)

setwd("/Users/Dell/Desktop/Data Mining codes Samples/Assignment 4")
wine_data = read.csv('wine.data.txt',header = FALSE)
#seed(123)
names(wine_data) <- c('Type','Alcohol','Malic','Ash','Alcalinity','Magnesium','Phenols','Flavanoids','Nonflavanoids','Proanthocyanins','Color','Hue','Dilution','Proline')
wine_train = sample(1:nrow(wine_data), nrow(wine_data)*0.80)
wine_test = -wine_train
wine_train_data = wine_data[wine_train, ]
wine_test_data = wine_data[wine_test, ]

model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
wine_model <- rpart(Type~., data = wine_train_data, method = "class", control = model.control)
tree_pred = predict(wine_model, wine_test_data, type = "class")
mean(tree_pred != wine_test_data$Type)

table(predict(wine_model, wine_train_data, type = "class"),wine_train_data[,"Type"])


x11()
plot(wine_model$cptable[,4], main = "Cp for model selection", ylab = "cv error")

min_cp = which.min(wine_model$cptable[,4])
pruned_fit_dig <- prune(wine_model, cp = wine_model$cptable[min_cp,1])
tree_pred = predict(pruned_fit_dig, wine_train_data, type = "class")
mean(tree_pred != wine_train_data$Type)
tree_pred = predict(pruned_fit_dig, wine_test_data, type = "class")
mean(tree_pred != wine_test_data$Type)

## plot the full tree and the pruned tree
windows()
rpart.plot(wine_model,main = "Full Tree")
windows()
rpart.plot(pruned_fit_dig,main = "Pruned Tree")


table(predict(wine_model, wine_train_data, type = "class"),wine_train_data[,"Type"])
summary(wine_model)

#confusion matrix
table_mat <- table(wine_test_data$Type, tree_pred)
table_mat

#Which row in which sample
wine_model$where
trainingnodes <- rownames(wine_model$frame)[wine_model$where]
trainingnodes

testresults <- predict(wine_model, wine_test_data, type = "matrix")
testresults <- data.frame(testresults)
names(testresults) <- c("ClassGuess","NofClass1onNode", "NofClass2onNode",
                        "NofClass3onNode", "PClass1", "PClass2", "PClass2")
unique(testresults[,2:4])

#calculating Training and testing sample of each node for pruned tree
# node - 1
nrow(subset(wine_train_data,Color<2.31))
nrow(subset(wine_test_data,Color<2.31))
# node - 2
nrow(subset(wine_train_data,Color>2.31 & Flavanoids>=1.8&Proline>=679))
nrow(subset(wine_test_data,Color>2.31 & Flavanoids>=1.8&Proline>=679))
# node - 3
nrow(subset(wine_train_data,Color>2.31 & Flavanoids>=1.8&Proline<679))
nrow(subset(wine_test_data,Color>2.31 & Flavanoids>=1.8&Proline<679))
# node - 4
nrow(subset(wine_train_data,Color>2.31 & Flavanoids<1.8))
nrow(subset(wine_test_data,Color>2.31 & Flavanoids<1.8))

############################################################################################################
## This code is to perform multiple regression on the preprocessed dataset
## Boobalaganesh Ezhilan 
## Created: September09, 2018
## Edited: 
#######################################################################################################
#Importing and Merging the Data
setwd("/Users/Dell/Desktop/Data Mining codes Samples/Assignment 1")

#Loading the Pre-Processed Data from Question_1
load("C:/Users/Dell/Desktop/Data Mining codes Samples/Assignment 1/Final_Student_Data.Rda") 

#Performing Single linear Regression on Dataset which is cleaned
lim1 <- lm(G1~ studytime,data=StudentData)
summary(lim1)
lim1$coefficients
cor(StudentData$G1,StudentData$studytime)
plot(lim1)

#Performing Multiple linear Regression on Dataset which is cleaned
fit <- lm(G1~.,data = StudentData)
summary(fit)

#plot the linear regression line
plot(fit)

#predict confident level
confint(fit)

#plot across different corelation factors
windows()
avPlots(fit,id.n=10,id.cex=3)


#Relation ship between * and :
fit1 <- lm(G1~ .:health,data=StudentData)
summary(fit1)

Fjobteacher

fit2 <- lm(G1~ .:higher,data=StudentData)
summary(fit2)
fit3 <- lm(G1~ .*higher,data=StudentData)
summary(fit3)




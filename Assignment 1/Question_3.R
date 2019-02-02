############################################################################################################
## This code is to build a predictive analysis for Boston Housing Data
## Boobalaganesh Ezhilan 
## Created: September09, 2018
## Edited: 
#######################################################################################################

# set the current working directory
setwd("/Users/Dell/Desktop/Data Mining codes Samples/Assignment 1")

# install some CRAN libararies 
install.packages("MASS")
install.packages("ggplot2")

#load the packages
library(MASS)
library(ggplot2)
library(gridExtra)

# a)pairwise scatter plot of predictors
##################################################################################
######### Pair wise scatter plots ##########
#############################################################################
bostonHousingData <- Boston
windows()
pairs(bostonHousingData[,1:14],col="purple")
window()
pairs(bostonHousingData[,1:5],col="purple")
windows()
pairs(bostonHousingData[,6:10],col="purple")
windows()
pairs(bostonHousingData[,11:14],col="purple")

##################################################################################
######### Scatter plot between single variable ##########
#############################################################################

#calculating the corelation matrix
bostonCorelationMatrix <- cor(bostonHousingData)
round(bostonCorelationMatrix, 2)

#plot scatter plot between variables with corelation value

#calculate corelation value for rad and tax
radAndTax <- with(subset(bostonHousingData),(cor.test(rad,tax)))
radAndTax
ggplot(bostonHousingData, aes(x=rad, y=tax)) + geom_point()+coord_cartesian(xlim=c(1,8))+ geom_smooth(method = "lm", se = FALSE)

#calculate corelation value for lstat and medv
lstatAndmedv <- with(subset(bostonHousingData),(cor.test(lstat,medv)))
lstatAndmedv
ggplot(bostonHousingData, aes(x=lstat, y=medv)) + geom_point()+geom_smooth(method = "lm", se = FALSE)


# b)predictors associated with per capita crime rate

#calculate corelation value for crime rate
crime_data <- as.data.frame(cor(bostonHousingData))
corelation_crime_rate<-crime_data[1,]
corelation_crime_rate
crimAndRad <- with(subset(bostonHousingData),(cor.test(crim,rad)))
windows()
ggplot(bostonHousingData, aes(x=crim, y=rad))+geom_point()+coord_cartesian(xlim=c(1,25),ylim=c(0,25))+geom_smooth(method = "lm", se = FALSE)
crimAndTax <- with(subset(bostonHousingData),(cor.test(crim,tax)))
windows()
ggplot(bostonHousingData, aes(x=crim, y=tax))+geom_point()+coord_cartesian(xlim=c(0,25),ylim=c(0,750))+geom_smooth(method = "lm", se = FALSE)

# c)suburbs related to high crime rate
range(bostonHousingData["crim"])
summary(bostonHousingData["crim"])
meanCrimeRate<-mean(bostonHousingData$crim)
meanHighCrimeRateSuburbs <- subset(bostonHousingData,crim>meanCrimeRate)
medianCrimeRate <- median(bostonHousingData$crim)
medianHighCrimeRateSuburbs <- subset(bostonHousingData,crim>medianCrimeRate)
subset(bostonHousingData,crim>=30)

#plot histogram to show suburbs which have high crime rate
windows()
duration = bostonHousingData$crim 
stem(duration) 
windows()
ggplot(data=bostonHousingData, aes(x=crim))+geom_histogram(binwidth=5)+ labs(title="Crime rate in subUrbs", x="Crime rate", y="Suburbs")+geom_vline(aes(xintercept=mean(crim)),color="blue", size=1)+geom_vline(aes(xintercept=median(crim)),color="red", size=1)

#suburbs related to high tax rate
range(bostonHousingData["tax"])
summary(bostonHousingData["tax"])
subset(bostonHousingData,tax>666)
meanTaxRate<-mean(bostonHousingData$tax)
meanHighTaxRateSuburbs <- subset(bostonHousingData,tax>meanTaxRate)
medianTaxRate <- median(bostonHousingData$tax)
medianHighTaxRateSuburbs <- subset(bostonHousingData,tax>medianTaxRate)

#plot histogram to show suburbs which have high tax rate
windows()
duration2 = bostonHousingData$tax 
stem(duration) 
windows()
ggplot(data=bostonHousingData, aes(x=tax))+geom_histogram(binwidth=8)+ labs(title="Tax rate in subUrbs", x="Tax rate", y="Suburbs")+geom_vline(aes(xintercept=mean(tax)),color="blue", size=1)+geom_vline(aes(xintercept=median(tax)),color="red", size=1)

#suburbs related to pupil-teacher ratio
range(bostonHousingData["ptratio"])
summary(bostonHousingData["ptratio"])
meanptRate<-mean(bostonHousingData$ptratio)
meanHighptRateSuburbs <- subset(bostonHousingData,ptratio>meanptRate)
medianptRate <- median(bostonHousingData$ptratio)
medianHighptRateSuburbs <- subset(bostonHousingData,ptratio>medianptRate)
subset(bostonHousingData,ptratio>21.0)

#plot histogram to show suburbs which have high tax rate
windows()
duration3 = bostonHousingData$ptratio 
stem(duration) 
windows()
ggplot(data=bostonHousingData, aes(x=ptratio))+geom_histogram(binwidth=0.5)+ labs(title="ptratio rate in subUrbs", x="ptratio", y="Suburbs")+geom_vline(aes(xintercept=mean(ptratio)),color="blue", size=1)+geom_vline(aes(xintercept=median(ptratio)),color="red", size=1)

#d)suburbs average more than seven rooms and average greater than eight dwelling
averageDwellingGreaterThanSeven<-subset(bostonHousingData,rm>=7)
averageDwellingGreaterThanSeven
averageDwellingGreaterThanEight<-subset(bostonHousingData,rm>=8)
averageDwellingGreaterThanEight
windows()
pairs(averageDwellingGreaterThanSeven,col="purple")
windows()
pairs(averageDwellingGreaterThanEight,col="purple")

#calculating corelation matrix for avearage dwelling greater than Seven
averageDwellingGreaterThanSevenMatrix<- cor(averageDwellingGreaterThanSeven)
round(averageDwellingGreaterThanSevenMatrix, 2)

#calculating corelation matrix for avearage dwelling greater than Eight
averageDwellingGreaterThanEightMatrix<- cor(averageDwellingGreaterThanEight)
round(averageDwellingGreaterThanEightMatrix, 2)

#ploting realtion between predictors when average dwelling is than Eight
windows()
plot1 <- ggplot(averageDwellingGreaterThanEight, aes(x=crim, y=tax))+geom_point()+geom_smooth(method = "lm", se = FALSE)
plot2 <- ggplot(averageDwellingGreaterThanEight, aes(x=crim, y=black))+geom_point()+geom_smooth(method = "lm", se = FALSE)
plot3 <- ggplot(averageDwellingGreaterThanEight, aes(x=rad, y=tax))+geom_point()+geom_smooth(method = "lm", se = FALSE)
grid.arrange(plot1,plot2,plot3,ncol=3)





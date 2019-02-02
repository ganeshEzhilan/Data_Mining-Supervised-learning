############################################################################################################
## This code is to build a predictive model for First period Grades Using EDA on Student Performance Data set
## Boobalaganesh Ezhilan 
## Created: September09, 2018
## Edited: 
#######################################################################################################
# set the current working directory
setwd("/Users/Dell/Desktop/Data Mining codes Samples/Assignment 1")


# install some CRAN libararies 
install.packages("car")
install.packages("ggplot2")
install.packages("gridExtra")


#load the packages
library("car")
library("ggplot2")
library("gridExtra")

#Importing and Merging the Data
MathStudentData <- read.table("student-mat.csv",sep=";",header=TRUE)
PortStudentData <- read.table("student-por.csv",sep=";",header=TRUE)
StudentData=merge(MathStudentData,PortStudentData,all="true")

#knowing my Data
dim(StudentData)
str(StudentData)
head(StudentData[1:5])
summary(StudentData)

#subsetting the data
StudentData <- StudentData[,-c(5,11,12,17,18,19,20,23,24,25,27,28,32,33)]
head(StudentData[1:5])  
summary(StudentData)  

#Analysing the data
windows()
ggplot(StudentData, aes(x=Pstatus, y=G1)) + geom_boxplot()+labs(title="Parent Status vs Grades", x="Pstatus", y="First Period grade")
StudentData <- StudentData[,-c(5)]

#Glimpse of my data in Histogram
windows()
Plot1 <- ggplot(data=StudentData, aes(x=G1))+geom_histogram(binwidth= 1,fill="blue",color = "black" ,alpha=0.5, position="identity") +geom_vline(aes(xintercept=mean(G1)),color="white",linetype="dashed", size=0.8)+geom_vline(aes(xintercept=median(G1)),color="green",linetype="dashed", size=0.8)+
labs(title="Histogram of First Period Grade", x="First Period Grade", y="Count")
plot2 <-ggplot(data = StudentData, aes(x = "", y = G1)) +geom_boxplot()+coord_cartesian(xlim = c(0,2))+labs(title="Box plot of First Period Grade", x="First Period Grade", y="Count")
grid.arrange(Plot1,plot2,ncol=2)

#Removing outliers in G1
firstGradeCol = StudentData[, "G1"]
ValidFirstPeriodGrade = firstGradeCol[!firstGradeCol %in% boxplot.stats(firstGradeCol)$out]    #https://stackoverflow.com/
StudentData<-subset(StudentData,G1!=0)

#Removing outliers for age
ggplot(data = StudentData, aes(x = "", y = age)) +geom_boxplot()+coord_cartesian(xlim = c(0,2))
ageCol = StudentData[, "age"]
validAgeCOl = ageCol[!ageCol %in% boxplot.stats(ageCol)$out] 
print(validAgeCOl)
StudentData<-subset(StudentData,age<21)

str(StudentData)

#Variable transformation
str(StudentData)
#function to transform Factor into Character variable
chracterVar <- function (StudentData) {
  for(i in 1:ncol((StudentData))) {                            #https://rstudio-pubs-static.s3.amazonaws.com/
    if (is.factor(StudentData[,i]) == TRUE) {
      StudentData[,i] <- as.character(StudentData[,i])
    }
  }
  return(StudentData)
}
str(StudentData)

#function to transform character string into binary variable
BinaryVar <- function (StudentData) {                     #https://rstudio-pubs-static.s3.amazonaws.com/
  for(i in 1:(nrow(StudentData))) {
    for(j in 1:ncol((StudentData))) {
      if (is.character(StudentData[,j]) ==TRUE) {
        if (StudentData[i,j] == "yes") {
          StudentData[i,j] <- 1
        }
        if (StudentData[i,j] == "no") {
          StudentData[i,j] <- 0
        }
      }
    }
  }
  return(StudentData)
}

StudentData <- chracterVar(StudentData)
StudentData <- BinaryVar(StudentData)
StudentData$internet <- as.numeric(as.character(StudentData$internet))
StudentData$higher <- as.numeric(as.character(StudentData$higher))
StudentData$schoolsup <- as.numeric(as.character(StudentData$schools))
str(StudentData)

#Relation between FirstPeriodGrades and Sex
aggregate(G1~sex,StudentData,mean)
windows()
ggplot(StudentData, aes(x=sex, y=G1,color = sex)) + geom_boxplot()+coord_cartesian(ylim=c(6,17))+labs(title="First Period grade vs Sex", x="Sex", y="First Period Grade")



#Relation between FirstPeriodGrades and age
cor(StudentData$G1,StudentData$age)
cor.test(StudentData$G1,StudentData$age)
aggregate(G1~age,StudentData,median)
windows()
ggplot(StudentData, aes(x=sex, y=G1,color = age)) + geom_boxplot()
StudentData = StudentData[,-c(3)]

#Relation between FirstPeriodGrades and father and mother education
cor(StudentData$G1,StudentData$Medu)
cor(StudentData$G1,StudentData$Fedu)
aggregate(G1~Fedu,StudentData,mean)
aggregate(G1~Medu,StudentData,mean)
StudentData = StudentData[,-c(5)]

#Relation between FirstPeriodGrades and Internet
aggregate(G1~internet,StudentData,mean)
StudentData = StudentData[,-c(12)]

#Relation between FirstPeriodGrades and absences
cor(StudentData$G1,StudentData$absences)
windows()
ggplot(StudentData, aes(x=G1, y=absences))+geom_point()+geom_smooth(method = "lm", se = FALSE)
cor.test(StudentData$G1,StudentData$absences)
aggregate(G1~absences,StudentData,median)
lm(G1~absences,StudentData)
StudentData = StudentData[,-c(14)]

#Relation between travel time and Grades
windows()
boxFatherJob <- with(StudentData, factor(traveltime, levels = c(1,2,3,4)))
with(StudentData, boxplot(G1 ~ boxFatherJob))
aggregate(G1~traveltime,StudentData,mean)
StudentData = StudentData[,-c(7)]

#Relation between Mothers Job and Grades
windows()
boxMotherJob <- with(StudentData, factor(Mjob, levels = c("at_home", "other", "teacher", "services", "health")))
with(StudentData, boxplot(G1 ~ boxMotherJob))
summary(with(StudentData, aov(G1 ~ boxMotherJob)))


#Relation between Fathers Job and Grades
windows()
boxFatherJob <- with(StudentData, factor(Fjob, levels = c("at_home", "other", "teacher", "services", "health")))
with(StudentData, boxplot(G1 ~ boxFatherJob),color=boxFatherJob)
summary(with(StudentData, aov(G1 ~ boxFatherJob)))


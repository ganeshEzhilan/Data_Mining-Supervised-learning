##########################################################################################
## This code is to carry out principle componenet analysis
## Boobalaganesh Ezhilan 
## Created: December16, 2018
## Edited: 
#################################################################################
rm(list = ls())
install.packages("alr3")
library(alr3)

data("banknote")
View(banknote)
swissbank=banknote

head(swissbank[,-7])
pairs(swissbank[,-7], col = swissbank$Y, upper.panel = NULL, pch = 16, cex = 0.5)

## PCA for all 200 notes
swissbank.pca <- princomp(swissbank[,-7]) 
summary(swissbank.pca) # summary givs the eigenvalues of covariance matrix 
loadings(swissbank.pca)# loadings shows the eigenvectors


swissbank.pc <- predict(swissbank.pca)# here predict() computes data in new coordinates
swissbank.true <- factor(c(rep("t",100),rep("f",100)))
swissbank.pc[,1]

#distinguishing between true and false banknotes using the first two ORIGINAL components
windows()
plot(swissbank[,1],swissbank[,2],type="n",xlab="first feature", ylab= "second feature") 
text(swissbank, labels=as.character(swissbank.true),col=as.integer(swissbank.true))


#distinguishing between true and false banknotes using the first two PRINCIPLE components
windows()
plot(swissbank.pc[,1],swissbank.pc[,2],type='n',xlab="first princ. comp.", ylab= "second princ. comp.",main = "All 200 notes") 
text(swissbank.pc, labels=as.character(swissbank.true),col=as.integer(swissbank.true))  


## separating 100 genuine notes
bank_genuine<-swissbank[which(swissbank$Y==0),]   ## in the help it says 0 is genuine and 1 is counterfeit
bank_gen.pca <- princomp(bank_genuine[,-7]) 
summary(bank_gen.pca) 
loadings(bank_gen.pca)
bank_gen.pc <- predict(bank_gen.pca,swissbank[,-7])

windows()
plot(bank_gen.pc[,1],bank_gen.pc[,2],type='n',xlab="first princ. comp.", ylab= "second princ. comp.",main = "Componenets built using genuine notes") 
text(bank_gen.pc, labels=as.character(swissbank.true),col=as.integer(swissbank.true))  


## separating 100  counterfeit notes
bank_counter<-swissbank[which(swissbank$Y==1),]   
bank_counter_.pca <- princomp(bank_counter[,-7]) 
summary(bank_counter_.pca) 
loadings(bank_counter_.pca)
bank_counter_.pc <- predict(bank_counter_.pca)

windows()
plot(bank_counter_.pc[,1],bank_counter_.pc[,2],xlab="first princ. comp.", ylab= "second princ. comp.",main = "100 counterfeit notes") 

















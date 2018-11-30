install.packages(c("RCurl","VIM","caret"))
library(caret)
library(RCurl)
library(MASS)
set.seed(1)
abalone<-read.csv("/home/campus21/rjpena/Desktop/MA5790-master/Datasets/Abalone/abalone.csv", header=T)

names(abalone)[1] <- 'Sex'
summary(abalone)


#class<-abalone$Rings
#predictors<-abalone[,-9]
dmy<-dummyVars(~., data=abalone,fullRank = T)
transform<- data.frame(predict(dmy, newdata=abalone))
abalone<-transform

head(abalone)

abalone$Rings=as.factor(abalone$Rings)
str(abalone$Rings)

drop<-c(1,2,25,26,29)
#abalone<-abalone[abalone$Rings[-drop]]
abalone$Rings

data<-createDataPartition(y=abalone$Rings, times=10, p=.80,list=F)

zeroVar <- nearZeroVar(abalone)
zeroVar






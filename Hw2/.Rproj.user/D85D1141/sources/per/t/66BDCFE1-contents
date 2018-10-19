#4.1
install.packages(c("caret","AppliedPredictiveModeling"))
require(RCurl)
music<-read.csv(text=getURL("https://raw.githubusercontent.com/rjpena/MA5790/master/Datasets/Genres/genresTrain.csv"), header=T)
head(music)
#save counts of Genres in a table 
counts = table(music$GENRE)
counts
#create barplot for freqs of classes
barplot(counts,ylab = "Frequency",xlab = "Genre", main="Music Genres",ylim=range(pretty(c(0, counts))))


library(caret)
set.seed(1)
#this dataset is large enough for 10-Fold Cross-Validation
crossval<-createFolds(music$GENRE,k=10,returnTrain=T,list=T)
#createDataPartitions default method uses stratification to handle class distributions. This sampling takes roughly 70% of each class for splitting. 
train.index <- createDataPartition(music$GENRE, p = .7, list = T)
train <- music[ train.index,]
test  <- music[-train.index,]
#display training partition
dim(train)
counts_train = table(train$GENRE)
counts_train
barplot(counts_train,ylab = "Frequency",xlab = "Genre",main="Training",ylim=range(pretty(c(0, counts_train))))
# display test partition
dim(test)
counts_test = table(test$GENRE)
counts_test
barplot(counts_test,ylab = "Frequency",xlab = "Genre", main="Testing",ylim=range(pretty(c(0, counts_test))))


#4.2
library(AppliedPredictiveModeling)
data(permeability)
head(permeability)
hist(permeability)
summary(apply(fingerprints, 2, mean))

testing <- scale(permeability)
set.seed(11)
#Do repeated cross-validation 10 folds, 20 times
repeatedCV<-createMultiFolds(permeability,k=10,times=20)

#4.3
install.packages("e1071")
library(e1071)
set.seed(111)
data(ChemicalManufacturingProcess)
head(ChemicalManufacturingProcess)
# Shows that the dimensionality of this data set needs repeated CV. Small sample size, many predictors.
dim(ChemicalManufacturingProcess)
#Many different scales(processes and materials). Needs to be centered and scaled
summary(ChemicalManufacturingProcess)
#had to excluse NA's
plsChem <- train(Yield ~ ., data=ChemicalManufacturingProcess, na.action=na.exclude, method="pls", preProc=c("center","scale"), tuneLength=10, trControl=trainControl(method="repeatedcv", repeats=5))
plsChem
#plot the avg performance
plot(plsChem, scales=list(x=list(log=2)))
#pull r2 vals from results
r2Vals<-plsChem$results[,c("ncomp","Rsquared","RsquaredSD")] 
r2Vals
#R2 Structural Equation Model
r2Vals$SEM<-r2Vals$RsquaredSD/sqrt(length(plsChem$control$index))
r2Vals
#find the best r2 based on the maximum RSquared value in the set
r2Opt<- subset(r2Vals,ncomp==which.max(r2Vals$Rsquared))
r2Opt
#lower bound of best r2
r2OptLB<-r2Opt$Rsquared - r2Opt$SEM
r2OptLB
#next best r2
r2NB<-subset(r2Vals,Rsquared >= r2OptLB & ncomp < r2Opt$ncom)
r2NB#NONE RETURNED
#calculate the tolerances and plot to find ~10%
tols<-(r2Vals$Rsquared - r2Opt$Rsquared)/r2Opt$Rsquared * 100
tols
plot(r2Vals$ncomp, tols, data=r2Vals)

#4.4
install.packages("descr")
library(descr)
set.seed(1111)
data(oil)
str(oilType)
length(oilType)
table(oilType)
freq(oilType)
#create Samples
sample1<-sample(oilType, 60)
sample2<-sample(oilType, 60)
sample3<-sample(oilType, 60)
sample4<-sample(oilType, 60)
sample5<-sample(oilType, 60)
#check frequencies
freq(sample1,plot=F)
freq(sample2,plot=F)
freq(sample3,plot=F)
freq(sample4,plot=F)
freq(sample5,plot=F)

#createDataPartition with stratified
samp<-createDataPartition(oilType,p=.6,5)
samp<-lapply(samp, function(x,y)table(y[x]),y=oilType)
head(samp,5)
freq(samp$Resample1)
freq(samp$Resample2)

#4.1
require(RCurl)
music<-read.csv(text=getURL("https://raw.githubusercontent.com/rjpena/MA5790/master/Datasets/Genres/genresTrain.csv"), header=T)
head(music)
#save counts of Genres in a table 
counts = table(music$GENRE)
counts
#create barplot for freqs of classes
barplot(counts,ylab = "Frequency",xlab = "Genre", main="Music Genres")

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
barplot(counts_train,ylab = "Frequency",xlab = "Genre",main="Training")
# display test partition
dim(test)
counts_test = table(test$GENRE)
barplot(counts_test,ylab = "Frequency",xlab = "Genre", main="Testing")


#4.2
data(permeability)
head(permeability)
hist(permeability)
summary(apply(fingerprints, 2, mean))

testing <- scale(pearmeability)
set.seed(1)
#Take a random sample of datapoints

#4.3







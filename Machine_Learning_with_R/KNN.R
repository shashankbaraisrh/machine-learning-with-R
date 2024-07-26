#1 Getting the data from the famous iris data set for this project.
install.packages('ISLR')
library(ISLR)
head(iris)

head(str(data.frame(iris))) 
var(iris[,1])
var(iris[,])
#2 Standardize Data
standardized.feature<-scale(iris[1:4]) 

#3Check that the scaling worked by checking the variance of one of the new columns
var(standardized.feature[,1])
var(standardized.feature[,2]) 

#4Join the standardized data with the response/target/label column (the column with the species names.
final.data<-cbind(standardized.feature,iris[5])
head(final.data) 

#5Use the caTools library to split your standardized data into train and test sets. Use a 70/30 split.
library(caTools)
set.seed(101)
sample<-sample.split(final.data$Species, SplitRatio = 0.70)
train<-subset(final.data,sample==TRUE)
test<-subset(final.data, sample==FALSE) 


#6Build a KNN model.
library(class) 

#7Use the knn function to predict Species of the test set. Use k=1
predicted.species<-knn(train[1:4],test[1:4],train$Species, k=1)
predicted.species 

#8checking to see the misclassification rate?
mean(test$Species!=predicted.species) 

#9Create a plot of the error (misclassification) rate for k values ranging from 1 to 10.
predicted.species<-NULL
error.rate<-NULL
for(i in 1:10)
  {
  set.seed(101)
  predicted.species<-knn(train[1:4],test[1:4],train$Species, k=i)
  error.rate[i]<-mean(test$Species!=predicted.species)
} 

library(ggplot2)
k.values<-1:10
error.df<-data.frame(error.rate,k.values)
pl<-ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()
pl + geom_line(lty="dotted",color='blue')



#e1071
#1
# Call the ISLR library and check the 
# head of College (a built-in data frame with ISLR, use data() to check this.) 
# Then reassign College to a dataframe called df
library(ISLR)

install.packages("tidyverse")
install.packages("rpart")
install.packages("rpart.plot")

library(tidyverse)
library(caTools)
library(rpart)
library(rpart.plot)

head(College)
df<-College

#2
#Create a scatterplot of Grad.Rate versus Room.Board, colored by the Private column.
df %>% ggplot(aes(x=Grad.Rate,y=Room.Board,color=Private))+geom_point()

#3
#Create a histogram of full time undergrad students, color by Private.
df %>% ggplot(aes(x=F.Undergrad,fill=Private))+geom_histogram(bins = 50,color="black",position=position_stack(reverse=TRUE))

#4
#Create a histogram of Grad.Rate colored by Private. You should see something odd here.
df %>% ggplot(aes(x=Grad.Rate,fill=Private))+geom_histogram(bins = 50,color="black",position=position_stack(reverse=TRUE))

#5
#What college had a Graduation Rate of above 100% ?
row.names(df[df$Grad.Rate>100,])  

#6
#Change that college's grad rate to 100%.
df["Cazenovia College","Grad.Rate"] <- 100

#7
#Split your data into training and testing sets 70/30. Use the caTools library to do this.
seed <- 103
set.seed(seed)
sample <- sample.split(df$Private, SplitRatio = 0.70)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

#8
# Decision Tree:
#   
#   Use the rpart library to build a decision tree to predict whether or 
# not a school is Private. Remember to only build your tree off the training data
library(rpart)
help("rpart")
tree <- rpart(Private ~ ., method = "class", data=train)


#9
# Use predict() to predict the Private label on the test data.
help("predict")
predicted.type <- predict(tree, test)[, "Yes"]


#10
# Check the Head of the predicted values. You should notice that you actually have two columns with the probabilities.
head(predicted.type)

#11
# Turn these two columns into one column to match the original Yes/No Label for a Private column.
test$predictedPrivateType <- ifelse(predicted.type>0.5,'Yes','No')


#12
# Now use table() to create a confusion matrix of your tree model.
table(test$Private, test$predictedPrivateType)

#13
# Use the rpart.plot library and the prp() function to plot out your tree model.
library(rpart.plot)
prp(tree)


#14
# Random Forest:
#   
#   Call the randomForest package library.
# 
# Now use randomForest() to build out a model to predict Private class. Add importance=TRUE as a parameter in the model. (Use help(randomForest) to find out what this does.
#                                                                                                                         
library(randomForest)
help(randomForest)
set.seed(101)
rf.df <- randomForest(Private ~ ., train, importance=TRUE)                                                                                                                        


#15
# What was your model's confusion matrix on its own training set? Use model$confusion
rf.df$confusion

#16
# Grab the feature importance with model$importance. Refer to the reading for more info on what Gini[1] means.[2]
rf.df$importance

#17
# Predictions:
#   
#   Now use your random forest model to predict on your test set
# 
test$rf <- predict(rf.df, test)
table(test$Private, test$rf)












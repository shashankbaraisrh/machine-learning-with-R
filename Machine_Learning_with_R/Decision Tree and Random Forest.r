
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
df
#2
#Create a scatterplot of Grad.Rate versus Room.Board, colored by the Private column.
install.packages('ggplot2')
library(ggplot2)

p1<-ggplot(df, aes(x = Grad.Rate, )) + geom_point(aes(y = Room.Board, color = "Private"), size = 3)+labs(title = "Grad.Rate versus Room.Board Columns",
                                                                                                         x = "Graduation Rate",
                                                                                                         y = "Room.Board")
p1





library(ggplot2)
p1<-ggplot(data=df, aes(x=Grad.Rate, y=Room.Board))+geom_point(aes(color=Private))+labs(y='Room Board', x='Grad rate')

p1
# Plot
p<-ggplot(College, aes(x = Room.Board, y = Grad.Rate, color = Private)) +
  geom_point() +
  labs(x = "Room & Board", y = "Graduation Rate") +
  ggtitle("Scatterplot of Graduation Rate vs Room & Board, colored by Private Status")
p


























df %>% ggplot(aes(x=Grad.Rate,y=Room.Board,color=Private))+geom_point()

#3
#Create a histogram of full time undergrad students, color by Private.
library('ggplot2')
library(ISLR)
data(college)
head(df)

p1<-ggplot(data<-df, aes(x=F.Undergrad))+ geom_histogram(aes(color=Private))+labs(x='F Undergrad', y = 'Frequency', title = 'frequency dost')
p1



library(ggplot2)
p1<-ggplot(data=df, aes(x=F.Undergrad))+geom_histogram(aes(color=Private),binwidth=100, alpha=0.2)
p1


ggplot(College, aes(x = F.Undergrad, fill = Private)) +
  geom_histogram(binwidth = 500, position = "identity", alpha = 0.8) +
  labs(x = "Full-time Undergraduate Students", y = "Frequency") +
  ggtitle("Histogram of Full-time Undergraduate Students, colored by Private Status") 














df<- ggplot(aes(x=F.Undergrad,fill=Private))+geom_histogram(bins = 50,color="black",position=position_stack(reverse=TRUE))

#4
#Create a histogram of Grad.Rate colored by Private. You should see something odd here.
library(ggplot2)
head(df)
p2<-ggplot(data=df, aes(x=Grad.Rate))+geom_histogram(aes(color=Private))+ labs(title=" histogram plot",x="Grade_rate", y = "freq")
p2









p2<-ggplot(data=df, aes(x=Grad.Rate))+geom_histogram(aes(color=Private))
p2






df<-College
p1<-ggplot(data=df,aes(x=Grad.Rate, fill=Private))+geom_histogram(fill=pink)
p1
library(ISLR)#position = "identity" is the default position for geom_histogram(), and it ensures that each bar is directly positioned according to the data, without any stacking or adjustment based on frequency.
data(College)
ggplot(College, aes(x = Grad.Rate, fill = Private)) +
  geom_histogram(binwidth =6, position = "identity", alpha = 0.6) +
  labs(x = "Graduation Rate", y = "Frequency") +
  ggtitle("Histogram of Graduation Rate, colored by Private Status") +
  scale_fill_manual(values = c("#FF5733", "#3498DB"))  # Customizing fill colors


#5
#What college had a Graduation Rate of above 100% ?
head(df)
row.names(df[df$Grad.Rate>100,])  

#6
#Change that college's grad rate to 100%.
df["Cazenovia College","Grad.Rate"] <- 100

#7
#Split your data into training and testing sets 70/30. Use the caTools library to do this.
library(caTools)
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
#help("rpart")
tree <- rpart(Private ~ ., method = "class", data=train)


#9
# Use predict() to predict the Private label on the test data.
#help("predict")
predicted.type <- predict(tree, test)[, "Yes"]


#10
# Check the Head of the predicted values. You should notice that you actually have two columns with the probabilities.
head(predicted.type)

#11
# Turn these two columns into one column to match the original Yes/No Label for a Private column.
test$predictedPrivateType <- ifelse(predicted.type>0.5,'Yes','No')
test$predictedPrivateType
head(test)
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
#help(randomForest)
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

test










####First Question

# Read the file 
library(tidyverse)
library(dplyr)
library(caTools)

df <- read.csv('C:/Users/Rohit/Downloads/Social_Network_Ads.csv')
head(df)

#1 
dataset <- df[-c(1:2)]
head(dataset)
dataset$Purchased <- as.factor(dataset$Purchased)
str(dataset)
any(is.na(dataset))

# EDA
str(dataset)
summary(dataset)

#3 
set.seed(123)

#4 
spl <- sample.split(dataset$Purchased,0.75)
train <- subset(dataset, spl == 1)
head(train)
str(train)
test <- subset(dataset, spl == 0)
str(test)

#6.
model <- glm(formula=Purchased ~ . , family = binomial,data = train)
model

#7.
prop_prep.probabilities <- predict(model,newdata=test,type='response')
prop_prep.probabilities

#8
prop_prep.result <- ifelse(prop_prep.probabilities > 0.5,1,0)
prop_prep.result

#9.Make the confusion Matrix.
table(test$Purchased, prop_prep.probabilities  > 0.5)

#11
# Calculate Misclassification rate
misclassification_rate <- 1 - sum(diag(table)) / sum(table)

# Calculate Accuracy
accuracy <- sum(diag(table)) / sum(table)


##### Second Question




#--------------------------------
#1
dataset <- read.csv("C:/Users/Rohit/Downloads/Loan_default.csv")
dataset
# 2 Convert Default column to as factor.
dataset$Default <- as.factor(dataset$Default)
str(dataset)
# 3
set.seed(123)
split_ratio <- 0.75
train_index <- sample(1:nrow(dataset), size = split_ratio * nrow(dataset))
train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]


#4
# Load necessary library
library(randomForest)


# Fit Random Forest model
rf_model <- randomForest( Default ~ ., data = dataset, ntree = 100)

# Print the model details
print(rf_model)


#5

rf_predictions <- predict(rf_model, newdata = dataset)

# Display the predicted values
print(rf_predictions)

#6 
#did not do
#7 

#did not do

#8

dataset <- read.csv("C:/Users/Rohit/Downloads/ChurnData.csv")
dataset
#9
set.seed(350)
spl <- sample.split(dataset$Churn,0.7)
train <- subset(dataset, spl == 1)
head(train)
str(train)
test <- subset(dataset, spl == 0)
str(test)

#10
library(rpart)
model <- rpart(Churn ~ ., data = train, method = "class")

#11
plot(model)
text(model)
library(rpart.plot)
prp(model)


#12
prediction <- predict(model, newdata = train, type = "class")
confusionMatrix(prediction, dataset$Churn)

# Load the dataset
data <- read.csv("ChurnData.csv")
#Name:Shashank Barai
#Matricluation NO- 11038167
#I tried to solve this exercise based on the learning with inclass activity file shared on teams by Professor Dr Kamellia
 

library(Boruta)
library(mlbench)
library(caret)
library(caTools)
library(randomForest)

# 1. Load the dataset Spambase.csv.
df <- read.csv("C:/Users/User/Desktop/shashankbaraicollege/R/in class/new_topics/spambase.csv")
head(df)

# 2. Convert the spam column to factor.
df$spam <- as.factor(df$spam)

# 3. Perform feature selection using Boruta algorithm.
set.seed(111)
boruta1 <- Boruta(spam ~ ., data = df, doTrace = 2, maxRuns = 500)
print(boruta1)

# 4. Visualize Boruta results.
plot(boruta1, las = 2, cex.axis = 0.7)

# 5. Calculate NonRejected values.
non_rejected_formula <- getNonRejectedFormula(boruta1)
print(non_rejected_formula)

attStats(boruta1)

# 6. Data partitioning with SplitRatio = 0.7
set.seed(221)
sample1 <- sample.split(df$spam, SplitRatio = 0.7)
train <- subset(df, sample1 == TRUE)
test <- subset(df, sample1 == FALSE)

# Ensure the spam column in train and test sets have the same levels
train$spam <- factor(train$spam, levels = levels(df$spam))
test$spam <- factor(test$spam, levels = levels(df$spam))

# 7. Build random forest models with NonRejected values.
rf <- randomForest(non_rejected_formula, data = train)
print(rf)

#Predictions and evaluation
p <- predict(rf, test)
p <- as.factor(p) # Ensure predictions are factors

# Ensure both factors have the same levels
p <- factor(p, levels = levels(test$spam))

# Now, calculate the confusion matrix
confusionMatrix(p, test$spam)


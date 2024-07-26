
#1.Using the following script, read data-for-knn.csv file: 
df22<-read.csv('https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/data-for-knn.csv')
head(df22)

#2.Define the control parameters for cross-validation using trainControl.
library(MLTools)
library(caret)
set.seed(1234)

ctrl<- trainControl(method='cv', number=10)


#3.Train the KNN model with cross-validation.
fit.cv <- train(Y ~ ., data = df22, method = "knn", # k nearest neighbors
                trControl = ctrl,  # Add the control
                preProcess = c("center","scale")  # preprocess the data (center=> -mean(); scale= /standard.deviation)
)


#4.Print the KNN model details.
print(fit.cv)


#5.Plot the KNN model.
plot(fit.cv)# as you can see from plot maximum accuracy with cross validation is with 9 Nearest Neighbours


#6.Define the control parameters for cross-validation.
fitcontrol1<-trainControl(method='cv', number=5, savePredictions = TRUE)


#7.Train the decision tree model with cross-validation.
dt.fitcv <- train(Y ~ ., data = df22, method = 'rpart2', trControl = fitcontrol1)



#8.Print the decision tree model details.
print(dt.fitcv)



#9.Predict using the trained model.


predic_dt<-predict(dt.fitcv, df22)

#10.Create confusion matrix.

confusionMatrix(table(predic_dt, df22$Y))



#11.Plot the final decision tree model. 

plot(dt.fitcv$finalModel, uniform = TRUE, main = "Decision Tree Model")

#12. Add text to the decision tree plot.
text(dt.fitcv$finalModel, use.n = TRUE, all = TRUE, cex = 0.8)



#13.Plot the final decision tree model using rpart.plot.
install.packages("rpart.plot")
library(rpart.plot)


# Plot the decision tree model
rpart.plot(dt.fitcv$finalModel, main = "Decision Tree Model")
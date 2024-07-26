#Name- Shashank Barai
#Matriculation-11038167
#Dear Dr Pr Kamellia, I tried to solve this exercise based on my learning through your inclass R file shared in teams, 


#1.Load the binary.csv dataset
binary = read.csv("C:/Users/User/Desktop/shashankbaraicollege/R/in class/new_topics/binary.csv")
str(binary)
head(binary)




#2.Fit logistic regression model to predict admit based on gre and gpa
#Logistic Regression Model
library(nnet)
library(caret)
library(pROC)

mymodel1 <- glm(admit ~ gre+gpa, data = binary, family = binomial)


#3.Predict admission probabilities for gre = 800, gpa = 4. (Using predict function)
predict_prob<- predict(mymodel1, binary, type='response')
predict_prob



#4.Convert probabilities to binary predictions.
p<-ifelse(predict_prob>0.5, 1, 0)
p

#5.Calculate confusion matrix.
confusion_matrix<-table(Predicted=p, actual=binary$admit)
confusion_matrix

#6.Load pROC library.
library(pROC)
roc<-roc(binary$admit, predict_prob)


#7.Plot ROC curve.
plot.roc(roc, legacy.axes = TRUE, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate", col = "red")

#8.Calculate AUC. (You can calculate AUC using AUC function)

val1<-auc(roc)
val1
#Area under the curve: 0.6354
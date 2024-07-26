# Support Vector Machine (SVM) exercises


# 1Opn the loan_data.csv file and save it as a dataframe called loans.
loans <- read.csv("C:/Users/User/Desktop/shashankbaraicollege/R/Exercise 3/loan_data (1).csv")
str(loans)


# 2Check the summary and structure of loans.
summary(loans)

# 3Convert the following columns to categorical data using factor()
# inq.last.6mths
# delinq.2yrs
# pub.rec
# not.fully.paid
# credit.policy
loans$credit.policy <- factor(loans$credit.policy)
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)

# 4Create a histogram of fico scores colored by not.fully.paid.
library(ggplot2)
pl <- ggplot(loans,aes(x=fico)) 
pl <- pl + geom_histogram(aes(fill=not.fully.paid),color='black',bins=40,alpha=0.5)
pl + scale_fill_manual(values = c('green','red')) + theme_bw()

# 5Create a barplot of purpose counts, colored by not.fully.paid. Use position=dodge in the geom_bar argument.
pl <- ggplot(loans,aes(x=factor(purpose))) 
pl <- pl + geom_bar(aes(fill=not.fully.paid),position = "dodge")
pl + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 6Create a scatterplot of fico score versus int.rate. 
ggplot(loans,aes(int.rate,fico)) +geom_point(alpha=0.3) + theme_bw()


# 7Create a scatterplot of fico score versus int.rate. Play around with the color scheme if you want.
ggplot(loans,aes(int.rate,fico)) +geom_point(aes(color=not.fully.paid),alpha=0.7) + theme_bw()


# 8Building the Model

# Now its time to build a model!
#   
#   Train and Test Sets:
#   Split your data into training and test sets using the caTools library.
library(caTools)
set.seed(101)
spl = sample.split(loans$not.fully.paid, 0.7)
train = subset(loans, spl == TRUE)
test = subset(loans, spl == FALSE)

# 9Call the e1071 library as shown in the lecture.
install.packages("e1071")
library(e1071)

# 10Now use the svm() function to train a model on your training set.
model <- svm(not.fully.paid ~ .,data=train)

# 11Get a summary of the model.
summary(model)

# 12Use predict to predict new values from the test set using your model. 
predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)

# 13Tuning the Model:
# 
# Use the tune() function to test out different cost and
# gamma values. In the lecture we showed how to do this by using train.x and train.y,
# but its usually simpler to just pass a formula. Try checking out help(tune) for more details. 
# This is the end of the project because tuning can take a long time (since its running a bunch of different models!). 
# Take as long or as little time with this step as you would like.
# Quick hint, your tune() should look something like this:
#   
#   tune.results <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial',                   
#                        ranges=list(cost=some.vector, gamma=some.other.vector))

tune.results <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial',
                    ranges=list(cost=c(1,10), gamma=c(0.1,1)))
model <- svm(not.fully.paid ~ .,data=train,cost=10,gamma = 0.1)
predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)

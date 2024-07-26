#1.Load the Dataset-with-missing.CSV file
dataset1 = read.csv("C:/Users/User/Desktop/shashankbaraicollege/R/in class/new_topics/Dataset-with-missing.csv")
dataset1
head(dataset1)

#2.Fill missing values in the 'Sepal.Length' column with the mean of that column

#   checking for missing values in sepal.length column
any(is.na(dataset1$Sepal.Length))

#   filling it with mean values
dataset1$Sepal.Length[is.na(dataset1$Sepal.Length)] <- mean(dataset1$Sepal.Length, na.rm = TRUE)
dataset1


#3.Convert the 'Species' column from character to factor
factor(dataset1$Species)

dataset1$Species = factor(dataset1$Species, 
                          labels = c(1, 2, 3))

#4.Encode the 'Species' column into numeric categories

#scale(dataset1), this gave me error since Species is still a categorical factor but not numeric
#so i will use now is.numeric function to convert species into numeric column


#5.Scale the numeric columns (e.g., Sepal.Length and Sepal.Width) using z-score scaling
dataset1$Species <- as.numeric(dataset1$Species)

scale(dataset1)


#or if we want to scale only numeric coulmns like "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"
# 5. Scale the numeric columns using z-score scaling
numeric_columns <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
dataset1[numeric_columns] <- scale(dataset1[numeric_columns])

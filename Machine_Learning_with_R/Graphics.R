# Graphics Exercises


# 1. Using R's 'ggplot2' library and the 'mtcars' dataset, could you generate a histogram illustrating the distribution of engine displacements ('disp') among the cars? Try to generate the following plot.
library(ggplot2)
df<-mtcars
df
ggplot(data = df, aes(x = disp)) +
  geom_histogram(color = "white", fill = "blue")+labs(x ='Distribution of engine displacements', y='Frequency', title = 'Distribution of engine displacements')





















library(ggplot2)

data(mtcars)

ggplot(mtcars, aes(x = disp))+
geom_histogram(fill = "blue", color = "black", bins = 20)+
labs(title = "Distribution of Engine Displacements",
       x = "Engine Displacement",
       y = "Frequency") +theme_minimal()



# 2. Using the 'mtcars' dataset, could you generate a bar plot that counts the occurrences of 'vs' (engine shape) and employs color fill to represent different counts of cylinders (cylinder count)?

library(ggplot2)
df<-mtcars
df
a<-ggplot(data=df, aes(x=factor(vs), fill=factor(cyl)))+geom_bar()+labs(title='engshape vs cyl counts', x='fact', y='count')+theme_minimal()
a









ggplot(mtcars, aes(x = factor(vs), fill = factor(cyl))) +
  geom_bar(position = "dodge") +
  labs(title = "Engine Shape vs. Cylinder Count",
       x = "Engine Shape (0 = V-shaped, 1 = Straight)",
       y = "Count") +
  scale_fill_discrete(name = "Cylinder Count") +
  theme_minimal()


# 3. Using the 'mtcars' dataset in R and the 'ggplot2' library, generate a scatter plot illustrating the relationship between engine displacement ('disp') on the x-axis and horsepower ('hp') on the y-axis? Create the plot with blue-colored points and set the transparency (alpha) of the points to 0.5.

df<-mtcars
df
library(ggplot2)
ggplot(data=df, aes(x=disp,y=hp))+geom_point(color='blue', alpha=0.5)+
    labs(x='engine disp', y='hoursep', title='maine kia')+theme_minimal()
















ggplot(mtcars, aes(x = disp, y = hp)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Engine Displacement vs. Horsepower",
       x = "Engine Displacement",
       y = "Horsepower") +
  theme_minimal()


# 4. Add a smooth fit line to the scatterplot from above using the 'mtcars' dataset.

df<-mtcars
df
library(ggplot2)
ggplot(data=df, aes(x=disp,y=hp))+geom_point(color='blue', alpha=0.5)+
  labs(x='engine disp', y='hoursep', title='maine kia')+theme_minimal()+
stat_smooth(method = "loess", 
            formula = y ~ x, 
            geom = "smooth", se= FALSE)+theme_minimal()


ggplot(mtcars, aes(x = disp, y = hp)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Engine Displacement vs. Horsepower with Smooth Fit Line",
       x = "Engine Displacement",
       y = "Horsepower") +
  theme_minimal()


# 5. Create a boxplot showing the distribution of miles per gallon (mpg) among different numbers of cylinders.
df<-mtcars
df
library(ggplot2)
ggplot(mtcars, aes(y=factor(cyl), x=mpg)) + 
  geom_boxplot(fill = "skyblue", color = "black")+theme_minimal()















ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of MPG by Number of Cylinders",
       x = "Number of Cylinders",
       y = "Miles Per Gallon") +
  theme_minimal()


# 6. Create a scatter plot using the 'ggplot2' library in R, showcasing the relationship between miles per gallon ('mpg') on the x-axis and horsepower ('hp') on the y-axis for cars in the 'mtcars' dataset? Additionally, organize the plot into separate panels based on the number of cylinders ('cyl') using 'facet_grid'.
library(ggplot2)
df<-mtcars
df
ggplot(data=df, aes(x=mpg, y=hp))+geom_point(color='blue', alpha=0.5)+facet_grid(~cyl)












ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point(color = "black", alpha = 0.5) +
  labs(title = "Relationship between MPG and Horsepower by Cylinder Count",
       x = "Miles Per Gallon",
       y = "Horsepower") +
  facet_grid(. ~ cyl) +
  theme_minimal()


# 7. Considering mpg dataset, plot the Barplot of car counts per manufacturer with color fill defined by cyl count.
library(ggplot2)
df<-mpg
df
x<-ggplot(df, aes(x = manufacturer, fill = factor(cyl))) + geom_bar()
x






ggplot(mpg, aes(x = manufacturer, fill = factor(cyl))) +
  geom_bar() +
  labs(title = "Car Counts per Manufacturer by Cylinder Count",
       x = "Manufacturer",
       y = "Car Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


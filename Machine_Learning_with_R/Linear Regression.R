#1.Read in bikeshare.csv file and set it to a dataframe called bike, and Check the head of df.
bike <- read.csv("C:/Users/User/Desktop/shashankbaraicollege/R/Exercise 3/bikeshare (4).csv")
bike

#2.Create a scatter plot of count vs temp. Set a good alpha value.
library(ggplot2)
install.packages("plotly")
library(plotly)
pl <- ggplot(data = bike,aes(y=count,x=temp)) + geom_point(aes(color=temp),alpha=0.2)
pl

#3.
library(ggplot2)
library(plotly)
bike$datetime <- as.POSIXct(as.character(bike$datetime),format = "%Y-%m-%d %H:%M:%S")
pl <- ggplot(bike,aes(y=count,x=datetime)) + geom_point(aes(color=temp),alpha=0.5) + scale_color_gradient(low = '#4ACABB' ,high = '#ff6b00')
pl       

#4.
library(corrgram)
library(corrplot)
cor.data <- cor(bike[,c('temp','count')])
print(cor.data)

#5.
library(ggplot2)
library(plotly)
pl <- ggplot(bike,aes(y = count,x = factor(season))) + geom_boxplot(aes(color = factor(season)))
print(ggplotly(pl))

#6.
bike$hour <- format(bike$datetime, '%H')
print(head(bike))

#7.
library(ggplot2)
library(plotly)
library(dplyr)
pl <- ggplot(filter(bike,workingday == 1),aes(y = count,x = hour)) + geom_jitter(aes(color = temp),alpha = 0.4) + scale_color_gradientn(colours = c('dark blue','blue','green','yellow','orange','red'),guide = 'colourbar')
pl

#8.
library(ggplot2)
library(plotly)
library(dplyr)
pl <- ggplot(filter(bike,workingday == 0),aes(y = count,x = hour)) + geom_jitter(aes(color = temp),alpha = 0.4) + scale_color_gradientn(colours = c('dark blue','blue','green','yellow','orange','red'),guide = 'colourbar')
pl

#9
temp.model <- lm(count ~ temp,data = bike)


#10.
print(summary(temp.model))

#11.
coef <- as.data.frame(coefficients(temp.model))
check_temp <- 25
count1 <- coef[1,1] + coef[2,1]*check_temp
print(paste('Method 1: Count of Bikes at temp 25 Degree Celcius =',round(count1,4)))

check.tempdf <- data.frame(temp = 25)
count2 <- predict(temp.model,check.tempdf)
print(paste('Method 2: Count of Bikes at temp 25 Degree Celcius =',round(count2,4)))

#12.
bike$hour <- sapply(bike$hour,as.numeric)
print(str(bike))

#13.
final.model <- lm(formula = count ~ . - casual - registered - datetime - atemp, data = bike)

#14.
print(summary(final.model))


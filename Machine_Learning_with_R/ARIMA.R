
#Name : Shashank Barai
#Matriculation Number- 11038167
#Dear Dr Prof Kamellie I am not sure about #2 whether I need to convert the sales_k into time series first or it get converted into time series 
#by plot.ts(sales$Sales_k)
# I tried to solve this exercise as what I understood from your teams code of inclass

library(readxl)


#1.Import sales.csv file.
sales <- read.csv("C:/Users/User/Desktop/shashankbaraicollege/R/in class/new_topics/sales.csv")
head(sales)

install.packages("tseries")
install.packages("forecast")
#choose no
library(tseries)
library(forecast)


#2. Convert the Sales data into a time series object and plot it to check it is stationary or not

sales_ts <- ts(sales$Sales_k)

plot(sales_ts)

# Its not stationary as per plot



#3.Perform the first level difference on the Sales time series data
#Hint: Utilize the diff() function with differences = 1 to compute the first difference.
first_level_dif <- diff(sales_ts, differences = 1)
plot(first_level_dif)




#4.Perform the second level difference on the Sales time series data
#Hint: Use the diff() function with differences = 2 to calculate the second difference.
sec_level_dif <- diff(sales_ts, differences = 2)
plot(sec_level_dif)




#5.Check the stationarity of the first differenced Sales time series data using the ADF test.

adf.test(first_level_dif)




#6.Check the stationarity of the second differenced Sales time series data using the ADF test.

adf.test(sec_level_dif)




#7.Plot the stationary series after the second difference.

plot(sec_level_dif)
# yes it is stationary because printed p-value is less then 0.05        





#8.Identify the order of the MA term (q) using the Autocorrelation Function (ACF) plot
# What is the value of the third lag at which autocorrelation crosses the Control Limit Lines?

acf(sec_level_dif)

acf(first_level_dif)

auto.arima(sec_level_dif)


#9.Fit an ARIMA(7,2,6) model to the Sales 

model<-arima(sec_level_dif, order=c(7,2,6))
model



#10.Plot the final series with forecast for the next 12 periods.
forecast_model<- forecast(model, h=12)
plot(forecast_model)

library(dplyr)
library(vars)
library (timeSeries)
library(tseries)
library(urca)
library(ggplot2)


#setting working directory
setwd("/Users/simba/Downloads")

#reading the dataset
data = read.csv("jabfebfinalaugmentedwords.csv")


#i am performing mod 7 transformation over here


data <- ts(data, frequency = 7)



negsent1 <- data[,11]
price1 <- data[,4]
volume1 <- data[,5]
possent1 <- data[,10]




#creating the differenced variables


negsent <- diff(data[,11])
price <- diff(log(data[,4]))
volume <- diff(log(data[,5]))
possent <- diff(data[,10])

#seasonal decompose
plot(decompose(negsent1))
plot(decompose(price1))
plot(decompose(volume1))
plot(decompose(possent1))

#performing stationarity test


adf.test(negsent)
adf.test(price)
adf.test(volume)
adf.test(possent)

adf.test(negsent1)
adf.test(price1)
adf.test(volume1)
adf.test(possent1)





#drawing the pacf to select the lags

pacf(negsent)
pacf(possent)
pacf(price)
pacf(volume)

pacf(negsent1)
pacf(possent1)
pacf(price1)
pacf(volume1)

#performing the granger test 

grangertest(negsent~price, order = 1)
grangertest(negsent~possent, order = 1)
grangertest(negsent~volume, order = 1)

grangertest(possent~price, order = 1)
grangertest(possent~negsent, order = 1)
grangertest(possent~volume, order = 1)


grangertest(price~negsent, order = 1)
grangertest(price~possent, order = 1)
grangertest(price~volume, order = 1)

grangertest(volume~price, order = 1)
grangertest(volume~possent, order = 1)
grangertest(volume~negsent, order = 1)

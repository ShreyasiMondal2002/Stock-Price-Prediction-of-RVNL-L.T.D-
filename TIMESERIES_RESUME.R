install.packages(c("quantmod","ggplot2", "tseries", "dplyr"))
rm(list=ls())
install.packages("tidyverse")
install.packages("forecast")
install.packages("lubridate")
library(tidyverse)
library(forecast)
library(lubridate)
librray(ggplot2)
library(tseries)
library(dplyr)
library(quantmod)
getwd()


setwd("C:/Users/DELL/Downloads")
getwd()
data=read.csv(file="RVNL.NS.CSV",header=T)
data
str(data)
data$Date=as.Date(data$Date)
xt=data$Close
xt

ts.1=ts(xt,frequency=252,start=c(2020,1))
ts.1

## EXPONENTIAL TREND ##

plot.ts(ts.1,ylab="Daily Closing price",xlab="Date",col="red")

sales=log(xt);sales
length(xt)
t1=seq(-1235,1235,2);t1
L1=lm(sales~t1);L1
summary(L1)

a0.hat=3.853927
a1.hat=0.001123
A=exp(a0.hat);A
B=exp(a1.hat);B

Tt=A*(B^t)



plot.ts(ts.1,ylab="Daily Closing price",xlab="Date",col="red")

D=decompose(ts.1,type="multiplicative");D
u=D$random;u
u1=na.omit(u);u1
plot.ts(u1,ylab="Residuals",xlab="Date",col="red")

#AR(1)#
u2=arima(u1,order=c(1,0,0))$residuals;u1
MAD_AR_1=mean(abs(u1-u2)) 
MAD_AR_1             ##mean absolute deviation (MAD)##
plot.ts(u2,ylab="AR1",xlab="Year",col="red")

#AR(2)#
u3=arima(u1,order=c(2,0,0))$residuals;u2
MAD_AR_2=mean(abs(u1-u3))               #AR(2) IS BETTER since it is greater#
MAD_AR_2
plot.ts(u3,ylab="AR2",xlab="Year",col="red")

par(mfrow=c(2,1))
plot.ts(u1,ylab="AR(1)")
plot.ts(u2,ylab="AR(2)")

#MA(1)#
u4=arima(u1,order=c(0,0,1))$residuals
u4
plot.ts(u4)

#MA(2)#
u5=arima(u1,order=c(0,0,2))$residuals
u5
plot.ts(u5)


## ARIMA MODEL ##

arima_model=auto.arima(ts.1)
summary(arima_model)

forecasted_values=forecast(arima_model, h = 30)
forecasted_values

# Plot the forecast

autoplot(forecasted_values)+labs(title = "RVNL Stock Price Forecast", x = "Date", y = "Price")+theme_minimal()

checkresiduals(arima_model)









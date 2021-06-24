# install needed packages
# install.packages("astsa")

#Import libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(astsa)
library(TTR)
library(timeSeries)
library(zoo)
library(forecast)
library(smooth)
library(Mcomp)
library(urca)
library(tseries)
library(aTSA)
library(stlplus)
library(gridExtra)
library(extremogram)
library(fGarch)

setwd("C:\\Users\\carli\\Documents\\GitHub\\TimeSeries")

# Values by hour, between 01-2014 and 12-2019
data = read_excel("2014-2019 PM10 LisAvLib.xlsx")
data$Data = as.Date(data$Data)
names(data)[names(data) == "Data"] <- "date"
names(data)[names(data) == "Av.da Lib. (µg/m3)"] <- "value"

dim(data)

# transform by hour into by day
data = aggregate(data$value, by=list(data$date), function(x) mean(x, na.rm=TRUE))
names(data)[names(data) == "Group.1"] <- "date"
names(data)[names(data) == "x"] <- "value"

data$value[is.nan(data$value)]<-NA
data$value = na.approx(data$value)

# Frequency = 12 since seasonal patterns repeat every year in the case of pm10 particles
tsdata = ts(data$value, frequency = 12, start = c(2014, 1), end = c(2019,12))
plot.ts(tsdata, ylab = 'PM10 particles (µg/m3)', xlab = 'Year')

monthNames = c("Ja", "F", "Mr", "Ap", "Ma", "Jn", "Jl", "Au", "S", "O", "N", "D")

# Stl method to decompose into trend, season, and remainder
y=stl(tsdata,s.window="period")
y2=stlplus(tsdata, s.window="period", sub.labels = monthNames)
plot(y)
plot(y2)
residuals=y$time.series[,3]

# Partial autocorrelation function
pacf(tsdata, lag.max=100, main="Partial Autocorrelation Original Data")

acf(tsdata, lag.max=100, main="Autocorrelation Original Data")
acf(residuals, lag.max=100, main="Autocorrelation Residuals")

z=ts(tsdata)

extremogram1(z, 0.90, 40, type=1, ploting = 1, cutoff = 1, start = 0)

#test if our data is stationary
#----Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test 
tsdata %>% ur.kpss() %>% summary()
tseries::kpss.test(tsdata)
#----Augmented Dickey–Fuller (ADF) test
tseries::adf.test(tsdata, k=0)
summary(ur.df(tsdata, type = 'trend'))
#----Ljung-Box test for independence
Box.test(tsdata, lag=25, type="Ljung-Box")

stationary.test(tsdata)
ndiffs(tsdata) #see differencing needed

#--------------------sarima and finding parameters
#moving average
sma(tsdata, h=2, silent=FALSE)

#arima - auto 
fit <- auto.arima(tsdata, seasonal = TRUE)
summary(fit)
plot(forecast(fit,h=20))
fit %>% forecast(h = 10) %>% autoplot()
autoplot(fit)

#arima - manual
acf(tsdata) #1 spikes, so q=1
pacf(tsdata) # 2spikes, so add p=2
fit1 = Arima(tsdata, order=c(1,0,2), seasonal = c(1,1,1))
summary(fit1)
fit1 %>% forecast::forecast(h = 12) %>% autoplot()

ar.yw(tsdata)
checkresiduals(fit1)


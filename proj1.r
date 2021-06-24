#Import libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(astsa)
library(TTR)
library(timeSeries)
library(zoo)
library(stlplus)
library(gridExtra)
library(extremogram)
library(fGarch)
library(rugarch)
library(tseries)
library(fBasics)
library(zoo)
library(lmtest) 
library(forecast)


####################################################################
#PROJECT 2
####################################################################

data2 = read.table("2016-20-Nasdaq.txt", header=TRUE, sep=",", dec=".")

tsdata2 = ts(data2$Close, frequency = 12, start = c(2016, 1), end = c(2020,12))
plot.ts(tsdata2, ylab = 'Close Value', xlab = 'Year')

# Log Returns
prices<-data2$Close
log_returns <- diff(log(prices), lag=1)

plot.ts(log_returns, ylab = 'Log Returns', xlab = 'Days')

acf(log_returns, main="Autocorrelation Log Returns")
pacf(log_returns, main="Partial Autocorrelation Log Returns")

acf(log_returns^2, main="Autocorrelation Squared Log Returns")
acf(abs(log_returns), main="Autocorrelation Absolute Log Returns")


#Model fitting
garch11.t.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
garch11.t.fit=ugarchfit(spec=garch11.t.spec, data=log_returns)
garch11.t.fit
plot(garch11.t.fit, which="all")

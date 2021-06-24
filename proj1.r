# install needed packages
# install.packages("astsa")
# install.packages("")

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


setwd("C:\\Users\\carli\\Documents\\GitHub\\TimeSeries")

# Values by hour, between 01-2014 and 12-2019
data = read_excel("2014-2019 PM10 LisAvLib.xlsx")
data$Data = as.Date(data$Data)

names(data)[names(data) == "Data"] <- "date"
names(data)[names(data) == "Av.da Lib. (�g/m3)"] <- "value"

dim(data)


# Analyse by day
data = aggregate(data$value, by=list(data$date), function(x) mean(x, na.rm=TRUE))

names(data)[names(data) == "Group.1"] <- "date"
names(data)[names(data) == "x"] <- "value"

data$value[is.nan(data$value)]<-NA

data$value = na.approx(data$value)



#Plots

# Frequency = 12 since seasonal patterns repeat every year in the case of pm10 particles
tsdata = ts(data$value, frequency = 12, start = c(2014, 1), end = c(2019,12))
plot.ts(tsdata, ylab = 'PM10 particles (�g/m3)', xlab = 'Year')

monthNames = c("Ja", "F", "Mr", "Ap", "Ma", "Jn", "Jl", "Au", "S", "O", "N", "D") 

# Stl method to decompose into trend, season, and remainder
y=stl(tsdata, s.window="period")
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

#Analisar gráfico do STL para análise exploratória?
#modelo possível com os residuos 

#Quais transformações? Nós só mudamos formato de data
#transformar em dados diários - não de 24h!!. para cada dia!!
#imputação?? interpolação linear, nearest neighboor
#remover sazonalizade, trend, deterministicas


#Report - Apresentação de resultados ou tbm parte mais teórica?
#metodologia teórica, mesmo que sem muitos pormenores. 2 paginas com métodos e dps apresentação de resultados

#faz sentido analisar por dia, semana, mês? fazer a média para cada um destes periodos?



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

summary(garchFit(???garch(1,1), data=data2$Close, trace = FALSE))
summary(garchFit(???garch(1,4), data=data2$Close, trace = FALSE))

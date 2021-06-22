# install needed packages
# install.packages("astsa")
install.packages("aTSA")

#Import libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(astsa)
library(TTR)
library(timeSeries)
library(zoo)
library(forecast.Arima)
library(smooth)
library(Mcomp)
library(urca)
library(tseries)
library(aTSA)

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
plot.ts(tsdata, ylab = 'PM10 particles (Âµg/m3)', xlab = 'Year', main='Av. da Liberdade: 2014-2019 PM10 particles')

# Stl method to decompose into trend, season, and remainder
y=stl(tsdata,s.window="period")
plot(y)

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

# Autocorrelation function
acf(tsdata)

# Partial autocorrelation function
pacf(tsdata)

#--------------------sarima and finding parameters

#moving average
sma(tsdata, h=2, silent=FALSE)

#arima - auto 
fit <- auto.arima(tsdata, seasonal = TRUE)
summary(fit)
fit %>% forecast::forecast(h = 10) %>% autoplot()
autoplot(fit)

#arima - manual
acf(tsdata) #1 spikes, so q=1
pacf(tsdata) # 2spikes, so add p=2
fit1 = Arima(tsdata, order=c(1,0,2), seasonal = c(1,1,1))
summary(fit1)
fit1 %>% forecast::forecast(h = 10) %>% autoplot()

ar.yw(tsdata)
checkresiduals(fit1)
autoplot(fit)

#Analisar grÃ¡fico do STL para anÃ¡lise exploratÃ³ria?
#modelo possÃ�vel com os residuos 

#Quais transformaÃ§Ãµes? NÃ³s sÃ³ mudamos formato de data
#transformar em dados diÃ¡rios - nÃ£o de 24h!!. para cada dia!!
#imputaÃ§Ã£o?? interpolaÃ§Ã£o linear, nearest neighboor
#remover sazonalizade, trend, deterministicas


#Report - ApresentaÃ§Ã£o de resultados ou tbm parte mais teÃ³rica?
#metodologia teÃ³rica, mesmo que sem muitos pormenores. 2 paginas com mÃ©todos e dps apresentaÃ§Ã£o de resultados

#faz sentido analisar por dia, semana, mÃªs? fazer a mÃ©dia para cada um destes periodos?

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

setwd("C:\\Users\\carli\\Documents\\GitHub\\TimeSeries")

# Values by hour, between 01-2014 and 12-2019
data = read_excel("2014-2019 PM10 LisAvLib.xlsx")
data$Data = as.Date(data$Data)

names(data)[names(data) == "Data"] <- "date"
names(data)[names(data) == "Av.da Lib. (�g/m3)"] <- "value"

dim(data)

# Analyse by hour, day, week and month
# Sum all values for the same day and divide by number of observations of that day
data = aggregate(data$value, by=list(data$date), sum)
names(data)[names(data) == "Group.1"] <- "date"
names(data)[names(data) == "x"] <- "value"

if (FALSE){
  s = 0
  for (date in data$Data){
    print("Printing date:")
    print(date)
    for (hour in date){
      print(hour)
      if (date[hour] == as.numeric(substr(data$Data[1], start=9, stop=10)))
      s = s + date[hour]$`Av.da Lib. (�g/m3)`
    }
    break
  }
}

p <- ggplot(data, aes(x=date, y=value)) +
  geom_line() +
  ylab("ug/m3") + xlab("year") + theme_classic()
p

# Frequency = 12 since seasonal patterns repeat every year in the case of pm10 particles
tsdata = ts(data$value, frequency = 12, start = c(2014, 1), end = c(2019,12))
plot.ts(tsdata, ylab = 'PM10 particles (µg/m3)', xlab = 'Year')

# Stl method to decompose into trend, season, and remainder
y=stl(tsdata,s.window="period")
plot(y)

# Remove trend by differencing - alternative to stl?
data_diff = diff(tsdata, lag=12 )
tsdata_diff = ts(data_diff, frequency = 12, start = c(2014, 1), end = c(2019,12))
plot.ts(tsdata_diff, xlab='Year')

# Autocorrelation function
acf(tsdata_diff, lag=12)

# Partial autocorrelation function
pacf(tsdata)




#Analisar gráfico do STL para análise exploratória?
#modelo possível com os residuos 

#Quais transformações? Nós só mudamos formato de data
#transformar em dados diários - não de 24h!!. para cada dia!!
#imputação?? interpolação linear, nearest neighboor
#remover sazonalizade, trend, deterministicas


#Report - Apresentação de resultados ou tbm parte mais teórica?
#metodologia teórica, mesmo que sem muitos pormenores. 2 paginas com métodos e dps apresentação de resultados

#faz sentido analisar por dia, semana, mês? fazer a média para cada um destes periodos?


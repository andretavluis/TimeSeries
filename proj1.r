install.packages("astsa")
install.packages("TTR")

library(readxl)
library(ggplot2)
library(dplyr)
library(astsa)
library(TTR)


data = read_excel("2014-2019 PM10 LisAvLib.xlsx")
data$Data = as.Date(data$Data)

names(data)[names(data) == "Data"] <- "hour"
names(data)[names(data) == "Av.da Lib. (µg/m3)"] <- "value"

#eliminate NAs?
#data_withoutNA = data[complete.cases(data), ]

p <- ggplot(data, aes(x=hour, y=value)) +
              geom_line() +
              ylab("Avg") + xlab("") + theme_classic()
p




tsdata = ts(data$value, frequency = 12, start = c(2014, 1), end = c(2019,12))
plot.ts(tsdata)

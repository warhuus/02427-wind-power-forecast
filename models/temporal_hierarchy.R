rm(list=ls())
setwd("/Volumes/GoogleDrive/Mit drev/Matematisk modellering/9. semester/Adv. Time Series/exercises/02427-wind-power-forecast")
source('functions/plotting/diagnostic_plots.R')
library(thief)
library(tidyverse)
library(xts)
library(lubridate)
library(tsibble)
library(dplyr)
config <- read.csv('config.txt')

data <- read.csv('data/data/cex4WindDataInterpolated12Hours.csv')
time_train <- as.character(data[['t']])
time_train <- as.POSIXct(time_train, format = "%Y-%m-%d %H:%M:%S", tz="GMT")
names <- c('p', 'Ws1', 'Ws2', 'Ws3', 'Ws4', 'Ws5', 'Ws6', 'Ws7', 'Ws8',
           'Ws9', 'Ws10', 'Ws11', 'Ws12')

train <- data[1:config$N_train, names]
val <- data[(length(data[['p']])-config$N_valid-1):length(data[['p']]),names]

train.xts <- msts(data = train, seasonal.periods = c(1, 3, 6, 12))
agg <- tsaggregates(train.xts)
agg <- agg[c(1, 3, 5, 6)]

#hourly forecasts
hourly.fit <- tbats(train$p)
hourly.fit <- auto.arima(train$p, xreg = train$Ws1)
hourly.fit.ets <- ets(train$p)

agg3.fit <- tbats(agg$Quarterly)
acf(agg3.fit$residuals, na.action = na.pass)
pacf(agg3.fit$residuals, na.action = na.pass)

agg6.fit <- auto.arima(agg$Biannual)
acf(agg6.fit$residuals, na.action = na.pass)
pacf(agg6.fit$residuals, na.action = na.pass)

agg12.fit <- auto.arima(agg$Annual)
acf(agg12.fit$residuals, na.action = na.pass)
pacf(agg12.fit$residuals, na.action = na.pass)

# Compute forecasts
fc <- list()
for(i in seq_along(agg))
  fc[[i]] <- forecast(auto.arima(agg[[i]]), h=2*frequency(agg[[i]]))

pacf(agg$Biannual, na.action = na.pass)
fit_biannual <- arima(agg$Annual, order = c(2,0,0))
pacf(fit_biannual$residuals, na.action = na.pass)

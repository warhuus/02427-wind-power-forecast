library(marima)

rm(list=ls())
setwd("/Volumes/GoogleDrive/Mit drev/Matematisk modellering/9. semester/Adv. Time Series/exercises/02427-wind-power-forecast")
source('functions/plotting/diagnostic_plots.R')

data <- read.csv('data/data/cex4WindDataInterpolated.csv')

my.tsdiag(data['p'], na.action = na.pass)

plot(data[['p']][-c(1,2)], data[['p']][-c(length(data[['p']]),length(data[['p']]) - 1)])

#compute arima model
fit1 <- (arima(data['p'], order = c(2,0,0)))

data[['residuals_ar']] <- fit1$residuals
plot(data[['residuals_ar']])
acf(data[['residuals_ar']], na.action = na.pass)
pacf(data[['residuals_ar']], na.action = na.pass)

plot(data[['Ws1']], data[['residuals_ar']])
plot(data[['Wd1']], data[['residuals_ar']])
plot(data[['T1']], data[['residuals_ar']])

fit2 <- (arima(data['p'], order = c(2,0,2), xreg = data[c('Ws1')]))
acf(fit2$residuals, na.action = na.pass)
pacf(fit2$residuals, na.action = na.pass)

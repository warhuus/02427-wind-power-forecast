library(marima)

rm(list=ls())
setwd("/Volumes/GoogleDrive/Mit drev/Matematisk modellering/9. semester/Adv. Time Series/exercises/02427-wind-power-forecast")
source('functions/plotting/diagnostic_plots.R')

data <- read.csv('data/data/cex4WindDataInterpolated.csv')

config <- read.csv('config.txt')

data <- read.csv('data/data/cex4WindDataInterpolated.csv')
train <- data[1:config$N_train, c('t','p', 'Ws1', 'Ws2', 'Ws3')]
val <- data[(length(data[['p']])-config$N_valid-1):length(data[['p']]), c('t','p', 'Ws1', 'Ws2', 'Ws3')]

my.tsdiag(train['p'], na.action = na.pass)

plot(train[['p']][-c(1)], train[['p']][-c(length(train[['p']]))])

#compute arima model
fit1 <- (Arima(data['p'], order = c(2,0,2), seasonal = list(order=c(1,0,1), period = 12)))

data[['residuals_ar']] <- fit1$residuals
plot(data[['residuals_ar']])
acf(data[['residuals_ar']], na.action = na.pass)
pacf(data[['residuals_ar']], na.action = na.pass)

plot(data[['Ws1']], data[['residuals_ar']])
plot(data[['Wd1']], data[['residuals_ar']])
plot(data[['T1']], data[['residuals_ar']])

fit2 <- (arima(data['p'], order = c(2,0,2), 
               seasonal = list(order=c(1,0,1), period = 12), 
               xreg = data[c('Ws1')]))
acf(fit2$residuals, na.action = na.pass)
pacf(fit2$residuals, na.action = na.pass)

fit4 <- (arima(train['p'], order = c(2,0,2),
               seasonal = list(order= c(1,0,0), period = 24),
               xreg = train[c('Ws1')]))
par(mfrow=c(2,1))
acf(fit4$residuals^2, na.action = na.pass)
pacf(fit4$residuals^2, na.action = na.pass)

my.tsdiag(fit4, 'residuals', na.action = na.pass)
cpgram(fit3$residuals)

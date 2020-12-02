library(marima)
library(rugarch)
library(xts)

rm(list=ls())
setwd("/Volumes/GoogleDrive/Mit drev/Matematisk modellering/9. semester/Adv. Time Series/exercises/02427-wind-power-forecast")
source('functions/plotting/diagnostic_plots.R')

config <- read.csv('config.txt')

data <- read.csv('data/data/cex4WindDataInterpolated.csv')
train <- data[1:(length(data[['p']])-config$N_valid-1), c('t','p', 'Ws1')]
val <- data[(length(data[['p']])-config$N_valid-1):length(data[['p']]), c('t','p', 'Ws1')]

time_train <- as.character(train[['t']])
time_train <- as.POSIXct(time_train, format = "%Y-%m-%d %H:%M:%S", tz="GMT")

time_val <- as.character(val[['t']])
time_val <- as.POSIXct(time_val, format = "%Y-%m-%d %H:%M:%S", tz="GMT")

train.xts <- na.omit(xts(x = train[c('p', 'Ws1')], order.by = time_train))
val.xts <- na.omit(xts(x = val[c('p', 'Ws1')], order.by = time_val))

spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   mean.model =list(armaOrder = c(2, 2), include.mean = TRUE, 
                                   external.regressors = matrix(train.xts$Ws1)))

garch = ugarchfit(spec = spec, data = train.xts$p)

pacf(garch@fit$residuals, lag.max = 1000)

plot(train$p[0:200], type = 'l', ylim = c(-10, 30))
lines(garch@fit$fitted.values[0:200], col = 'red', type = 'l')
lower = garch@fit$fitted.values[0:200] - 2*garch@fit$sigma[0:200]
upper = garch@fit$fitted.values[0:200] + 2*garch@fit$sigma[0:200]
lines(lower, type = 'l', col ='blue', lty = 2)
lines(upper, type = 'l', col ='blue', lty = 2)

spec1 <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   mean.model =list(armaOrder = c(2, 2), include.mean = TRUE, 
                                    external.regressors = matrix(val.xts$Ws1)))
setfixed(spec1)<-as.list(coef(garch))

forecast = ugarchforecast(spec1, n.ahead = 1, n.roll = 1, data = val.xts$p , out.sample =1)

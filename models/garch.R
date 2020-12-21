library(marima)
library(rugarch)
library(xts)

rm(list=ls())
setwd("..")
source('functions/plotting/diagnostic_plots.R')

config <- read.csv('config.txt')

data <- read.csv('data/data/cex4WindDataInterpolated.csv')
train <- data[, c('t','p', 'Ws1', 'Ws2', 'Ws3')]
train$p <- append(NA, diff(train$p))
val <- data[(length(data[['p']])-config$N_valid-1):length(data[['p']]), c('t','p', 'Ws1', 'Ws2', 'Ws3')]
#create timeseries data
time_train <- as.character(train[['t']])
time_train <- as.POSIXct(time_train, format = "%Y-%m-%d %H:%M:%S", tz="GMT")

time_val <- as.character(val[['t']])
time_val <- as.POSIXct(time_val, format = "%Y-%m-%d %H:%M:%S", tz="GMT")

train.xts <- na.omit(xts(x = train[c('p', 'Ws1', 'Ws2', 'Ws3')], order.by = time_train))
val.xts <- na.omit(xts(x = val[c('p', 'Ws1', 'Ws2', 'Ws3')], order.by = time_val))

#specify model
spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   mean.model =list(armaOrder = c(2, 2), include.mean = TRUE,
                                   external.regressors = matrix(train.xts$Ws1)))

garch = ugarchfit(spec = spec, data = train.xts$p, out.sample = config$N_valid)

#3 step predictions
step1pred = rep(NA,  length(train.xts$p))
step1sigma =  rep(NA,  length(train.xts$p))
step2pred = rep(NA,  length(train.xts$p))
step2sigma =  rep(NA,  length(train.xts$p))
step3pred = rep(NA,  length(train.xts$p))
step3sigma = rep(NA,  length(train.xts$p))

n_train = length(train.xts$p)-config$N_valid
n_total = length(train.xts$p)

for (i in n_train:(n_total-3)) {
  data = train.xts$p[1:i]
  wind = train.xts$Ws1[1:(i+1)]
  wind = append(wind,train.xts$Ws2[(i+2)])
  wind = append(wind,train.xts$Ws3[(i+3)])
  wind_for = c(train.xts$Ws1[(i+1)], train.xts$Ws2[(i+2)], train.xts$Ws3[(i+3)])
  spec1 <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                      mean.model =list(armaOrder = c(2, 2), include.mean = TRUE, 
                                       external.regressors = matrix(wind)))
  setfixed(spec1)<-as.list(coef(garch))
  pred <- ugarchforecast(spec1, data=data, n.ahead = 3, n.old = n_train, 
                         external.forecasts = list(mregfor = wind_for, vregfor = NULL))
  step1pred[i+1] <- fitted(pred)[1]
  step1sigma[i+1] <- pred@forecast$sigmaFor[1]
  step2pred[i+2] <- fitted(pred)[2]
  step2sigma[i+2] <- pred@forecast$sigmaFor[2]
  step3pred[i+3] <- fitted(pred)[3]
  step3sigma[i+3] <- pred@forecast$sigmaFor[3]
}

par(mfrow=c(1,1))
plot(temp[1:200], type = 'l', ylim = c(0,15))
lines(train$p[n_train:(n_train+200)], col='green', type='l')

#2 step predictions
step2pred = rep(NA,  length(train.xts$p))
step2sigma = rep(NA,  length(train.xts$p))
n_train = length(train.xts$p)-length(val.xts$p)
n_total = length(train.xts$p)

for (i in n_train:(n_total-2)) {
  data = train.xts$p[1:i]
  wind = train.xts$Ws1[1:(i+1)]
  wind = append(wind,train.xts$Ws2[(i+2)])
  wind_for = c(train.xts$Ws1[(i+1)], train.xts$Ws2[(i+2)])
  spec1 <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                      mean.model =list(armaOrder = c(2, 2), include.mean = TRUE, 
                                       external.regressors = matrix(wind)))
  setfixed(spec1)<-as.list(coef(garch))
  pred <- ugarchforecast(spec1, data=data, n.ahead = 2, n.old = n_train, 
                         external.forecasts = list(mregfor = wind_for, vregfor = NULL))
  step2pred[i+2] <- fitted(pred)[2]
  step2sigma[i+2] <- pred@forecast$sigmaFor[2]
}



lower = step3pred - 2*step3sigma
upper = step3pred + 2*step3sigma
plot(na.omit(step3pred)[0:200], type = 'l')
lines(na.omit(lower), lty = 2, col = 'blue')
lines(na.omit(upper), lty = 2, col = 'blue')

lines(val$p[0:200], col='green')


acf(garch@fit$residuals, lag.max = 1000)

plot(train$p[1700:1800], type = 'l', ylim = c(-10, 30))
lines(garch@fit$fitted.values[1700:1800], col = 'red', type = 'l')
lower = garch@fit$fitted.values - 2*garch@fit$sigma
upper = garch@fit$fitted.values + 2*garch@fit$sigma
lines(lower[1700:1800], type = 'l', col ='blue', lty = 2)
lines(upper[1700:1800], type = 'l', col ='blue', lty = 2)

spec1 <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   mean.model =list(armaOrder = c(2, 2), include.mean = TRUE, 
                                    external.regressors = matrix(val.xts$Ws1)))
setfixed(spec1)<-as.list(coef(garch))

forecast = ugarchfilter(spec1, data = val.xts$p, n.roll = config$N_valid)

predictions = fitted(forecast)

low_ind = 0
up_ind = 200
plot(val$p[low_ind:up_ind], type = 'l', ylim = c(-5, 28))
lower = coredata(predictions) - 2*forecast@filter$sigma
upper = coredata(predictions) + 2*forecast@filter$sigma
lines(coredata(predictions)[low_ind:up_ind], col = 'red')
lines(lower[low_ind:up_ind], col = 'blue', lty = 2)
lines(upper[low_ind:up_ind], col = 'blue', lty = 2)

temp=val.xts$p - coredata(predictions)
forecast@filter$residuals

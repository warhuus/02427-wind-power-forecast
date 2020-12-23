library(marima)
library(rugarch)
library(xts)

rm(list=ls())
setwd("..")
source('functions/plotting/diagnostic_plots.R')

config <- read.csv('config.txt')

data <- read.csv('data/data/cex4WindDataInterpolated.csv')
train <- data[, c('t','p', 'Ws1', 'Ws2', 'Ws3')]
train$p1 <- append(NA, diff(train$p))
val <- data[(length(data[['p']])-config$N_valid-1):length(data[['p']]), c('t','toy', 'p', 'Ws1', 'Ws2', 'Ws3')]
val$p1 <- append(NA, diff(val$p))
#create timeseries data
time_train <- as.character(train[['t']])
time_train <- as.POSIXct(time_train, format = "%Y-%m-%d %H:%M:%S", tz="GMT")

time_val <- as.character(val[['t']])
time_val <- as.POSIXct(time_val, format = "%Y-%m-%d %H:%M:%S", tz="GMT")

train.xts <- na.omit(xts(x = train[c('p', 'p1', 'Ws1', 'Ws2', 'Ws3')], order.by = time_train))
val.xts <- na.omit(xts(x = val[c('toy','p','p1', 'Ws1', 'Ws2', 'Ws3')], order.by = time_val))

#specify model
spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   mean.model =list(armaOrder = c(2, 2), include.mean = FALSE, #arfima = TRUE,
                                   external.regressors = matrix(train.xts$Ws1)))

garch = ugarchfit(spec = spec, data = train.xts$p1, out.sample = config$N_valid)

#3 step predictions
step1pred = rep(NA,  length(val.xts$p))
step1sigma =  rep(NA,  length(val.xts$p))
step2pred = rep(NA,  length(val.xts$p))
step2sigma =  rep(NA,  length(val.xts$p))
step3pred = rep(NA,  length(val.xts$p))
step3sigma = rep(NA,  length(val.xts$p))

n_train = length(train.xts$p)-config$N_valid
n_total = length(train.xts$p)

k=1
for (i in n_train:(n_total-3)) {
  data = train.xts$p1[1:i]
  dataundiff = train.xts$p[1:i]
  wind = train.xts$Ws1[1:(i+1)]
  wind = append(wind,train.xts$Ws2[(i+2)])
  wind = append(wind,train.xts$Ws3[(i+3)])
  wind_for = c(train.xts$Ws1[(i+1)], train.xts$Ws2[(i+2)], train.xts$Ws3[(i+3)])
  spec1 <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                      mean.model =list(armaOrder = c(2, 2), include.mean = FALSE, 
                                       external.regressors = matrix(wind)))
  setfixed(spec1)<-as.list(coef(garch))
  pred <- ugarchforecast(spec1, data=data, n.ahead = 3, n.old = n_train, 
                         external.forecasts = list(mregfor = wind_for, vregfor = NULL))
  step1pred[k+1] <- fitted(pred)[1] + dataundiff[i]
  step1sigma[k+1] <- pred@forecast$sigmaFor[1]
  step2pred[k+2] <- fitted(pred)[2] + step1pred[k+1]
  step2sigma[k+2] <- pred@forecast$sigmaFor[2]
  step3pred[k+3] <- fitted(pred)[3] + step2pred[k+2]
  step3sigma[k+3] <- pred@forecast$sigmaFor[3]
  k=k+1
}
temp <- train.xts$p[n_train:length(train.xts$p)]
mean((temp-step2pred)^2, na.rm=TRUE)

par(mfrow=c(1,1))
upper <- step3pred + 2*step3sigma
lower <- step3pred - 2*step3sigma
plot(coredata(val.xts$toy[1:200]), step3pred[1:200], type = 'l', ylim = c(-5, 20), ylab = 'Wind power',
     xlab = 'Day of 2003')
lines(coredata(val.xts$toy[1:200]), upper[1:200], col = 'blue', lty = 2)
lines(coredata(val.xts$toy[1:200]), lower[1:200], col = 'blue', lty = 2)
lines(coredata(val.xts$toy[1:200]), coredata(temp)[1:200], col='red', type='l')
legend('topright', 
       legend= c('Predicted value', '95% confidence intervals', 'True value'),
       lty = c(1,2,1), col = c('black', 'blue', 'red'))

plot(coredata(val.xts$Ws1), coredata(val.xts$p))
points(coredata(val.xts$Ws1), step1pred, col = 'red')

mean((coredata(val.xts$p)-step1pred)^2, na.rm=TRUE)

#Undifferenced model
#specify model
spec1 <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   mean.model =list(armaOrder = c(2, 2), include.mean = TRUE, #arfima = TRUE,
                                    external.regressors = matrix(train.xts$Ws1)))

garch1 = ugarchfit(spec = spec1, data = train.xts$p, out.sample = config$N_valid)

#3 step predictions
step1pred_new = rep(NA,  length(val.xts$p))
step1sigma_new =  rep(NA,  length(val.xts$p))
step2pred_new = rep(NA,  length(val.xts$p))
step2sigma_new =  rep(NA,  length(val.xts$p))
step3pred_new = rep(NA,  length(val.xts$p))
step3sigma_new = rep(NA,  length(val.xts$p))

n_train = length(train.xts$p)-config$N_valid
n_total = length(train.xts$p)

k=1
for (i in n_train:(n_total-3)) {
  data = train.xts$p[1:i]
  dataundiff = train.xts$p[1:i]
  wind = train.xts$Ws1[1:(i+1)]
  wind = append(wind,train.xts$Ws2[(i+2)])
  wind = append(wind,train.xts$Ws3[(i+3)])
  wind_for = c(train.xts$Ws1[(i+1)], train.xts$Ws2[(i+2)], train.xts$Ws3[(i+3)])
  spec2 <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                      mean.model =list(armaOrder = c(2, 2), include.mean = TRUE, 
                                       external.regressors = matrix(wind)))
  setfixed(spec2)<-as.list(coef(garch1))
  pred <- ugarchforecast(spec2, data=data, n.ahead = 3, n.old = n_train, 
                         external.forecasts = list(mregfor = wind_for, vregfor = NULL))
  step1pred_new[k+1] <- fitted(pred)[1] 
  step1sigma_new[k+1] <- pred@forecast$sigmaFor[1]
  step2pred_new[k+2] <- fitted(pred)[2] 
  step2sigma_new[k+2] <- pred@forecast$sigmaFor[2]
  step3pred_new[k+3] <- fitted(pred)[3] 
  step3sigma_new[k+3] <- pred@forecast$sigmaFor[3]
  k=k+1
}
temp <- train.xts$p[n_train:length(train.xts$p)]
mean((temp-step3pred_new)^2, na.rm=TRUE)


par(mfrow=c(1,1))
upper <- step3pred_new + 2*step3sigma_new
lower <- step3pred_new - 2*step3sigma_new
plot(coredata(val.xts$toy[1:200]), step3pred_new[1:200], type = 'l', ylim = c(-5, 20), ylab = 'Wind power',
     xlab = 'Day of 2003')
lines(coredata(val.xts$toy[1:200]), upper[1:200], col = 'blue', lty = 2)
lines(coredata(val.xts$toy[1:200]), lower[1:200], col = 'blue', lty = 2)
lines(coredata(val.xts$toy[1:200]), coredata(temp)[1:200], col='red', type='l')
legend('topright', 
       legend= c('Predicted value', '95% confidence intervals', 'True value'),
       lty = c(1,2,1), col = c('black', 'blue', 'red'))


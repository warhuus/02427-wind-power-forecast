rm(list=ls())
setwd("/Volumes/GoogleDrive/Mit drev/Matematisk modellering/9. semester/Adv. Time Series/exercises/02427-wind-power-forecast")
source('functions/plotting/diagnostic_plots.R')
library(thief)
library(tidyverse)
library(xts)
library(fable)
library(lubridate)
library(tsibble)
library(dplyr)
config <- read.csv('config.txt')

data <- read.csv('data/data/cex4WindDataInterpolated12Hours.csv')
time_train <- as.character(data[['t']])
time_train <- as.POSIXct(time_train, format = "%Y-%m-%d %H:%M:%S", tz="GMT")
names <- c('t', 'toy','p', 'Ws1', 'Ws2', 'Ws3', 'Ws4', 'Ws5', 'Ws6', 'Ws7', 'Ws8',
           'Ws9', 'Ws10', 'Ws11', 'Ws12')
windnames <- c('Ws1', 'Ws2', 'Ws3', 'Ws4', 'Ws5', 'Ws6', 'Ws7', 'Ws8',
               'Ws9', 'Ws10', 'Ws11', 'Ws12')

train <- data[, names]
time_train <- as.character(train[['t']])
time_train <- as.POSIXct(time_train, format = "%Y-%m-%d %H:%M:%S", tz="GMT")
train.xts <- msts(data = train[1:(length(train$p)-config$N_valid-2),c('p')], seasonal.periods = c(1,3,6,12))
trainwind.xts <- msts(data = train[1:(length(train$p)-config$N_valid-2),c('Ws1')], seasonal.periods = c(1,3,6,12))

full.xts <- msts(data = train[c('p')], seasonal.periods = c(1,3,6,12))
fullwind <- train[, windnames]

val <- train[(length(train[['p']])-config$N_valid-2):length(train[['p']]),names]
val.xts <- msts(val$p, seasonal.periods = c(1,3,6,12))
valwind <- train[(length(train[['p']])-config$N_valid-2):length(train[['p']]), windnames]

valagg <- tsaggregates(val.xts)

valnew <- train[(length(train[['p']])-config$N_valid-2):length(train[['p']]),names]
val.xts <- msts(valnew$p, seasonal.periods = c(1,3,6,12))
valaagnew <- tsaggregates(val.xts)

agg <- tsaggregates(train.xts)

# Make models
hourly <- Arima(train.xts, order = c(2,1,2), 
                #seasonal = list(order = c(1,0,0), period = 24),
                xreg = trainwind.xts)

fc <- list()
for(i in 2:6) {
  fc[[i]] <- auto.arima(agg[[i]])
  print(i)
}

fc[[6]] <- auto.arima(agg[[6]])

mse <- rep(NA, 6)
mse[1] <- mean(hourly$residuals^2, na.rm=TRUE)
residuals_collect <- list
for(i in 2:5) {
  residuals_collect[[i]] <- fc[[i]]$residuals
  mse[i] <- mean(fc[[i]]$residuals^2, na.rm = TRUE)
}

residuals_collect[[6]] <- fc[[6]]$residuals
mse[6] <- mean(fc[[6]]$residuals^2, na.rm = TRUE)

par(mfrow=c(3,2), mar=c(3,4,2,2))
levels = c('Hourly', '2-Hourly', '3-Hourly', '4-Hourly', '6-Hourly', '12-Hourly')
plot(agg[[1]], type = 'l', ylab = levels[1], xlab = 'Number of observation',
     xlim  = c(0,100))
lines(hourly$fitted, col = 'red', lty = 2)
legend('topright', legend = c('True value', 'Fitted'), lty = c(1,1), 
       col = c('black', 'red'))
for (i in 2:6) {
  plot(agg[[i]], type = 'l', ylab = levels[i], xlab = 'Number of observation',
       xlim  = c(0,100))
  lines(fc[[i]]$fitted, col = 'red', lty = 2)
  legend('topright', legend = c('True value', 'Fitted'), lty = c(1,2), 
         col = c('black', 'red'))
  if (i == 5) {
    mtext('Number of observations',side = 1, line= 2)
  } else if (i == 6) {
    mtext('Number of observations',side = 1, line= 2)
  }
}

#predictions
aggdata <- list()
predictions_collect <- list()
sigmas_collect <- list()
val_data <- list()
frequencies6 <- c(6,3,2,1)


for (i in 1:12) {
  full.xts <- msts(data = train[i:length(train$p), c('p')], seasonal.periods = c(1,3,6,12))
  fullwind <- train[i:length(train$p), windnames]
  aggdata[[i]] <- tsaggregates(full.xts)
  predictions_collect[[i]] <- list()
  sigmas_collect[[i]] <- list()
  val_data[[i]] <- list()
  m <- 1
  for (j in 1:6) {
    freq <- frequency(aggdata[[i]][[j]])
    period <- 12/freq
    data_temp <- aggdata[[i]][[j]]
    nval <- as.integer((2003-(i-1))/period)
    start_train <- as.integer(length(data_temp)*4/5)
    r <- seq((length(data_temp)-nval), (length(data_temp)-(freq)), by = freq)
    val_data[[i]][[j]] <- data_temp[(length(data_temp)-(nval-1)):length(data_temp)]
    
    model <- fc[[j]]
    predictions <- rep(NA, nval)
    sigmas <- rep(NA, nval)
    n <- 1
    for (k in r) {
      train_data <- data_temp[start_train:k]
      if (j==1) {
        wind_for = c(fullwind$Ws1[(k+1)])
        wind1 = fullwind$Ws1[start_train:k]
        for (l in 2:freq) {
          ws = paste('Ws', as.character(l), sep ='')
          wind_for = append(wind_for, fullwind[(k+l), ws])
        }
        fit1 <- Arima(train_data, model = hourly, xreg = wind1)
        pred <- forecast(fit1, h = freq, xreg = wind_for)
      } else if (j<6) {
        fit <- Arima(train_data, model = model)
        pred <- forecast(fit, h = freq)
      } else {
        fit <- Arima(train_data, model = model)
        pred <- forecast(fit, h = freq)
      }
      
      predictions[n:(n+(freq-1))] <- pred$mean[1:freq]
      sigmas[n:(n+(freq-1))] <- pred$mean[1:freq]-pred$lower[1:freq,2]
      n <- n+freq
    }
    predictions_collect[[i]][[j]] <- predictions
    sigmas_collect[[i]][[j]] <- sigmas
  }
}

frequencies <- c(12, 6, 4, 3, 2, 1)
frequencies6 <- c(6,3,2,1)
reconciled <- list()
for (i in 1:12) {
  l <- length(predictions_collect[[i]][[6]])
  ts <- list()
  for (j in 1:6) {
    freq <- frequencies[j]
    ts[[j]] <- msts(predictions_collect[[i]][[j]][1:(freq*l)], seasonal.periods = c(12,6,4,3,2,1), ts.frequency = freq)
  }
  reconciled[[i]] <- reconcilethief(forecasts=ts, residuals = residuals_collect, mse = mse, comb = "mse") 
}

mse_forecast <- rep(NA, 6)
mse_recon <- rep(NA, 6)
l <- length(predictions_collect[[1]][[6]])
for (j in 1:6) {
  freq <- frequencies[j]
  val <- val_data[[1]][[j]][1:freq*l]
  pred <- predictions_collect[[1]][[j]][1:(freq*l)]
  recon <- reconciled[[1]][[j]]
  mse_forecast[j] <- mean((pred-val)^2, na.rm =TRUE)
  mse_recon[j] <- mean((recon-val)^2, na.rm =TRUE)
}

pred <- predictions_collect[[1]][[1]][1:(12*l)]
recon <- reconciled[[1]][[1]]
plot(recon)

par(mfrow=c(1,1))
upper <- reconciled[[1]][[1]]+ sigmas_collect[[1]][[1]][1:length(reconciled[[1]][[1]])]
lower <- reconciled[[1]][[1]]- sigmas_collect[[1]][[1]][1:length(reconciled[[1]][[1]])]
plot(val$toy[1:200], val.xts[1:200], type ='l', ylim = c(-6, 18), ylab = 'Wind power',
     xlab = 'Day of 2003')
lines(val$toy[1:200],predictions_collect[[1]][[1]][1:200], col ='red')
lines(val$toy[1:200],reconciled[[1]][[1]][1:200], col = 'green')
lines(val$toy[1:200],upper[1:200], lty = 2, col = 'blue')
lines(val$toy[1:200],lower[1:200], lty = 2, col = 'blue')
legend('topright', legend = c('True values', 'Predictions before reconciliation',
                              'Predictions after reconciliation', '95% PI for reconciled forecast'),
       lty = c(1,1,1,2), col = c('black', 'red', 'green', 'blue'))

# get 1 step predictions
step1pred <- rep(NA, length(val.xts))
step1pred_norecon <- rep(NA, length(val.xts))
sigma1pred <- rep(NA, length(val.xts))

k=0
for (i in 1:12) {
  predictions <- reconciled[[i]]$Monthly
  predictions_norecon <- predictions_collect[[i]][[1]]
  sigmas <- sigmas_collect[[i]][[1]]
  for (j in seq(1, length(predictions), by = 12)) {
    step1pred[k+j] <- predictions[j]
    step1pred_norecon[k+j] <- predictions_norecon[j]
    sigma1pred[k+j] <- sigmas[j]
  }
  k=k+1
}

plot(val$toy[1:200], val.xts[1:200], type ='l', ylab = 'Wind power',
     xlab = 'Day of 2003', ylim = c(-3, 16))
upper <- step1pred + sigma1pred
lower <- step1pred - sigma1pred
lines(val$toy[1:200], step1pred[1:200], type ='l', col = 'red')
lines(val$toy[1:200], upper[1:200], lty = 2, col = 'blue')
lines(val$toy[1:200], lower[1:200], lty = 2, col = 'blue')
legend('topright', legend = c('True values', '1 step predictions',
                              '95% prediction intervals'),
       lty = c(1,1,2), col = c('black', 'red', 'blue'))

step2pred <- rep(NA, length(val.xts))
step2pred_norecon <- rep(NA, length(val.xts))
sigma2pred <- rep(NA, length(val.xts))


k=0
for (i in 1:12) {
  predictions <- reconciled[[i]]$Monthly
  predictions_norecon <- predictions_collect[[i]][[1]]
  sigmas <- sigmas_collect[[i]][[1]]
  for (j in seq(2, length(predictions), by = 12)) {
    step2pred[k+j] <- predictions[j]
    step2pred_norecon[k+j] <- predictions_norecon[j]
    sigma2pred[k+j] <- sigmas[j]
  }
  k=k+1
}


plot(val$toy[1:200], val.xts[1:200], type ='l', ylab = 'Wind power',
     xlab = 'Day of 2003', ylim = c(-5, 16))
upper <- step2pred + sigma2pred
lower <- step2pred - sigma2pred
lines(val$toy[1:200], step2pred[1:200], type ='l', col = 'red')
lines(val$toy[1:200], upper[1:200], lty = 2, col = 'blue')
lines(val$toy[1:200], lower[1:200], lty = 2, col = 'blue')
legend('topright', legend = c('True values', '2 step predictions',
                              '95% prediction intervals'),
       lty = c(1,1,2), col = c('black', 'red', 'blue'))

step3pred <- rep(NA, length(val.xts))
step3pred_norecon <- rep(NA, length(val.xts))
sigma3pred <- rep(NA, length(val.xts))

k=0
for (i in 1:12) {
  predictions <- reconciled[[i]]$Monthly
  predictions_norecon <- predictions_collect[[i]][[1]]
  sigmas <- sigmas_collect[[i]][[1]]
  for (j in seq(3, length(predictions), by = 12)) {
    step3pred[k+j] <- predictions[j]
    step3pred_norecon[k+j] <- predictions_norecon[j]
    sigma3pred[k+j] <- sigmas[j]
  }
  k=k+1
}


plot(val$toy[1:200], val.xts[1:200], type ='l', ylab = 'Wind power',
     xlab = 'Day of 2003', ylim = c(-5, 17))
upper <- step3pred + sigma3pred
lower <- step3pred - sigma3pred
lines(val$toy[1:200], step3pred[1:200], type ='l', col = 'red')
lines(val$toy[1:200], upper[1:200], lty = 2, col = 'blue')
lines(val$toy[1:200], lower[1:200], lty = 2, col = 'blue')
legend('topright', legend = c('True values', '3 step predictions',
                              '95% prediction intervals'),
       lty = c(1,1,2), col = c('black', 'red', 'blue'))

step12pred <- rep(NA, length(val.xts))
k=0
for (i in 1:12) {
  predictions <- predictions_collect[[i]][[1]]
  for (j in seq(4, length(predictions), by = 12)) {
    step12pred[k+j] <- predictions[j]
  }
  k=k+1
}
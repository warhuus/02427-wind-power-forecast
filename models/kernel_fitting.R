rm(list=ls())
setwd("/Volumes/GoogleDrive/Mit drev/Matematisk modellering/9. semester/Adv. Time Series/exercises/02427-wind-power-forecast")
source('functions/plotting/diagnostic_plots.R')
source('functions/kernel_functions.R')

config <- read.csv('config.txt')

data <- read.csv('data/data/cex4WindDataInterpolated.csv')
n_train <- config$N_train
train <- data[(n_train-2000):n_train, c('t', 'toy','p', 'Ws1', 'Ws2', 'Ws3')]
test <- data[(n_train+1):length(data$p), c('t', 'toy','p', 'Ws1', 'Ws2', 'Ws3')]

y <- train$p[-c(1)]
y1 <- train$p[-length(train$p)]
ws <- train$Ws1[-1]

h_values <- seq(from = 0.2, to=1, by = 0.01)
mse <- rep(NA,length(h_values))

# 1 step predictions
mse <- rep(NA,length(h_values))

i <- 1
for (h in h_values) {
  df <- localLSval(y, y1, y1, ws, ws, h)
  mse[i] <- mean(y-df[['pred']], na.rm = TRUE)^2
  i <- i + 1
}

opt_width_1step <- h_values[which(mse == min(mse))]
opt_width_1step <- 0.9
pred1step <- localLSval(y, y1, y1, ws, ws, opt_width_1step)

plot(pred1step[['pred']][100:200], type = 'l')
lines(train$p[-c(1)][100:200], col = 'red')

plot(train$Ws1[-1], train$p[-c(1)])
points(train$Ws1[-1], pred1step[['pred']], col = 'red')

plot(h_values, mse, ylab = 'Mean squared error', xlab = 'Bandwidth', ylim = c(0, 2e-4))

#2 step predictions
mse2 <- rep(NA,length(h_values))
y <- train$p[-c(1, 2)]
y1train <- train$p[-c(1,length(train$p))]
y1test <- pred1step[['pred']][-length(y)]
ws1 <- train$Ws1[-c(1,2)]
ws2 <- train$Ws2[-c(1,2)]

i <- 1
for (h in h_values) {
  df <- localLSval(y, y1train, y1test, ws1, ws2, h)
  mse2[i] <- mean(y-df[['pred']], na.rm = TRUE)^2
  i <- i + 1
}

plot(h_values, mse2, ylab = 'Mean squared error', xlab = 'Bandwidth')
opt_width_2step <- h_values[which(mse2 == min(mse2))]
pred2step<- localLSval(y, y1train, y1test, ws1, ws2, opt_width_2step)

plot(pred2step[['pred']][100:200], type = 'l')
lines(train$p[-c(1, 2)][100:200], col = 'red')

#3 step predictions
y <- train$p[-c(1, 2, 3)]
y1train <- train$p[-c(1, 2, length(train$p))]
y1test <- pred2step[['pred']][-length(pred2step[['pred']])]
ws3 <- train$Ws3[-c(1,2,3)]
ws1 <- train$Ws1[-c(1,2,3)]

mse3 <- rep(NA,length(h_values))

i <- 1
for (h in h_values) {
  df <- localLSval(y, y1train, y1test, ws1, ws3, h)
  mse3[i] <- mean(y-df[['pred']], na.rm = TRUE)^2
  i <- i + 1
}

plot(h_values, mse3, ylab = 'Mean squared error', xlab = 'Bandwidth')
opt_width_3step <- h_values[which(mse3 == min(mse3))]
pred3step <- localLSval(y, y1train, y1test, ws1, ws3, opt_width_3step)

plot(pred3step[['pred']][100:200], type = 'l')
lines(train$p[-c(1, 2,3)][100:200], col = 'red')


#predictions
ytrain <- train$p[-1]
y1train <- train$p[-length(train$p)]
wstrain <- train$Ws1[-1]

#1 step
ytest <- test$p[-1]
y1test <- test$p[-length(test$p)]
wstest <- test$Ws1[-1]

#test_1step <- localLSval(ytrain, y1train, y1test, wstrain, wstest, opt_width_1step)
test_1step <- localLSval(ytrain, y1train, y1test, wstrain, wstest, 0.9, train = FALSE)
test_temp <- localLSval(ytrain, y1train, y1train, wstrain, wstrain, 0.9)
residuals <- y1train-test_temp

residuals1 <- ytest - test_1step[['pred']]

plot(test$toy[-1][1:200], test_1step[['pred']][1:200], type = 'l', 
     ylim = c(-2, 15), ylab = 'Wind power', xlab = 'Day of 2003')
lines(test$toy[-1][1:200], test_1step[['upper']][1:200], lty = 2, col = 'blue')
lines(test$toy[-1][1:200], test_1step[['lower']][1:200], lty = 2, col = 'blue')
lines(test$toy[-1][1:200], ytest[1:200], col = 'red')
legend('topright', 
       legend= c('Predicted value', '95% confidence intervals', 'True value'),
       lty = c(1,2,1), col = c('black', 'blue', 'red'))

#2 step
ytest <- test$p[-c(1, 2)]
y1test <- test_1step[['pred']][-length(test_1step[['pred']])]
wstest <- test$Ws2[-c(1, 2)]

#test_2step <- localLSval(ytrain, y1train, y1test, wstrain, wstest, opt_width_2step)
test_2step <- localLSval(ytrain, y1train, y1test, wstrain, wstest, 0.34, train = FALSE)
residuals2 <- ytest - test_2step[['pred']]

plot(test$toy[-c(1,2)][1:200], test_2step[['pred']][1:200], type = 'l', 
     ylim = c(-2, 15), ylab = 'Wind power', xlab = 'Day of 2003')
lines(test$toy[-c(1,2)][1:200], test_2step[['upper']][1:200], lty = 2, col = 'blue')
lines(test$toy[-c(1,2)][1:200], test_2step[['lower']][1:200], lty = 2, col = 'blue')
lines(test$toy[-c(1,2)][1:200], ytest[1:200], col = 'red')
legend('topright', 
       legend= c('Predicted value', '95% confidence intervals', 'True value'),
       lty = c(1,2,1), col = c('black', 'blue', 'red'))

#3 step
ytest <- test$p[-c(1, 2, 3)]
y1test <- test_2step[['pred']][-length(test_2step[['pred']])]
wstest <- test$Ws3[-c(1, 2, 3)]

#test_3step <- localLSval(ytrain, y1train, y1test, wstrain, wstest, opt_width_3step)
test_3step <- localLSval(ytrain, y1train, y1test, wstrain, wstest, 0.21, train =FALSE)
residuals3 <- ytest - test_3step[['pred']]

plot(test$toy[-c(1,2,3)][1:200], test_3step[['pred']][1:200], type = 'l', 
     ylim = c(-2, 15), ylab = 'Wind power', xlab = 'Day of 2003')
lines(test$toy[-c(1,2,3)][1:200], test_3step[['upper']][1:200], lty = 2, col = 'blue')
lines(test$toy[-c(1,2,3)][1:200], test_3step[['lower']][1:200], lty = 2, col = 'blue')
lines(test$toy[-c(1,2,3)][1:200], ytest[1:200], col = 'red')
legend('topright', 
       legend= c('Predicted value', '95% confidence intervals', 'True value'),
       lty = c(1,2,1), col = c('black', 'blue', 'red'))


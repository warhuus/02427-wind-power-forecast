rm(list=ls())
setwd("/Volumes/GoogleDrive/Mit drev/Matematisk modellering/9. semester/Adv. Time Series/exercises/02427-wind-power-forecast")

data <- read.csv('data/data/cex4WindDataInterpolated.csv')

#print variables in file
summary(data)

#get first and last day in data set
print('First day in data set:')
print(data[['t']][1])

print('Last day in data set:')
print(data[['t']][length(data[['t']])])

#plot against predicted temperature
par(mfrow=c(4,1))
par(mar = c(1, 4, 1, 1))
plot(data[[2]][1:1000], data[[3]][1:1000], type = 'l', xlab = 'Time of year in days',
     ylab = 'Wind power',
      axes = FALSE, frame.plot = TRUE)
axis(side = 2)
plot(data[[2]][1:1000], data[['T1']][-c(1)][1:1000], type = 'l', 
     axes = FALSE, frame.plot = TRUE, ylab = '1-hour temp')
axis(side = 2)
plot(data[[2]][1:1000], data[['T2']][-c(1,2)][1:1000], type = 'l', 
     axes = FALSE, frame.plot = TRUE, ylab = '2-hour temp')
axis(side = 2)
plot(data[[2]][1:1000], data[['T3']][-c(1,2,3)][1:1000], type = 'l', 
     axes = FALSE, frame.plot = TRUE, ylab = '3-hour temp')
axis(side = 1)
axis(side = 2)


#plot against predicted wind speed
par(mfrow=c(4,1))
par(mar = c(1, 4, 1, 1))
plot(data[[2]][1:1000], data[[3]][1:1000], type = 'l', xlab = 'Time of year in days',
     ylab = 'Wind power',
     axes = FALSE, frame.plot = TRUE)
axis(side = 2)
plot(data[[2]][1:1000], data[['Ws1']][-c(1)][1:1000], type = 'l', 
     axes = FALSE, frame.plot = TRUE, ylab = '1-hour speed')
axis(side = 2)
plot(data[[2]][1:1000], data[['Ws2']][-c(1,2)][1:1000], type = 'l', 
     axes = FALSE, frame.plot = TRUE, ylab = '2-hour speed')
axis(side = 2)
plot(data[[2]][1:1000], data[['Ws3']][-c(1,2,3)][1:1000], type = 'l', 
     axes = FALSE, frame.plot = TRUE, ylab = '3-hour speed')
axis(side = 1)
axis(side = 2)

#plot against predicted wind direction
par(mfrow=c(4,1))
par(mar = c(1, 4, 1, 1))
plot(data[[2]][1:1000], data[[3]][1:1000], type = 'l', xlab = 'Time of year in days',
     ylab = 'Wind power',
     axes = FALSE, frame.plot = TRUE)
axis(side = 2)
plot(data[[2]][1:1000], data[['Wd1']][-c(1)][1:1000], type = 'l', 
     axes = FALSE, frame.plot = TRUE, ylab = '1-hour speed')
axis(side = 2)
plot(data[[2]][1:1000], data[['Wd2']][-c(1,2)][1:1000], type = 'l', 
     axes = FALSE, frame.plot = TRUE, ylab = '2-hour speed')
axis(side = 2)
plot(data[[2]][1:1000], data[['Wd3']][-c(1,2,3)][1:1000], type = 'l', 
     axes = FALSE, frame.plot = TRUE, ylab = '3-hour speed')
axis(side = 1)
axis(side = 2)

fit <- loess('p ~ Ws1 + Wd1', data)

pred <- predict(data)

plot(data$Ws1, pred)

par(mfrow=c(2,1))
acf(data[[3]], na.action = na.pass)
pacf(data[[3]], na.action = na.pass)

data_diff = diff(data[[3]], lag = 1)
#data_diff = diff(data_diff, lag = 24)
acf(data_diff, na.action = na.pass, lag.max = 100)
pacf(data_diff, na.action = na.pass, lag.max = 100)



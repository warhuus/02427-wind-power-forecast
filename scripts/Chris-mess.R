setwd("C:\\Users\\CHSWA\\OneDrive - Ørsted\\DTU\\semester_1\\02427_advanced_tsa\\projects\\02427-wind-power-forecast")

# Get data and set `t` as POSIX
dat <- read.csv("data/data/cex4WindDataInterpolated.csv")
config <- read.csv("config.txt")
dat$t <- as.POSIXct(dat$t)

# Remove nans (for now)
dat <- na.omit(dat)

# Get shortened data for plotting
shortenData <- function(n, dat) {
  index <- sample.int(dim(dat)[1], n)
  return(dat[index,])
}
shortData <- shortenData(1000, dat)

# Separate data
NEData <- shortData[which(shortData$Wd1 <= 90),]
SEData <- shortData[which(shortData$Wd1 <= 90*2 & shortData$Wd1 > 90),]
SWData <- shortData[which(shortData$Wd1 <= 90*3 & shortData$Wd1 > 90*2),]
NWData <- shortData[which(shortData$Wd1 <= 90*4 & shortData$Wd1 > 90*2),]
dirData <- list("NW"=NWData, "NE"=NEData, "SW"=SWData, "SE"=SEData)

# Plot
par(mfrow = c(2, 2))
par(oma = c(4, 4, 0, 0))
par(mar = c(0.3, 0.3, 0.3, 0.3))
for (name in names(dirData))
{
  plot(dirData[[name]]$Ws1, dirData[[name]]$p, pch=20, axes=FALSE, frame.plot=TRUE)
  legend("topleft", legend=name)
  if (name == "NW" | name == "SW")
  {
    axis(2)
  }
  if (name == "SW" | name == "SE")
  {
    axis(1)
  }
}
mtext("Power [kW]", side=2, outer=TRUE, line=2)
mtext("Forecasted wind speed [m/s]", side=1, outer=TRUE, line=2)


# 3d plot
library(rgl)
open3d()
points3d(shortData$Wd1, shortData$Ws1, shortData$p)
aspect3d(c(1,1,1))
axes3d()
title3d(xlab = "Direction [deg]", ylab="Forecasted wind speed [m/s]", zlab="Power [kW]")

# Kernel estimate
fit <- loess('p ~ Ws1 + Wd1', dat, span = 0.8)
nplot <- 20
x1Seq <- seq(min(shortData$Wd1), max(shortData$Wd1), len=nplot)
y1Seq <- seq(min(shortData$Ws1), max(shortData$Ws1), len=nplot)
yprd <- outer(x1Seq, y1Seq, function(Wd1, Ws1){predict(fit, data.frame(Wd1 = Wd1, Ws1 = Ws1))})
surface3d(x1Seq, y1Seq, yprd, color="blue", alpha=0.5)

# Plot end of training data
source('functions\\plotting\\basic_data_plots.R')
plot_train_valid_data(dat, config, colors=c('black', 'red'), lty=c(1, 2), lwd=2)

fit <- arima(dat$p[1:config$N_train], order=c(1, 1, 0), xreg=dat[1:config$N_train, c("Ws1", "Wd1")])
preds <- predict(fit, newxreg=dat[config$N_train:dim(dat)[1], c("Ws1", "Wd1")])
plot_fit(preds$pred, config)


# SETAR model
training_data = dat[1:config$N_train,]
# Plot end of training data
source('models\\setar.R')
optim.model <- optim(rep(0, 18), setarRSS)
setar_preds <- setar(optim.model$par)

plot_train_valid_data(dat, config, colors=c('black', 'black'), lty=c(1, 2), lwd=2)
plot_fit(setar_preds, config, lty=c(1, 1), col="red")
legend("topright", legend=c("training data",
                            "testing_data",
                            "1-step in/out-of-sample predictions"),
       lty=c(1, 2, 1), col=c("black", "black", "red"))


# MMAR


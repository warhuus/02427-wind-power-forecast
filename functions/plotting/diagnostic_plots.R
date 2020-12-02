#diagnostic plots
my.tsdiag <- function(dat, residuals,  nlag = 200, ...){
  if(class(dat) == "Arima")
    dat <- dat[[residuals]]
  oldpar <- par(mfrow=c(4,1), mgp=c(2,0.7,0), mar=c(3,3,1.5,1))
  on.exit(par(oldpar))
  plot(dat)
  acf(dat,...)
  pacf(dat,...)
  
  pval <- sapply(1:nlag, function(i) Box.test(dat, i, type = "Ljung-Box")$p.value)
  plot(1L:nlag, pval, xlab = "lag", ylab = "p value", ylim = c(0,1), main = 
         "p values for Ljung-Box statistic")
  abline(h = 0.05, lty = 2, col = "blue")
}

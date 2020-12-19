## Epanechnikov kernel
Epanechnikov <- function(xall,x,h) {
  ## Make the weights with an Epanechnikov kernel
  ## h has the same unit as x (i.e. it is on the same absolute scale, so if x is Watt, h is also given in Watt) 
  u <- abs(xall-x)
  u <- u / h
  w <- 3/4 * (1 - u^2)
  ## Set values with |u|>1 to 0
  w[abs(u)>1] <- 0
  return(w)
}

localLSval <- function(ytrain, y1train, y1test, wstrain, wstest, h, train = TRUE) {
  pred <- rep(NA, length(y1test))
  lower <- rep(NA, length(y1test))
  upper <- rep(NA, length(y1test))
  for (j in 1:length(y1test)) {
    w <- Epanechnikov(wstrain, wstest[j], h)
    if (train == TRUE) {
      w[j] <- 0 
    }
    ok <- w > 0
    
    if (sum(ok) == 0) {
      pred[j] <- NA
      lower[j] <- NA
      upper[j] <- NA
    }    else {
      fit <- lm(y ~ y1, weights = w[ok], data = data.frame(y = ytrain[ok], y1 = y1train[ok]))
      temp <- predict(fit, data.frame(y1 = y1test[j]), interval = "prediction")
      pred[j] <- temp[1]
      lower[j] <- temp[2]
      upper[j] <- temp[3] 
    }
    
    #offset[j] <- fit$coefficients[1]
    #slope[j] <- fit$coefficients[2]
  }
  return(data.frame('pred' = pred, 'upper' = upper, 'lower' = lower))
}
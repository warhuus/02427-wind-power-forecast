library(MSwM)

half_tr_data = training_data[(dim(training_data)[1] %/% 2):(dim(training_data)[1]),]

# A few different models
mod <- lm(p~1, half_tr_data)
mod_wd <- lm(p~Wd1, half_tr_data)
mod_ws <- lm(p~Ws1, half_tr_data)
mod_full <- lm(p~Ws1 + Wd1, half_tr_data)
mod_list <- list("mod"=mod, "mod_wd"=mod_wd, "mod_ws"=mod_ws, "mod_full"=mod_full)

l <- vector("list", 2*4 + 3)

i <- 1
for (k in 2:3) {
  for (p in 1) {
    for (mod_name in names(mod_list)) {
      
      print(sprintf("\nFitting %s\n---------------", mod_name))
      coefs_fit = length(coefficients(mod_list[[mod_name]])) + p + 1
      new_fit <- msmFit(mod_list[[mod_name]], k=k, p=p,
                        sw=rep(TRUE, coefs_fit), control=list(maxiterOuter=10,
                                                      maxiterInner=20,
                                                      maxiter=200,
                                                      trace=TRUE))
      l[[i]] <- new_fit
      i <- i + 1
    }
  } 
}

for (i in 1:8) {
  summary(l[[i]])
}

############## SECOND TRY ###########################
mod_list <- list("mod_ws"=mod_ws)
i <- 9
for (k in 3) {
  for (p in 1:3) {
    for (mod_name in names(mod_list)) {
      
      print(sprintf("\nFitting %s\n---------------", mod_name))
      coefs_fit = length(coefficients(mod_list[[mod_name]])) + p + 1
      new_fit <- msmFit(mod_list[[mod_name]], k=k, p=p,
                        sw=rep(TRUE, coefs_fit), control=list(maxiterOuter=10,
                                                              maxiterInner=20,
                                                              maxiter=200,
                                                              trace=TRUE,
                                                              parallel=FALSE))
      l[[i]] <- new_fit
      i <- i + 1
    }
  } 
}

####### OUR MODEL ################
#AR(3) model w/ Ws included as external var
best <- l[[11]]

# Now make the simulation
source("functions//markovChainSimulation.R")

# MMAR(3, 3)
mmar33 <- function(Fit, valid_dat)
{
  # Make MC chain
  N <- dim(valid_dat)[1]
  P <- Fit@transMat
  MC <- run.mc.sim(P=P, N-3)
  
  # Get actual values
  y = valid_dat$p
  
  # Get other values
  ws = valid_dat$Ws1
  
  # Preds
  X = matrix(c(rep(1, N-3),         # intercept
               ws[(1+3):N],         # wind speed
               y[-N][-1:-2],        # lag 1
               y[-N][-N+1][-1],     # lag 2
               y[-N][-N+1][-N+2]),  # lag 3
             ncol=5)
  theta <- Fit@Coef
  y_hat_all <- X %*% t(coefs)
  eps <- y_hat_all - matrix(rep(y))

  # Get predictions for specfic regime
  # y_hat <- rep(NA, N-3)
  # sds <- rep(NA, N-3)
  # for (i in 1:(N-3))
  # {
  #   y_hat[i] <- y_hat_all[i, MC[i]]
  #   sds[i] <- Fit@std[MC[i]]
  # }
  return(list("preds"=y_hat, "std"=sds))
}

preds <- mmar33(best, dat)
std <- c(NA, NA, NA, preds$std)
preds <- c(NA, NA, NA, preds$preds)
plot_train_valid_data(dat, config, colors=c('black', 'red'), lty=c(1, 2), lwd=2)
plot_fit(pred, dat, config, col='blue', lty=c(3, 3), lwd=2)
legend("topright", legend=c("training data", "validation data", "MSAR fit"),
       col=c('black', 'red', 'blue'), lty=c(1, 2, 3), lwd=rep(2, 3))

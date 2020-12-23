############## CALCULATE XI ##############
N_train <- dim(half_tr_data)[1]
eps <- model@Fit@error
std <- model@std
P <- model@transMat
xi0 <- round(model@iniProb)

eta <- pnorm(eps, sd=std)
eta_t <- diag(eta[dim(eta)[1],])

weighted_probs <- eta_t %*% t(P)

for (i in (dim(eta)[1] - 1):1)
{ 
  eta_t <- eta[i,]
  if (any(eta_t < 0.01))
  {
    eta_t[which(eta_t == 0)] = eta_t[which(eta_t == 0)] + 1e-5
    eta_t <- eta_t * 6500
  }
  weighted_probs <- (diag(eta_t) %*% t(P)) %*% weighted_probs
}

unnormed_xi <- weighted_probs %*% xi0
xi <- unnormed_xi / sum(unnormed_xi)

##### CALCULATE Nth-STEP PREDICTIONS #####

state_probs <- t(xi)

y = dat$p[(config$N_train - 1):config$N_train]

ws = dat$Ws1[config$N_train + 1]
ws = append(ws, dat$Ws2[config$N_train + 2])
ws = append(ws, dat$Ws3[config$N_train + 3])

theta <- model@Coef

for (i in 1:3)        # Loop through steps
{
  X = matrix(c(1,     # Intercept
               ws[i], # Latest wind speed
               y[1 + i],  # Lag 1
               y[i]), # Lag 2
             ncol = 1)
  
  y_hat_all_states <- data.matrix(theta) %*% X
  
  state_probs <- state_probs %*% P
  state_probs <- state_probs / sum(state_probs)
  y_hat <- sum((y_hat_all_states) * t(state_probs))
  y <- append(y, y_hat)
}






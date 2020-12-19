regimes <- 3
order <- 1


# Set up initial parameters
theta <- rep(0, order * regimes)
sigs <- rep(1, order * regimes)
p_ij <- rep(1/regimes, regimes * regimes)
eps_t <- rep(0, regimes)


s_ij <- sqrt(p_ij)
st_ij <- log(s_ij / (1 - s_ij))
sigst <- log(sigs)


Theta <- matrix(c(theta, sigst, st_ij), nrow=1)

# Calculate eta
eta_t <- pnorm(eps_t, mean=0, sd=sigs)  # z x t
eta <- c(eta_t)

# Get Ps




u_t <- t(eta) %*% t(P) %*% xi_t1

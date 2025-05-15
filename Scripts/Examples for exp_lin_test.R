# In this script you can find the R code for the examples
# shown in the Supplementary Material

library(gamlss)      # To use dEXP dist
library(RelDists)    # To use dLIN dist
library(survival)    # To manage censored data

# Here we have the main functions to perform the test:
source("https://raw.githubusercontent.com/fhernanb/T_exp_lin/main/Scripts/exp_lin_test.R")

# -------------------------------------------------------------------------
# Example 1
# Exponential case
lambda <- 3
nobs <- 500
set.seed(123456)
y <- rexp(n=nobs, rate=lambda)

# Applying the test
exp_lin_test(y, alternative="not.exp", type="complete")

# -------------------------------------------------------------------------
# Example 2
# Lindley case
theta <- 3
nobs <- 500
set.seed(123456)
y <- rLIN(n=nobs, mu=theta)

# Applying the test
exp_lin_test(y, alternative="not.lin", type="complete")

# -------------------------------------------------------------------------
# Example 3
# Exponential case with Type I censoring
lambda <- 3
t0 <- 0.76 # End of the study to create censored observations
nobs <- 500
set.seed(1234567)
y <- rexp(n=nobs, rate=lambda)
delta <- rep(1, times=length(y))
delta[y >= t0] <- 0
y[y >= t0] <- t0
y <- Surv(y, delta, type="right") # Converting y to Surv class

# Applying the test
exp_lin_test(y, alternative="not.exp", type="I")

# -------------------------------------------------------------------------
# Example 4
# Lindley case with Type I censoring
theta <- 3
t0 <- 0.94 # End of the study to create censored observations
nobs <- 500
set.seed(1234567)
y <- rLIN(n=nobs, mu=theta)
delta <- rep(1, times=length(y))
delta[y >= t0] <- 0
y[y >= t0] <- t0
y <- Surv(y, delta, type="right") # Converting y to Surv class

# Applying the test
exp_lin_test(y, alternative="not.lin", type="I")



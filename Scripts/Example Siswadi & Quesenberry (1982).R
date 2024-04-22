library(gamlss)      # To use dEXP dist
library(survival)    # To manage censored data
library(gamlss.cens)

n <- 5
lambda <- 0.5

p_cens <- 0.1
t0 <- qexp(p=1-p_cens, rate=lambda, lower.tail=TRUE)

y <- rexp(n=n, rate=lambda)
delta <- rep(1, times=length(y))
delta[y > t0] <- 0
y[y > t0] <- t0
y <- Surv(y, delta, type="right")

y

logvero <- function(lambda, x) {
  x_obs <- x[x[, "status"] == 1, "time"]
  t0 <- max(x[, "time"])
  n <- length(x)
  r <- length(x_obs)
  
  p1 <- (n-r)*log(pexp(q=t0, rate=lambda, lower.tail=FALSE))
  p2 <- sum(dexp(x=x_obs, rate=lambda, log=TRUE))
  p1 + p2
}

optimize(f=logvero, lower=0.001, upper=100, x=y, maximum=TRUE)

# Usando gamlss
gen.cens(EXP, type="right")

mod_exp <- gamlss(y~1, family=EXPrc)

lambda_hat <- 1/exp(coef(mod_exp))
lambda_hat


# Second part of the experiment









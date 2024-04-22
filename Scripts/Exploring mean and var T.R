library(gamlss)      # To use dEXP dist
library(RelDists)    # To use dLIN dist
library(survival)    # To manage censored data

#source("https://raw.githubusercontent.com/ousuga/RelDists/master/R/dLIN.R")
#source("https://raw.githubusercontent.com/ousuga/RelDists/master/R/LIN.R")

# Exponential for complete case -------------------------------------------

Ts <- NULL
lambda <- 3.5
n <- 2000

for (i in 1:1000) {
  y <- rexp(n=n, rate=lambda)
  res <- exp_lin_test(y, alternative="not.exp", type="complete")
  Ts[i] <- res$statistic
}

head(Ts)

mean(Ts >= 0)

mean(Ts)
var(Ts)

mean(Ts) / n
var(Ts) / n

AME_AVE_complete(lambda=lambda)[1:2]

# Lindley for complete case -------------------------------------------

Ts <- NULL
theta <- 3.5
n <- 2000

for (i in 1:1000) {
  y <- rLIN(n=n, mu=theta)
  res <- exp_lin_test(y, alternative="not.lin", type="complete")
  Ts[i] <- res$statistic
}

head(Ts)

mean(Ts >= 0)

mean(Ts)
var(Ts)

mean(Ts) / n
var(Ts) / n

AML_AVL_complete(theta=theta)[1:2]


# Exponential for censored  -----------------------------------------------

Ts <- NULL
lambda <- 0.10

p_cens <- 0.2
t0 <- qexp(p=1-p_cens, rate=lambda, lower.tail=TRUE)

n <- 20000

for (i in 1:1000) {
  y <- rexp(n=n, rate=lambda)
  delta <- rep(1, times=length(y))
  delta[y > t0] <- 0
  y[y > t0] <- t0
  y <- Surv(y, delta, type="right")
  res <- exp_lin_test(y, alternative="not.exp", type="I")
  res
  Ts[i] <- res$statistic
}

head(Ts)

mean(Ts >= 0)

mean(Ts)
var(Ts)

mean(Ts) / n
var(Ts) / n

AME_AVE_censored(lambda=lambda, t0=t0, n=n, delta=1-p_cens)[1:2]


# Lindely for censored  -----------------------------------------------

Ts <- NULL
theta <- 1.5

p_cens <- 0.20
t0 <- qLIN(p=1-p_cens, mu=theta, lower.tail=TRUE)

n <- 5000

for (i in 1:1000) {
  y <- rLIN(n=n, mu=theta)
  delta <- rep(1, times=length(y))
  delta[y > t0] <- 0
  y[y > t0] <- t0
  y <- Surv(y, delta, type="right")
  res <- exp_lin_test(y, alternative="not.lin", type="I")
  res
  Ts[i] <- res$statistic
}

head(Ts)

mean(Ts >= 0)

mean(Ts)
var(Ts)

mean(Ts) / n
var(Ts) / n

AML_AVL_censored(theta=theta, t0=t0, delta=1-p_cens)[1:2]


# Sera mejor quitar las obs censuradas ------------------------------------

theta <- 0.5

p_cens <- 0.10
n <- 100

y <- rLIN(n=n, mu=theta)
t0 <- as.numeric(quantile(y, probs=1-p_cens))
y_obs <- y[y < t0]
delta <- rep(1, times=length(y))
delta[y >= t0] <- 0
y[y >= t0] <- t0
y <- Surv(y, delta, type="right")

res <- exp_lin_test(y, alternative="not.lin", type="I")
res
res <- exp_lin_test(y_obs, alternative="not.lin", type="complete")
res

  

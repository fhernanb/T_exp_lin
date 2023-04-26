
library(gamlss)
library(RelDists)
library(gamlss.cens)
library(survival)

source("https://raw.githubusercontent.com/fhernanb/T_exp_lin/main/Scripts/exp_lin_test.R")
source("https://raw.githubusercontent.com/fhernanb/T_exp_lin/main/Scripts/cens_functions.R")

# Example 1
# Exponential case
mu <- 1/3
n <- 50
set.seed(123456)
y <- rEXP(n=n, mu=mu)

exp_lin_test(y, alternative="not.exp", type="no-cens")


# Example 2
# Lindley case
mu <- 3
n <- 50
set.seed(123)
y <- rLIN(n=n, mu=mu)

exp_lin_test(y, alternative="not.lin", type="no-cens")

# Example 3
# Exponential case with 75% of left censored observations
mu <- 1/3
n <- 50
set.seed(123456)
y <- rEXP(n=n, mu=mu)
# To obtain a y_cut to induce left censoring
y_cut <- quantile(y, probs=0.75)
ind <- y < y_cut
y[ind] <- y_cut
event <- rep(x=1, times=n) # Assuming all are dead
event[ind] <- 0
# To create a Surv object
y <- Surv(time=y, event=event, type="left")
y

exp_lin_test(y, alternative="not.exp", type="left")

# Example 4
# Lindley case with 40% of right censored observations
mu <- 1/3
n <- 50
set.seed(123456)
y <- rLIN(n=n, mu=mu)
# To obtain a y_cut to induce left censoring
y_cut <- quantile(y, probs=0.40)
ind <- y > y_cut
y[ind] <- y_cut
event <- rep(x=1, times=n) # Assuming all are dead
event[ind] <- 0
# To create a Surv object
y <- Surv(time=y, event=event, type="right")
y

exp_lin_test(y, alternative="not.lin", type="right")


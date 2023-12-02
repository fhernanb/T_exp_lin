
library(gamlss)      # To estimate parameters and use dEXP dist
library(gamlss.cens) # For censored data
library(RelDists)    # To use dLIN dist

# Here we have the main functions to perform the test
source("https://raw.githubusercontent.com/fhernanb/T_exp_lin/main/Scripts/exp_lin_test.R")

# Example 1
# Exponential case
mu <- 1/3
n <- 50
set.seed(123456)
y <- rEXP(n=n, mu=mu)

# Applying the test
exp_lin_test(y, alternative="not.exp", type="no-cens")


# Example 2
# Lindley case
mu <- 3
n <- 50
set.seed(123)
y <- rLIN(n=n, mu=mu)

# Applying the test
exp_lin_test(y, alternative="not.lin", type="no-cens")

# Example 3
# Lindley case with right censored observations
y <- c(0.07, 0.41, 0.44, 1.49, 0.07, 0.32, 0.07, 0.48, 0.74, 0.12)
event <- c(0, 1, 1, 1, 0, 1, 0, 1, 1, 1)
# To create a Surv object
y <- Surv(time=y, event=event, type="right")
y

# Applying the test
exp_lin_test(y, alternative="not.lin", type="right")





# Example 4
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

# Example 5
# Lindley case with 40% of right censored observations
mu <- 3
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


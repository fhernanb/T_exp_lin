
library(gamlss)      # To use dEXP dist
library(RelDists)    # To use dLIN dist

# Here we have the main functions to perform the test
#source("https://raw.githubusercontent.com/fhernanb/T_exp_lin/main/Scripts/exp_lin_test.R")


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
nobs <- 100
set.seed(12345)
y <- rexp(n=nobs, rate=lambda)
n <- nobs + 10 # Assuming that 10 items were not observed
t0 <- 2.4      # The value of t0 for Type I censoring

# Applying the test
exp_lin_test(y, alternative="not.exp", type="I", n=n, t0=t0)

# -------------------------------------------------------------------------
# Example 4
# Lindley case with Type I censoring
theta <- 3
nobs <- 100
set.seed(12345)
y <- rLIN(n=nobs, mu=theta)
n <- nobs + 10 # Assuming that 10 items were not observed
t0 <- 1.8      # The value of t0 for Type I censoring

# Applying the test
exp_lin_test(y, alternative="not.lin", type="I", n=n, t0=t0)




# -------------------------------------------------------------------------
# Example 5
# Exponential case with Type II censoring
lambda <- 3
nobs <- 100
set.seed(123)
y <- rexp(n=n, rate=lambda)
n <- nobs + 10  # Assuming that 10 items were not observed

# Applying the test
exp_lin_test(y, alternative="not.exp", type="II", n=n)

# -------------------------------------------------------------------------
# Example 6
# Lindley case with Type II censoring
theta <- 3
nobs <- 100
set.seed(1234)
y <- rLIN(n=n, mu=theta)
n <- nobs + 10  # Assuming that 10 items were not observed

# Applying the test
exp_lin_test(y, alternative="not.lin", type="II", n=n)




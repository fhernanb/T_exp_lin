
library(gamlss)
library(RelDists)

# Example 1
# Case exponential
mu <- 3
n <- 500
y <- rEXP(n=n, mu=mu)

exp_lin_test(y, H0="Data come from exponential")

# Example 2
# Case Lindley
mu <- 3
n <- 500
y <- rLIN(n=n, mu=mu)

exp_lin_test(y, H0="Data come from Lindley")

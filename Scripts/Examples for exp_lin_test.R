
library(gamlss)
library(RelDists)

source("https://raw.githubusercontent.com/fhernanb/T_exp_lin/main/Scripts/exp_lin_test.R")

# Example 1
# Case exponential
mu <- 1/3
n <- 50
set.seed(123456)
y <- rEXP(n=n, mu=mu)

exp_lin_test(y, alternative="not.exp")


# Example 2
# Case Lindley
mu <- 3
n <- 50
set.seed(123)
y <- rLIN(n=n, mu=mu)

exp_lin_test(y, alternative="not.lin")

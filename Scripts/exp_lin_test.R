
exp_lin_test <- function(y, data=NULL) {
  # To fit both models
  mod1 <- gamlss(y ~ 1, family=EXP, data=data,
                 control=gamlss.control(trace=FALSE))
  mod2 <- gamlss(y ~ 1, family=LIN, data=data,
                 control=gamlss.control(trace=FALSE))
  # To obtain T
  T <- as.numeric(logLik(mod1) - logLik(mod2))
  return(T)
}

library(gamlss)
library(RelDists)

# Case exponential
mu <- 3
n <- 500
y <- rEXP(n=n, mu=mu)

exp_lin_test(y)

# Case Lindley
mu <- 3
n <- 500
y <- rLIN(n=n, mu=mu)

exp_lin_test(y)


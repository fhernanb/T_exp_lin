# In this script you can find the R function to obtain
# the  probability of correct selection (PCS)
# for the cases:
# 1) Exp complete data
# 2) Lin complete data
# 1) Exp with censored data
# 2) Lin with censored data

# PCS for exponential with complete data
PCS_exp_complete <- function(lambda, n) {
  res <- AME_AVE_complete(lambda=lambda)
  pnorm(q=0, lower.tail=FALSE, mean=res$AME, sd=sqrt(res$AVE/n))
}

PCS_exp_complete <- Vectorize(PCS_exp_complete)

# PCS for Lindley with complete data
PCS_lin_complete <- function(theta, n) {
  res <- AML_AVL_complete(theta=theta)
  pnorm(q=0, lower.tail=TRUE, mean=res$AML, sd=sqrt(res$AVL/n))
}

PCS_lin_complete <- Vectorize(PCS_lin_complete)

# PCS for exponential with censored data
PCS_exp_censored <- function(lambda, n, delta) {
  t0 <- qexp(p=delta, rate=lambda, lower.tail=TRUE)
  res <- AME_AVE_censored(lambda=lambda, t0=t0, delta=delta)
  pnorm(q=0, lower.tail=FALSE, mean=res$AME, sd=sqrt(res$AVE/n))
}

PCS_exp_censored <- Vectorize(PCS_exp_censored)

# PCS for Lindley with censored data
PCS_lin_censored <- function(theta, n, delta) {
  t0 <- qLIN(p=delta, mu=theta, lower.tail=TRUE)
  res <- AML_AVL_censored(theta=theta, t0=t0, delta=delta)
  pnorm(q=0, lower.tail=TRUE, mean=res$AML, sd=sqrt(res$AVL/n))
}

PCS_lin_censored <- Vectorize(PCS_lin_censored)


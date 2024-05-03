
# Function to obtain the integral Lambda
Lambda_fun <- function(x, i, j, k, l, lambda, theta) {
  p1 <- (log(1+x))^i
  p2 <- (1+x)^j
  p3 <- exp(-x*(k*lambda+l*theta))
  return(p1*p2*p3)
}

# Function to obtain theta tilde given lambda
theta_tilde_complete <- function(lambda) {
  a <- 1
  b <- 1 - lambda
  c <- -2*lambda
  roots <- (-b+c(1, -1)*sqrt(b^2-4*a*c)) / (2*a)
  max(roots)
}

# Function to obtain theta tilde given lambda
theta_tilde_censored <- function(lambda, t0, delta) {
  aux_fun <- function(theta, lambda, t0, delta) {
    part1 <- delta*(2/theta-1/lambda-1/(1+theta))
    part2 <- (1-delta)*((1+t0)/(theta+1+theta*t0)-t0-1/(theta+1))
    res <- part1 + part2
  }
  
  theta_tilde <- uniroot(f=aux_fun, 
                         lower=0.001, 
                         upper=1000, 
                         t0=t0,
                         lambda=lambda, 
                         delta=delta)$root
  
  return(theta_tilde)
}

# Function to obtain lambda tilde given theta
lambda_tilde_complete <- function(theta) {
  theta * (theta+1) / (theta+2)
}

# Function to obtain lambda tilde given theta
lambda_tilde_censored <- function(theta, t0, delta) {
  
  aux_fun <- function(lambda, theta, t0, delta) {
    delta/lambda - delta*(theta+2)/(theta*(theta+1)) - (1-delta)*t0
  }
  
  lambda_tilde <- uniroot(f=aux_fun, 
                          lower=0.001, 
                          upper=1000, 
                          t0=t0,
                          theta=theta, 
                          delta=delta)$root
  return(lambda_tilde)
}


# Function to obtain AME and AVE in exponential case
AME_AVE_complete <- function(lambda) {
  
  theta <- theta_tilde_complete(lambda=lambda)
  
  Lambda_1010 <- integrate(f=Lambda_fun, 
                           lower=0, 
                           upper=Inf, 
                           i=1, j=0, 
                           k=1, l=0, 
                           lambda=lambda, 
                           theta=theta)$value
  Lambda_2010 <- integrate(f=Lambda_fun, 
                           lower=0, 
                           upper=Inf, 
                           i=2, j=0, 
                           k=1, l=0, 
                           lambda=lambda, 
                           theta=theta)$value
  
  Lambda_1110 <- integrate(f=Lambda_fun, 
                           lower=0, 
                           upper=Inf, 
                           i=1, j=1, 
                           k=1, l=0, 
                           lambda=lambda, 
                           theta=theta)$value
  
  AME <- log(lambda) - 2 * log(theta) +
    log(1+theta) + theta/lambda - lambda * Lambda_1010 - 1
  
  AVE <- (theta/lambda-1)^2 + lambda * Lambda_2010 -
    (lambda * Lambda_1010)^2 - 
    2 * (theta-lambda) * (lambda*Lambda_1110 - (1+lambda)*Lambda_1010)
  
  list(AME=AME, AVE=AVE, theta_tilde_complete=theta)
}

AME_AVE_censored <- function(lambda, t0, delta) {
  
  theta <- theta_tilde_complete(lambda=lambda)
  
  # Finding AM
  aux1 <- function(x, lambda, theta) {
    p1 <- log(lambda) - lambda*x
    p2 <- 2*log(theta) + log(1+x) - log(1+theta) - theta*x
    return((p1 - p2) * lambda * exp(-lambda*x))
  }
  
  part1 <- integrate(f=aux1, 
                     lower=0, 
                     upper=t0, 
                     lambda=lambda, 
                     theta=theta)$value
  
  part2 <- -lambda*t0-log(theta+1+theta*t0)+theta*t0+log(theta+1)
  
  AME <- part1 + (1-delta) * part2
  
  # Finding AV
  
  aux3 <- function(x, lambda, theta) {
    p1 <- log(lambda) - lambda*x
    p2 <- 2*log(theta) + log(1+x) - log(1+theta) - theta*x
    return((p1 - p2)^2 * lambda * exp(-lambda*x))
  }
  
  part3 <- integrate(f=aux3,
                     lower=0,
                     upper=t0,
                     lambda=lambda,
                     theta=theta)$value
  
  part4 <- dexp(x=t0, rate=lambda) * log(dexp(x=t0, rate=lambda) / dLIN(x=t0, mu=theta))
  
  part5 <- part1 * dexp(x=t0, rate=lambda) / delta
  
  h_fun <- function(x, lambda, theta) {
    p1 <- 1 - pexp(q=x, rate=lambda)
    p2 <- 1 - pLIN(q=x, mu=theta)
    return(log(p1/p2))
  }
  
  library(numDeriv)
  deriv_h <- grad(func=h_fun, x=t0, lambda=lambda, theta=theta)
  
  part6 <- (1-delta) * deriv_h
  
  b <- - part4 + part5 - part6
  
  tau <- part3 - part1^2 / delta
  
  AVE <- tau + delta * (1-delta) * b^2 / (dexp(x=t0, rate=lambda))^2
  
  list(AME=AME, AVE=AVE, theta_tilde_complete=theta)
}


# Function to obtain AML and AVL in Lindley case
AML_AVL_complete <- function(theta) {
  
  lambda <- lambda_tilde_complete(theta=theta)
  
  Lambda_1100 <- integrate(f=Lambda_fun, 
                           lower=0, 
                           upper=Inf, 
                           i=1, j=1, 
                           k=0, l=1, 
                           lambda=lambda, 
                           theta=theta)$value
  Lambda_2101 <- integrate(f=Lambda_fun, 
                           lower=0, 
                           upper=Inf, 
                           i=2, j=1, 
                           k=0, l=1, 
                           lambda=lambda, 
                           theta=theta)$value
  
  Lambda_1101 <- integrate(f=Lambda_fun, 
                           lower=0, 
                           upper=Inf, 
                           i=1, j=1, 
                           k=0, l=1, 
                           lambda=lambda, 
                           theta=theta)$value
  
  Lambda_1201 <- integrate(f=Lambda_fun, 
                           lower=0, 
                           upper=Inf, 
                           i=1, j=2, 
                           k=0, l=1, 
                           lambda=lambda, 
                           theta=theta)$value
  
  AML <- log(lambda) - (lambda-theta)*(theta+2)/(theta*(theta+1)) +
    log(1/theta+1/theta^2) - theta^2 * Lambda_1101 / (theta+1)
  
  AVL <- (theta-lambda)^2*(theta^2+4*theta+2)/(theta^2*(theta+1)^2) +
    theta^2*Lambda_2101/(theta+1) -
    (theta^2*Lambda_1101/(theta+1))^2 - 
    2*(theta-lambda)*(theta^2*(Lambda_1201-Lambda_1101)/(theta+1)-theta*(theta+2)*Lambda_1101/(theta+1)^2)
  
  list(AML=AML, AVL=AVL, lambda_tilde_complete=lambda)
}

AML_AVL_censored <- function(theta, t0, delta) {
  
  lambda <- lambda_tilde_complete(theta=theta)
  
  # Finding AM
  aux1 <- function(x, lambda, theta) {
    p1 <- log(lambda) - lambda*x
    p2 <- 2*log(theta) + log(1+x) - log(1+theta) - theta*x
    return((p1 - p2) * theta^2*(1+x)*exp(-theta*x)/(1+theta))
  }
  
  part1 <- integrate(f=aux1, 
                     lower=0, 
                     upper=t0, 
                     lambda=lambda, 
                     theta=theta)$value
  
  part2 <- -lambda*t0-log(theta+1+theta*t0)+theta*t0+log(theta+1)
  
  AML <- part1 + (1-delta) * part2 
  
  # Finding AV
  
  aux3 <- function(x, lambda, theta) {
    p1 <- log(lambda) - lambda*x
    p2 <- log(theta^2) + log(1+x) - log(1+theta) - theta*x
    return((p1 - p2)^2 * theta^2*(1+x)*exp(-theta*x)/(1+theta))
  }
  
  part3 <- integrate(f=aux3, 
                     lower=0, 
                     upper=t0, 
                     lambda=lambda, 
                     theta=theta)$value
  
  part4 <- dLIN(x=t0, mu=theta) * log(dexp(x=t0, rate=lambda) / dLIN(x=t0, mu=theta))
  
  part5 <- part1 * dLIN(x=t0, mu=theta) / delta
  
  h_fun <- function(x, lambda, theta) {
    p1 <- 1 - pexp(q=x, rate=lambda)
    p2 <- 1 - pLIN(q=x, mu=theta)
    return(log(p1/p2))
  }
  
  library(numDeriv)
  deriv_h <- grad(func=h_fun, x=t0, lambda=lambda, theta=theta)
  
  part6 <- (1-delta) * deriv_h
  
  b <- part4 - part5 + part6
  
  tau <- part3 - part1^2 / delta
  
  AVL <- tau + delta * (1-delta) * b^2 / (dLIN(x=t0, mu=theta))^2
  
  list(AML=AML, AVL=AVL, lambda_tilde_complete=lambda)
}

# Main function to perform the test

exp_lin_test <- function(y, delta=NULL,
                         alternative=c("not.exp", "not.lin"),
                         type=c("complete", "I")) {
  
  # Checking the inputs
  
  # To define the right Family with or without censoring
  
  #------------------------------------------
  # Complete data
  if (type == "complete") {
    n <- length(y)
    
    # To estimate lambda
    lambda_hat <- n / sum(y)
    # To estimate theta
    theta_hat <- (-(mean(y)-1)+sqrt((mean(y)-1)^2+8*mean(y))) / (2*mean(y))
    
    log_lik_exp <- sum(dexp(x=y, rate=lambda_hat, log=TRUE))
    log_lik_lin <- sum(dLIN(x=y, mu=theta_hat, log=TRUE))
    
    # The T statistic
    statistic <- log_lik_exp - log_lik_lin
    
    # Re-naming the estimated parameters in short form
    lambda <- lambda_hat
    theta  <- theta_hat
    
    # To obtain the NULL distribution of T
    if (alternative == "not.exp") {
      temp <- AME_AVE_complete(lambda=lambda)
      AM <- temp$AME
      AV <- temp$AVE
    }
    
    if (alternative == "not.lin") {
      temp <- AML_AVL_complete(theta=theta)
      AM <- temp$AML
      AV <- temp$AVL
    }
  }
  
  #------------------------------------------
  # Type I censoring data
  if (type == "I") {
    
    stopifnot(class(y) == "Surv")
    
    ##########################################################
    # Obtaining the estimator of lambda and theta manually
    ##########################################################
    
    # To estimate lambda
    y_obs <- y[y[, "status"] == 1, "time"]
    t0 <- max(y[, "time"])
    n <- length(y)
    d <- length(y_obs)
    delta <- d/n
    lambda_hat <- d / (sum(y_obs) + (n-d)*t0)
    # To estimate theta numerically
    aux_fun <- function(theta, y, n, t0) {
      d <- length(y)
      part1 <- 2*d/theta + ((n-d)*(1+t0))/(1+theta+theta*t0) - n/(1+theta)
      part2 <- sum(y) + (n-d)*t0
      return(part1 - part2)
    }
    theta_hat <- uniroot(f=aux_fun,
                         lower=0.001,
                         upper=100,
                         y=y_obs,
                         n=length(y),
                         t0=t0)$root

    log_lik_exp <- sum(dexp(x=y_obs, rate=lambda_hat, log=TRUE)) +
      (n-d) * log(1 - pexp(q=t0, rate=lambda_hat))
    log_lik_lin <- sum(dLIN(x=y_obs, mu=theta_hat, log=TRUE)) +
      (n-d) * log(1 - pLIN(q=t0, mu=theta_hat))

    # The T statistic
    statistic <- log_lik_exp - log_lik_lin
    
    ##########################################################
    #  Obtaining the estimator of lambda and theta by gamlss
    ##########################################################
    # library(gamlss.cens)
    # gen.cens(EXP, type="right")
    # gen.cens(LIN, type="right")
    # 
    # mod_exp <- gamlss(y~1, family=EXPrc)
    # mod_lin <- gamlss(y~1, family=LINrc)
    # 
    # lambda_hat <- 1/exp(coef(mod_exp))
    # theta_hat <- exp(coef(mod_lin))
    # 
    # statistic <- logLik(mod_exp) - logLik(mod_lin)

    
    # Re-naming the estimated parameters in short form
    lambda <- lambda_hat
    theta  <- theta_hat
    
    # To obtain the NULL distribution of T
    if (alternative == "not.exp") {
      temp <- AME_AVE_censored(lambda=lambda, delta=delta, t0=t0)
      AM <- temp$AME
      AV <- temp$AVE
    }
    
    if (alternative == "not.lin") {
      temp <- AML_AVL_censored(theta=theta, delta=delta, t0=t0)
      AM <- temp$AML
      AV <- temp$AVL
    }
  }
  
  # The p.value of T assuming N(AM, AV)
  lower.tail <- alternative == "not.exp"
  
  p.value <- pnorm(q=statistic, mean=AM, sd=sqrt(AV),
                   lower.tail=lower.tail)
  
  # To ensure that the output values are in the correct form
  method <- "Exponential-Lindley test"
  names(statistic) <- "T"
  data.name <- deparse(substitute(x))
  
  # To obtain appropiate information about
  # sample and distribution of T
  estimate <- ifelse(alternative=="not.exp", lambda, theta)
  estimate <- c(estimate, AM, AV)
  names_for_first_element <- ifelse(alternative=="not.exp", 
                                    "lambda_hat", 
                                    "theta_hat")
  names(estimate) <- c(names_for_first_element,
                       "AM for T",
                       "AV for T")
  
  res <- list(statistic=statistic,
              p.value=p.value,
              estimate=estimate,
              method=method,
              alternative=alternative,
              lambda=lambda, 
              theta=theta,
              AM=AM, AV=AV)
  
  class(res) <- "htest"
  return(res)
}



# Function to obtain the integral Lambda
Lambda_fun <- function(x, i, j, k, l, lambda, theta) {
  p1 <- (log(1+x))^i
  p2 <- (1+x)^j
  p3 <- exp(-x*(k*lambda+l*theta))
  return(p1*p2*p3)
}

# Function to obtain theta tilde given lambda
theta_tilde <- function(lambda) {
  a <- 1
  b <- 1 - lambda
  c <- -2*lambda
  roots <- (-b+c(1, -1)*sqrt(b^2-4*a*c)) / (2*a)
  max(roots)
}

# Function to obtain lambda tilde given theta
lambda_tilde <- function(theta) {
  theta*(theta+1) / (theta+2)
}

# Function to obtain AME and AVE in exponential case
aux_case_exp <- function(lambda) {
  
  theta <- theta_tilde(lambda=lambda)
  
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
  
  list(AME=AME, AVE=AVE, theta_tilde=theta)
  
}

# Function to obtain AML and AVL in Lindley case
aux_case_lin <- function(theta) {
  
  lambda <- lambda_tilde(theta=theta)
  
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
  
  list(AML=AML, AVL=AVL, lambda_tilde=lambda)
  
}


# Main function to perform the test

exp_lin_test <- function(y, t0=NULL, n,
                         alternative=c("not.exp", "not.lin"),
                         type=c("complete", "I", "II")) {
  
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
    log_lik_exp <- sum(dEXP(x=y, mu=1/lambda_hat, log=TRUE))
    log_lik_lin <- sum(dLIN(x=y, mu=theta_hat, log=TRUE))
    statistic <- log_lik_exp - log_lik_lin
  }
  
  #------------------------------------------
  # Type I censoring data
  if (type == "I") {
    
    if(is.null(t0)) 
      stop("In type I you must enter the time t0.")
    if(is.null(n))  
      stop("In type I you must enter the total number of items observed.")
    
    d <- length(y)
    
    # To estimate lambda
    lambda_hat <- d / (sum(y) + (n-d)*t0)
    
    # To estimate theta numerically
    aux_fun <- function(theta, y, n, t0) {
      d <- length(y)
      part1 <- 2*d/theta + ((n-d)*(1+t0))/(1+theta+theta*t0) - n/(1+theta)
      part2 <- sum(y) + (n-d)*t0
      return(part1 - part2)
    }
    theta_hat <- uniroot(f=aux_fun, lower=0.001, upper=100, 
                         y=y, n=n, t0=t0)$root
    
    # loglik for EXP
    log_lik_exp <- log(lambda_hat) * d - 
      lambda_hat * sum(y) - 
      lambda_hat * t0 * (n-d)
    
    log_lik_exp2 <- sum(dEXP(x=y, mu=1/lambda_hat, log=TRUE)) + 
      (n-d) * (1 - pEXP(q=t0, mu=1/lambda_hat))
    
    # loglik for LIN
    log_lik_lin <- 2*log(theta_hat)*d - theta_hat*sum(y) + 
      log(theta_hat+1+theta_hat*t0)*(n-d) - n*log(1+theta_hat) -
      theta_hat*t0*(n-d) + sum(log(1+y))
    
    log_lik_lin2 <- sum(dLIN(x=y, mu=theta_hat, log=TRUE)) + 
      (n-d) * (1 - pLIN(q=t0, mu=theta_hat))
    
    # The proposed statistic
    statistic <- log_lik_exp - log_lik_lin
  }
  
  #------------------------------------------
  # Type II censoring data
  if (type == "II") {
    
    if(is.null(n))  
      stop("In type II you must enter the total number of items observed.")
    
    r <- length(y)  # To obtain the number of items observed
    y_r <- max(y)   # To obtain the x_r or y_r
    
    # To estimate lambda
    lambda_hat <- r / (sum(y) + (n-r)*y_r)
    
    # To estimate theta numerically
    aux_fun <- function(theta, y, n) {
      r <- length(y)
      y <- sort(y)
      y_r <- tail(y, n=1)
      part1 <- 2*r/theta + (n-r)*(1+y_r)/(1+theta+theta*y_r) - n/(1+theta)
      part2 <- sum(y) + (n-r)*y_r
      return(part1 - part2)
    }
    theta_hat <- uniroot(f=aux_fun, lower=0.001, upper=100, 
                         y=y, n=n)$root
    
    # loglik for EXP
    log_lik_exp <- sum(dEXP(x=y, mu=1/lambda_hat, log=TRUE)) + 
      (n-r) * log(1 - pEXP(q=y_r, mu=1/lambda_hat))
    
    # loglik for LIN
    log_lik_lin <- sum(dLIN(x=y, mu=theta_hat, log=TRUE)) + 
      (n-r) * log(1 - pLIN(q=y_r, mu=theta_hat))
    
    # The proposed statistic
    statistic <- log_lik_exp - log_lik_lin
  }
  
  print("The estimated parameter assuming Exponential is:")
  print(paste0("lambda_hat = ", lambda_hat))
  print("The estimated parameter assuming Lindley is:")
  print(paste0("theta_hat = ", theta_hat))
  
  # Re-naming the estimated parameters in short form
  lambda <- lambda_hat
  theta <- theta_hat
  
  # To obtain the NULL distribution of T
  if (alternative == "not.exp") {
    AM <- aux_case_exp(lambda=lambda)$AME
    AV <- aux_case_exp(lambda=lambda)$AVE
  }
  
  if (alternative == "not.lin") {
    AM <- aux_case_lin(theta=theta)$AML
    AV <- aux_case_lin(theta=theta)$AVL
  }
  
  # The p.value of T assuming N(AM, AV)
  lower.tail <- alternative == "not.exp"
  
  p.value <- pnorm(q=statistic, mean=AM, sd=sqrt(AV),
                   lower.tail=lower.tail)
  
  # To ensure that the output values are in the correct form
  method <- "Exponential-Lindley test"
  names(statistic) <- 'T'
  data.name <- deparse(substitute(x))
  
  # To obtain appropiate information about
  # sample and distribution of T
  estimate <- ifelse(alternative=="not.exp", lambda, theta)
  estimate <- c(estimate, AM, AV)
  names_for_first_element <- ifelse(alternative=="not.exp", 
                                    "lambda_hat", 
                                    "theta_hat")
  names(estimate) <- c(names_for_first_element,
                       "AV for T",
                       "AM for T")
  
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


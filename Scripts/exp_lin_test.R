
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

exp_lin_test <- function(y, data=NULL, 
                         alternative=c("not.exp",
                                       "not.lin")) {
  # To fit both models
  mod1 <- gamlss(y ~ 1, family=EXP, data=data,
                 control=gamlss.control(trace=FALSE))
  mod2 <- gamlss(y ~ 1, family=LIN, data=data,
                 control=gamlss.control(trace=FALSE))
  
  # To obtain the estimated parameters
  lambda <- 1/exp(coef(mod1, what="mu"))
  theta <- exp(coef(mod2, what="mu"))
  
  # To convert
  lambda <- as.numeric(lambda)
  theta  <- as.numeric(theta)
  
  # To obtain T using loglik values
  statistic <- as.numeric(logLik(mod1) - logLik(mod2))
  
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


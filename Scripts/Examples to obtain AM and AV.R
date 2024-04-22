# Creating the table 7
lambdas <- c(0.1, 0.5, 0.9, 1.3, 1.5, 2.0, 2.5)
t(sapply(X=lambdas, FUN=AME_AVE_complete))

# Creating the table 8
thetas <- c(0.1, 0.5, 0.9, 1.3, 1.5, 2.0, 2.5)
t(sapply(X=thetas, FUN=AML_AVL_complete))

# Creating the table 9
lambdas <- c(0.1, 0.5, 0.9, 1.3, 1.5, 2.0, 2.5)
res <- t(sapply(X=lambdas, FUN=AME_AVE_censored, 
                t0=1, delta=0.90))

round(apply(res, 2, as.numeric), digits=4)

# Creating the table 10
lambdas <- c(0.1, 0.5, 0.9, 1.3, 1.5, 2.0, 2.5)
t(sapply(X=lambdas, FUN=AML_AVL_censored, 
         t0=1, delta=0.90))

# To obtian PCS_exp_complete table 4

PCS_exp_complete <- function(lambda, n) {
  res <- AME_AVE_complete(lambda=lambda)
  pnorm(q=0, lower.tail=FALSE, mean=res$AME, sd=sqrt(res$AVE/n))
}

lambdas <- c(0.1, 0.5, 0.9, 1.3, 1.5, 2.0, 2.5)
ns <- c(20, 40, 60, 80, 100, 200)

PCS_exp_complete <- Vectorize(PCS_exp_complete)

results <- outer(X=lambdas, Y=ns, FUN=PCS_exp_complete)
rownames(results) <- lambdas
colnames(results) <- ns
round(results, digits=3)

# To obtian PCS_lin_complete table 5

PCS_lin_complete <- function(theta, n) {
  res <- AML_AVL_complete(theta=theta)
  pnorm(q=0, lower.tail=TRUE, mean=res$AML, sd=sqrt(res$AVL/n))
}

thetas <- c(0.1, 0.5, 0.9, 1.3, 1.5, 2.0, 2.5)
ns <- c(20, 40, 60, 80, 100, 200)

PCS_lin_complete <- Vectorize(PCS_lin_complete)

results <- outer(X=thetas, Y=ns, FUN=PCS_lin_complete)
rownames(results) <- thetas
colnames(results) <- ns
round(results, digits=3)

# To obtian PCS_exp_censored table 6

PCS_exp_censored <- function(lambda, n, delta) {
  t0 <- qexp(p=delta, rate=lambda, lower.tail=TRUE)
  res <- AME_AVE_censored(lambda=lambda, t0=t0, delta=delta)
  pnorm(q=0, lower.tail=FALSE, mean=res$AME, sd=sqrt(res$AVE/n))
}

lambdas <- c(0.1, 0.5, 0.9, 1.3, 1.5, 2.0, 2.5)
ns <- c(20, 40, 60, 80, 100, 200)

PCS_exp_censored <- Vectorize(PCS_exp_censored)

results <- outer(X=lambdas, Y=ns, FUN=PCS_exp_censored, delta=0.90)
rownames(results) <- lambdas
colnames(results) <- ns
round(results, digits=3)

# To obtian PCS_exp_censored table 7

results <- outer(X=lambdas, Y=ns, FUN=PCS_exp_censored, delta=0.80)
rownames(results) <- lambdas
colnames(results) <- ns
round(results, digits=3)

# To obtian PCS_exp_censored table 8

PCS_lin_censored <- function(theta, n, delta) {
  t0 <- qLIN(p=delta, mu=theta, lower.tail=TRUE)
  res <- AML_AVL_censored(theta=theta, t0=t0, delta=delta)
  pnorm(q=0, lower.tail=TRUE, mean=res$AML, sd=sqrt(res$AVL/n))
}

thetas <- c(0.1, 0.5, 0.9, 1.3, 1.5, 2.0, 2.5)
ns <- c(20, 40, 60, 80, 100, 200)

PCS_lin_censored <- Vectorize(PCS_lin_censored)

results <- outer(X=thetas, Y=ns, FUN=PCS_lin_censored, delta=0.90)
rownames(results) <- thetas
colnames(results) <- ns
round(results, digits=3)

# To obtian PCS_exp_censored table 9

results <- outer(X=lambdas, Y=ns, FUN=PCS_exp_censored, delta=0.80)
rownames(results) <- lambdas
colnames(results) <- ns
round(results, digits=3)


# En este script se ilustra la forma de estimar parametros
# para la distribucion Lindley

library(RelDists)
library(gamlss)

# Ejemplo 1 - simulando modelo regresion Lindley y calculando T

n <- 100
x1 <- runif(n=n)
x2 <- runif(n=n)
eta <- 1 + 3 * x1 - 2 * x2
mu <- exp(eta)
y <- rLIN(n=n, mu=mu)

mod1 <- gamlss(y ~ x1 + x2, family=LIN)
mod2 <- gamlss(y ~ x1 + x2, family=EXP)

T <- logLik(mod2) - logLik(mod1)
T

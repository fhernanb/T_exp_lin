# En este script se ilustra la forma de estimar parametros
# para la distribucion Lindley

library(RelDists)
library(gamlss)

# Ejemplo 1 - Estimando el parametro mu (usualmente theta)

x <- rLIN(n=100, mu=2)
mod1 <- gamlss(x ~ 1, family=LIN)
summary(mod1)

# Para obtener mu hat
exp(coef(mod1))

# Ejemplo 2 - Estimando parametros modelo regresion Y ~ Lindley

n <- 100
x1 <- runif(n=n)
x2 <- runif(n=n)
eta <- 1 + 3 * x1 - 2 * x2
mu <- exp(eta)
y <- rLIN(n=n, mu=mu)

mod2 <- gamlss(y ~ x1 + x2, family=LIN)
summary(mod2)



require(stats)
require(VGAM)
require(stats4)

tht <- 0.1
lm_t <- (tht*(tht+1))/(tht+2)

x <- rexp(100000,lm_t)
y <- rlind(100000,tht)

# AM when lindley is null
integrand4  <-  function(x) {log(1+x)*(1+x)*exp(-tht*x)}
M_L <- integrate(integrand4, lower = 0, upper = Inf)$value
AM_L <- log(lm_t)-((lm_t-tht)*(tht+2)/(tht*(tht+1)))-2*log(tht)+log(tht+1)-((tht^2/(tht+1))*M_L)
AM_L

# AV when lindley is null
integrand5  <-  function(x) {((log(1+x))^2)*(1+x)*exp(-tht*x)}
V1_L <- integrate(integrand5, lower = 0, upper = Inf)$value
integrand6  <-  function(x) {x*log(1+x)*(1+x)*exp(-tht*x)}
V2_L <- integrate(integrand6, lower = 0, upper = Inf)$value
AV_L <- (((-lm_t+tht)^2)*((tht^2)+(4*tht)+2)/((tht^2)*((tht+1)^2)))+(((tht^2/(tht+1))*V1_L)-(((tht^2/(tht+1))*M_L)^2))-2*(-lm_t+tht)*((tht^2/(tht+1)*V2_L)-((tht+2)/(tht*(tht+1))*(tht^2/(tht+1))*M_L))
AV_L

#PCS_asymptotic
n <- c(20,40,60,80,100,200)
pcs_L <- pnorm(-sqrt(n)*AM_L/sqrt(AV_L))
pcs_L

pnorm(sqrt(n)*AM_L/sqrt(AV_L), lower.tail=FALSE)


          
library(RelDists)
library(gamlss)
library(gamlss.cens)

gen_data <- function(mu, n, dist, p_cens) {
  # To convert mu to beta0
  k <- match(mu, c(0.1, 0.5, 0.9, 1.3, 1.5, 2.0, 2.5))
  beta0 <- c(-1.8, -0.19, 0.39, 0.76, 0.91, 1.19, 1.42)[k]
  # The covariates
  x1 <- runif(n=n)
  x2 <- runif(n=n)
  eta <- beta0 + 2 * x1 - 3 * x2
  mu <- exp(eta)
  # To simulate from exp
  if (dist=="EXP") {
    mu <- 1/mu
    y <- rEXP(n=n, mu=mu)
  }
  # to simulate from lin
  else
    y <- rLIN(n=n, mu=mu)
  event <- rep(x=1, times=n) # Assuming all are dead
  # To create censored observations using p_cens
  if (p_cens > 0) {
    y_cut <- quantile(y, probs=1-p_cens)
    ind <- y > y_cut
    y[ind]     <- y_cut
    event[ind] <- 0
  }
  # To create a Surv object
  y <- Surv(time=y, event=event)
  data.frame(y, x1, x2)
}

# Some tests
gen_data(mu=0.1, n=30, dist="EXP", p_cens=0.5)
gen_data(mu=0.1, n=30, dist="LIN", p_cens=0.5)

# To fit both models
gen.cens(EXP, type="right")
gen.cens(LIN, type="right")

data <- gen_data(mu=0.1, n=30000, dist="EXP", p_cens=0.0)

mod1 <- NULL
mod1 <- gamlss(y ~ x1 + x2, family=EXPrc, data=data,
               control=gamlss.control(trace=FALSE))
coef(mod1)

mod2 <- NULL
mod2 <- gamlss(y ~ x1 + x2, family=LINrc, data=data,
               control=gamlss.control(trace=FALSE))
coef(mod2)

# To simulate one time
obtain_one_T <- function(mu, n, dist, p_cens) {
  data <- gen_data(mu=mu, n=n, dist=dist, p_cens=p_cens)
  # To fit both models
  mod1 <- gamlss(y ~ x1 + x2, family=EXPrc, data=data,
                 control=gamlss.control(trace=FALSE))
  mod2 <- gamlss(y ~ x1 + x2, family=LINrc, data=data,
                 control=gamlss.control(trace=FALSE))
  # To obtain T
  T <- as.numeric(logLik(mod1) - logLik(mod2))
  return(T)
}

# Some tests
obtain_one_T(mu=0.1, n=300, dist="EXP", p_cens=0.3)
obtain_one_T(mu=0.1, n=300, dist="LIN", p_cens=0.3)


# To perform the simulation -----------------------------------------------

library("parSim")

parSim(
  ### SIMULATION CONDITIONS
  mu = c(0.1, 0.5, 0.9, 1.3, 1.5, 2.0, 2.5),
  n = c(20, 40, 60, 80, 100, 200),
  dist = c("EXP", "LIN"),
  p_cens = c(0, 0.1, 0.2),
  
  reps = 1000,                            # Repetitions
  write = TRUE,                           # Writing to a file
  name = "Simul/with_X_right_cens_part1", # Name of the file
  nCores = 1,                             # Number of cores to use
  
  expression = {
    # Obtain T:
    T <- obtain_one_T(mu=mu, n=n, dist=dist, p_cens=p_cens)
    # Decision:
    decision <- ifelse(T > 0, "EXP", "LIN")
    
    # Results list:
    Results <- list(
      decision = decision
    )
    
    # Return:
    Results
  }
)


# To load the results -----------------------------------------------------

datos1 <- read.table("Simul/with_X_right_cens_part1.txt", header = TRUE)
datos2 <- read.table("Simul/with_X_right_cens_part2.txt", header = TRUE)
datos3 <- read.table("Simul/with_X_right_cens_part3.txt", header = TRUE)
datos <- rbind(datos1, datos2, datos3)

library(dplyr)

# To convert some variables to factors
datos <- datos |> mutate(mu = factor(mu),
                         p_cens = factor(p_cens))

# To summarise pcs for each case
res00 <- datos %>% 
  filter(p_cens == 0) %>% 
  group_by(mu, n, dist) %>% 
  summarise(pcs=mean(dist == decision))

res01 <- datos %>% 
  filter(p_cens == 0.1) %>% 
  group_by(mu, n, dist) %>% 
  summarise(pcs=mean(dist == decision))

res02 <- datos %>% 
  filter(p_cens == 0.2) %>% 
  group_by(mu, n, dist) %>% 
  summarise(pcs=mean(dist == decision))

# To plot
library(ggplot2)

p0 <- ggplot(res00, aes(x=n, y=pcs, group=mu)) +
  geom_line(aes(color=mu)) +
  geom_point(aes(color=mu)) +
  guides(color = guide_legend(title=expression(mu))) +
  facet_wrap(~ dist, ncol = 2) + 
  ylim(0, 1) + 
  ggtitle("PCS for regression model and 0% of right censored data")

p1 <- ggplot(res01, aes(x=n, y=pcs, group=mu)) +
  geom_line(aes(color=mu)) +
  geom_point(aes(color=mu)) +
  guides(color = guide_legend(title=expression(mu))) +
  facet_wrap(~ dist, ncol = 2) + 
  ylim(0, 1) + 
  ggtitle("PCS for regression model and 10% of right censored data")

p2 <- ggplot(res02, aes(x=n, y=pcs, group=mu)) +
  geom_line(aes(color=mu)) +
  geom_point(aes(color=mu)) +
  guides(color = guide_legend(title=expression(mu))) +
  facet_wrap(~ dist, ncol = 2) + 
  ylim(0, 1) + 
  ggtitle("PCS for regression model and 20% of right censored data")

p0
p1
p2

ggsave("Plots/with_X_right_cens_00.pdf", plot = p0, width = 8, height = 4)
ggsave("Plots/with_X_right_cens_01.pdf", plot = p1, width = 8, height = 4)
ggsave("Plots/with_X_right_cens_02.pdf", plot = p2, width = 8, height = 4)


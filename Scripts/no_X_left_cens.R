library(RelDists)
library(gamlss)
library(gamlss.cens)

gen_data <- function(mu, n, dist, p_cens) {
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
    y_cut <- quantile(y, probs=p_cens)
    ind <- y < y_cut
    y[ind]     <- y_cut
    event[ind] <- 0
  }
  # To create a Surv object
  y <- Surv(time=y, event=event, type="left")
  data.frame(y)
}

# Some tests
gen_data(mu=10, n=30, dist="EXP", p_cens=0.5)
gen_data(mu=10, n=30, dist="LIN", p_cens=0.5)

# To fit both models
gen.cens(EXP, type="left")
gen.cens(LIN, type="left")

data <- gen_data(mu=10, n=300, dist="EXP", p_cens=0.5)

mod1 <- NULL
mod1 <- gamlss(y ~ 1, family=EXPlc, data=data,
               control=gamlss.control(trace=FALSE))
exp(coef(mod1))

mod2 <- NULL
mod2 <- gamlss(y ~ 1, family=LINlc, data=data,
               control=gamlss.control(trace=FALSE))
exp(coef(mod2))

# To simulate one time
obtain_one_T <- function(mu, n, dist, p_cens) {
  data <- gen_data(mu=mu, n=n, dist=dist, p_cens=p_cens)
  # To fit both models
  mod1 <- gamlss(y ~ 1, family=EXPlc, data=data,
                 control=gamlss.control(trace=FALSE))
  mod2 <- gamlss(y ~ 1, family=LINlc, data=data,
                 control=gamlss.control(trace=FALSE))
  # To obtain T
  T <- as.numeric(logLik(mod1) - logLik(mod2))
  return(T)
}

# Some tests
obtain_one_T(mu=5, n=300, dist="EXP", p_cens=0.3)
obtain_one_T(mu=5, n=300, dist="LIN", p_cens=0.3)


# To perform the simulation -----------------------------------------------

library("parSim")

parSim(
  ### SIMULATION CONDITIONS
  mu = c(0.1, 0.5, 0.9, 1.3, 1.5, 2.0, 2.5),
  n = c(20, 40, 60, 80, 100, 200),
  dist = c("EXP", "LIN"),
  p_cens = c(0, 0.1, 0.2),
  
  reps = 100,                        # Repetitions
  write = TRUE,                        # Writing to a file
  name = "Simul/no_X_left_cens_part2",     # Name of the file
  nCores = 1,                          # Number of cores to use
  
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

datos1 <- read.table("Simul/no_X_left_cens_part1.txt", header = TRUE)
datos2 <- read.table("Simul/no_X_left_cens_part2.txt", header = TRUE)
datos3 <- read.table("Simul/no_X_left_cens_part3.txt", header = TRUE)
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
  ggtitle("PCS for a random sample and 0% of left censored data")

p1 <- ggplot(res01, aes(x=n, y=pcs, group=mu)) +
  geom_line(aes(color=mu)) +
  geom_point(aes(color=mu)) +
  guides(color = guide_legend(title=expression(mu))) +
  facet_wrap(~ dist, ncol = 2) + 
  ylim(0, 1) + 
  ggtitle("PCS for a random sample and 10% of left censored data")

p2 <- ggplot(res02, aes(x=n, y=pcs, group=mu)) +
  geom_line(aes(color=mu)) +
  geom_point(aes(color=mu)) +
  guides(color = guide_legend(title=expression(mu))) +
  facet_wrap(~ dist, ncol = 2) + 
  ylim(0, 1) + 
  ggtitle("PCS for a random sample and 20% of left censored data")

p0
p1
p2

ggsave("Plots/no_X_left_cens_00.pdf", plot = p0, width = 8, height = 4)
ggsave("Plots/no_X_left_cens_01.pdf", plot = p1, width = 8, height = 4)
ggsave("Plots/no_X_left_cens_02.pdf", plot = p2, width = 8, height = 4)


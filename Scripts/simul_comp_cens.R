library(RelDists)
library(gamlss)

gen_data <- function(mu, n, dist, type="complete", p_cens=NULL) {
  
  # Creating the output objects
  y <- NULL
  t0 <- NULL
  
  # To simulate from exp
  if (dist == "EXP") {
    y <- rEXP(n=n, mu=1/mu)
  }
  # To simulate from lin
  else
    y <- rLIN(n=n, mu=mu)
  
  # To create censored observations type I
  if (type == "I") {
    quantile_cens <- quantile(y, probs=1-p_cens)
    t0 <- as.numeric(quantile_cens)
    delta <- rep(1, times=length(y))
    delta[y >= t0] <- 0
    y[y >= t0] <- t0
    y <- Surv(y, delta, type="right") # Converting y to Surv class
  }

  # To generate the output
  list(y=y, type=type, n=n, t0=t0)
}

# Some tests
gen_data(mu=1.0, n=30, dist="EXP", type="complete")
gen_data(mu=1.0, n=30, dist="LIN", type="complete")

gen_data(mu=1.0, n=30, dist="EXP", type="I", p_cens=0.1)
gen_data(mu=1.0, n=30, dist="LIN", type="I", p_cens=0.1)

# To simulate one time
simul_one <- function(mu, n, dist, type, p_cens) {
  data <- gen_data(mu=mu, n=n, dist=dist, type=type, p_cens=p_cens)
  # Applying the test
  ALT <- ifelse(dist=="EXP", "not.exp", "not.lin")
  test <- exp_lin_test(data$y, alternative=ALT, type=type)
  # To conclude
  result <- test$statistic
  return(result)
}

# Some tests
simul_one(mu=5, n=300, dist="EXP", type="complete")
simul_one(mu=5, n=300, dist="EXP", type="I", p_cens=0.1)

simul_one(mu=0.5, n=300, dist="LIN", type="complete")
simul_one(mu=1.5, n=300, dist="LIN", type="I", p_cens=0.90)

# To perform the simulation -----------------------------------------------

library("parSim")

# Instruction to simulate type I
parSim(
  ### SIMULATION CONDITIONS
  
  mu = c(0.1, 0.5, 0.9, 1.3, 1.5, 2.0, 2.5),
  n = c(20, 40, 60, 80, 100, 200),
  dist = c("EXP", "LIN"),
  p_cens = c(0.1, 0.2),
  type = c("I"),
  
  reps = 10000,                    # Repetitions
  write = TRUE,                  # Writing to a file
  name = "Simul/sim_type_I",     # Name of the file
  nCores = 1,                    # Number of cores to use
  
  expression = {
    # Obtain decision:
    T <- simul_one(mu=mu, n=n, dist=dist, type=type, p_cens=p_cens)
    # Decision:
    decision <- ifelse(T >= 0, "EXP", "LIN")
    
    # Results list:
    Results <- list(
      decision = decision
    )
    
    # Return:
    Results
  }
)

# Instruction to simulate complete
parSim(
  ### SIMULATION CONDITIONS
  
  mu = c(0.1, 0.5, 0.9, 1.3, 1.5, 2.0, 2.5),
  n = c(20, 40, 60, 80, 100, 200),
  dist = c("EXP", "LIN"),
  p_cens = c(0),
  type = c("complete"),
  
  reps = 10000,                    # Repetitions
  write = TRUE,                  # Writing to a file
  name = "Simul/sim_complete",     # Name of the file
  nCores = 1,                    # Number of cores to use
  
  expression = {
    # Obtain decision:
    T <- simul_one(mu=mu, n=n, dist=dist, type=type, p_cens=p_cens)
    # Decision:
    decision <- ifelse(T >= 0, "EXP", "LIN")
    
    # Results list:
    Results <- list(
      decision = decision
    )
    
    # Return:
    Results
  }
)


# To load the results -----------------------------------------------------

library(dplyr)
library(ggplot2)

# Case complete

# Reading data
datos <- read.table("Simul/sim_complete.txt", header = TRUE)
# To convert some variables to factors
datos <- datos |> mutate(mu = factor(mu))
# To summarise pcs for each case
res_comp <- datos %>% 
  filter(type == "complete" & p_cens == 0) %>% 
  group_by(mu, n, dist, type, p_cens) %>% 
  summarise(pcs=mean(dist == decision), nrep=n())

# To explore PCS for certain case
res_comp |> filter(dist=="EXP", mu==0.1)

# Plotting
p1 <- ggplot(res_comp, aes(x=n, y=pcs, group=mu)) +
  geom_line(aes(color=mu)) +
  geom_point(aes(color=mu)) +
  guides(color = guide_legend(title=expression(mu))) +
  facet_wrap(~ dist, ncol = 2) + 
  ylim(0, 1) + 
  ggtitle("PCS for a complete random sample")
p1

ggsave("Plots/complete.pdf", plot = p1, width = 8, height = 4)





################ Case type I censoring with 10%

# Reading data
datos <- read.table("Simul/sim_type_I.txt", header = TRUE)
# To convert some variables to factors
datos <- datos |> mutate(mu = factor(mu))
# To summarise pcs for each case
res_I_01 <- datos %>% 
  filter(type == "I" & p_cens == 0.1) %>% 
  group_by(mu, n, dist, type, p_cens) %>% 
  summarise(pcs=mean(dist == decision), nrep=n())

# To explore PCS for certain case
res_I_01 |> filter(dist=="LIN", mu==2.5) |> pull(pcs) |> 
  round(digits=3) |> 
  paste0(sep=") & (", collapse="")

# Plotting
p2 <- ggplot(res_I_01, aes(x=n, y=pcs, group=mu)) +
  geom_line(aes(color=mu)) +
  geom_point(aes(color=mu)) +
  guides(color = guide_legend(title=expression(mu))) +
  facet_wrap(~ dist, ncol = 2) + 
  ylim(0, 1) + 
  ggtitle("PCS for a random sample with type I censoring and 10% of censoring")
p2

ggsave("Plots/type_I_10.pdf", plot = p2, width = 8, height = 4)


################ Case type I censoring with 20%

# Reading data
datos <- read.table("Simul/sim_type_I.txt", header = TRUE)
# To convert some variables to factors
datos <- datos |> mutate(mu = factor(mu))
# To summarise pcs for each case
res_I_02 <- datos %>% 
  filter(type == "I" & p_cens == 0.2) %>% 
  group_by(mu, n, dist, type, p_cens) %>% 
  summarise(pcs=mean(dist == decision), nrep=n())

# To explore PCS for certain case
res_I_02 |> filter(dist=="LIN", mu==2.5) |> pull(pcs) |> 
  round(digits=3) |> 
  paste0(sep=") & (", collapse="")

# Plotting
p3 <- ggplot(res_I_02, aes(x=n, y=pcs, group=mu)) +
  geom_line(aes(color=mu)) +
  geom_point(aes(color=mu)) +
  guides(color = guide_legend(title=expression(mu))) +
  facet_wrap(~ dist, ncol = 2) + 
  ylim(0, 1) + 
  ggtitle("PCS for a random sample with type I censoring and 20% of censoring")
p3

ggsave("Plots/type_I_20.pdf", plot = p3, width = 8, height = 4)








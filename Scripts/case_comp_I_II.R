library(RelDists)
library(gamlss)

gen_data <- function(mu, n, dist, type="complete", p_cens=NULL) {
  
  # Creating the output objects
  y <- NULL
  t0 <- NULL
  
  # To simulate from exp
  if (dist=="EXP") {
    y <- rEXP(n=n, mu=1/mu)
  }
  # To simulate from lin
  else
    y <- rLIN(n=n, mu=mu)
  
  # To create censored observations type I
  if (type=="I") {
    quantile_cens <- quantile(y, probs=1-p_cens)
    y <- y[y <= quantile_cens]
    t0 <- as.numeric(quantile_cens)
  }

  # To create censored observations type II
  if (type=="II") {
    quantile_cens <- quantile(y, probs=1-p_cens)
    y <- y[y <= quantile_cens]
  }
  
  # To generate the output
  list(y=y, type=type, n=n, t0=t0)
}

# Some tests
gen_data(mu=10, n=30, dist="EXP", type="complete")
gen_data(mu=10, n=30, dist="LIN", type="complete")

gen_data(mu=10, n=30, dist="EXP", type="I", p_cens=0.1)
gen_data(mu=10, n=30, dist="LIN", type="I", p_cens=0.1)

gen_data(mu=10, n=30, dist="EXP", type="II", p_cens=0.1)
gen_data(mu=10, n=30, dist="LIN", type="II", p_cens=0.1)


# To simulate one time
simul_one <- function(mu, n, dist, type, p_cens) {
  data <- gen_data(mu=mu, n=n, dist=dist, type=type, p_cens=p_cens)
  # Applying the test
  ALT <- ifelse(dist=="EXP", "not.exp", "not.lin")
  test <- exp_lin_test(data$y, alternative=ALT, 
                       type=type, n=data$n, t0=data$t0)
  # To conclude
  p.value <- test$p.value
  return(p.value)
}

# Some tests
simul_one(mu=5, n=300, dist="EXP", type="complete")
simul_one(mu=5, n=300, dist="EXP", type="I", p_cens=0.5)
simul_one(mu=5, n=300, dist="EXP", type="II", p_cens=0.5)

simul_one(mu=5, n=300, dist="LIN", type="complete")
simul_one(mu=5, n=300, dist="LIN", type="I", p_cens=0.5)
simul_one(mu=5, n=300, dist="LIN", type="II", p_cens=0.5)

# To perform the simulation -----------------------------------------------

library("parSim")

parSim(
  ### SIMULATION CONDITIONS
  
  mu = c(0.1, 0.5, 0.9, 1.3, 1.5, 2.0, 2.5),
  n = c(20, 40, 60, 80, 100, 200),
  dist = c("EXP", "LIN"),
  p_cens = c(0, 0.1, 0.2),
  type = c("complete", "I", "II"),
  
  reps = 1000,                        # Repetitions
  write = TRUE,                    # Writing to a file
  name = "Simul/simuls2",           # Name of the file
  nCores = 1,                      # Number of cores to use
  
  expression = {
    # Obtain decision:
    PVALUE <- simul_one(mu=mu, n=n, dist=dist, type=type, p_cens=p_cens)
    # Decision:
    no_dist <- ifelse(dist=="EXP", "LIN", "EXP")
    decision <- ifelse(PVALUE >= 0.05, dist, no_dist)
    
    # Results list:
    Results <- list(
      decision = decision
    )
    
    # Return:
    Results
  }
)


# To load the results -----------------------------------------------------

datos <- read.table("Simul/simuls1.txt", header = TRUE)

library(dplyr)

# To convert some variables to factors
datos <- datos |> mutate(mu = factor(mu))

# To summarise pcs for each case
res_comp <- datos %>% 
  filter(type == "complete") %>% 
  group_by(mu, n, dist, type, p_cens) %>% 
  summarise(pcs=mean(dist == decision))

res_I_01 <- datos %>% 
  filter(type == "I" & p_cens == 0.1) %>% 
  group_by(mu, n, dist, type, p_cens) %>% 
  summarise(pcs=mean(dist == decision))

res_I_02 <- datos %>% 
  filter(type == "I" & p_cens == 0.2) %>% 
  group_by(mu, n, dist, type, p_cens) %>% 
  summarise(pcs=mean(dist == decision))

res_II_01 <- datos %>% 
  filter(type == "II" & p_cens == 0.1) %>% 
  group_by(mu, n, dist, type, p_cens) %>% 
  summarise(pcs=mean(dist == decision))

res_II_02 <- datos %>% 
  filter(type == "II" & p_cens == 0.2) %>% 
  group_by(mu, n, dist, type, p_cens) %>% 
  summarise(pcs=mean(dist == decision))

# To plot
library(ggplot2)

p0 <- ggplot(res_comp, aes(x=n, y=pcs, group=mu)) +
  geom_line(aes(color=mu)) +
  geom_point(aes(color=mu)) +
  guides(color = guide_legend(title=expression(mu))) +
  facet_wrap(~ dist, ncol = 2) + 
  ylim(0, 1) + 
  ggtitle("PCS for a random sample complete")

p0

p1 <- ggplot(res_I_01, aes(x=n, y=pcs, group=mu)) +
  geom_line(aes(color=mu)) +
  geom_point(aes(color=mu)) +
  guides(color = guide_legend(title=expression(mu))) +
  facet_wrap(~ dist, ncol = 2) + 
  ylim(0, 1) + 
  ggtitle("PCS for a random sample with type I censoring and 10% of censoring")

p1

p2 <- ggplot(res_I_02, aes(x=n, y=pcs, group=mu)) +
  geom_line(aes(color=mu)) +
  geom_point(aes(color=mu)) +
  guides(color = guide_legend(title=expression(mu))) +
  facet_wrap(~ dist, ncol = 2) + 
  ylim(0, 1) + 
  ggtitle("PCS for a random sample with type I censoring and 20% of censoring")

p2

p3 <- ggplot(res_II_01, aes(x=n, y=pcs, group=mu)) +
  geom_line(aes(color=mu)) +
  geom_point(aes(color=mu)) +
  guides(color = guide_legend(title=expression(mu))) +
  facet_wrap(~ dist, ncol = 2) + 
  ylim(0, 1) + 
  ggtitle("PCS for a random sample with type II censoring and 10% of censoring")

p3

p4 <- ggplot(res_II_02, aes(x=n, y=pcs, group=mu)) +
  geom_line(aes(color=mu)) +
  geom_point(aes(color=mu)) +
  guides(color = guide_legend(title=expression(mu))) +
  facet_wrap(~ dist, ncol = 2) + 
  ylim(0, 1) + 
  ggtitle("PCS for a random sample with type II censoring and 20% of censoring")

p4




# In this script you can find the R code to replicate
# Figure 1 
# Illustration of hazard rate function shapes corresponding to 
# different probability distributions:

library(ggplot2)
library(dplyr)

# Create sequence of time points (using positive values to 
avoid issues with some distributions)
t <- seq(0.01, 25, by=0.1)

# Function to calculate hazard rate for different distributions

# Weibull hazard: h(t)=(k/lambda) * (t/lambda)^(k-1)
weibull_hazard <- function(t, shape, scale) {
  (shape/scale) * (t/scale)^(shape-1)
}

# Hjorth hazard: h(t)=f(t)/(1-F(t)) where f is PDF and F is CDF
library(rmutil)
Hjorth_hazard <- function(t, m, s, f) {
  dhjorth(t, m, s, f) / 
    (1-phjorth(t, m, s, f))
}

# Gamma hazard: h(t)=f(t)/(1-F(t)) where f is PDF and F is CDF
gamma_hazard <- function(t, shape, scale) {
  dgamma(t, shape=shape, scale=scale) /
    (1 - pgamma(t, shape=shape, scale=scale))
}

# Create data frame with different hazard rate functions
hazard_data <- data.frame(
  time=rep(t, 5),
  type=factor(rep(c("Constant (Exponential)",
                      "Increasing (Weibull)",
                      "Decreasing (Gamma)",
                      "Bathtub (Hjorth)",
                      "Hump-shaped (Lognormal)"),
                    each=length(t))),
  
  hazard=c(
    # Constant hazard (Exponential distribution)
    rep(0.5, length(t)),
    # Increasing hazard (Weibull with shape > 1)
    weibull_hazard(t, shape=2.5, scale=2),
    # Decreasing hazard (Gamma with shape < 1)
    gamma_hazard(t, shape=0.5, scale=2),
    # Bathtub hazard (Hjorth with m=4, s=100, f=10)
    #0.5 * exp(-0.8 * t) + 0.2 + 0.1 * t,
    Hjorth_hazard(t, m=4, s=100, f=10),
    # Hump-shaped hazard (Lognormal approximation)
    dlnorm(t, meanlog=1, sdlog=0.5) /
      (1 - plnorm(t, meanlog=1, sdlog=0.5))
  )
)

# Create the plot
p_hr <- ggplot(hazard_data, aes(x=time, y=hazard, color=type)) +
  geom_line(size=1.2) +
  #theme_minimal() +
  labs(title="Hazard rate function for different 
       probability distributions",
       x="Time",
       y="Hazard Rate",
       color="Distribution") +
  theme(legend.position="right",
        plot.title=element_text(hjust=0.5, 
                                  size=14, 
                                  face="bold"),
        axis.title=element_text(size=12),
        legend.title=element_text(size=12),
        legend.text=element_text(size=10)) +
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim=c(0, 2.5)) 

p_hr

ggsave("Plots/hrfs.pdf", plot=p_hr, width=10, height=7)

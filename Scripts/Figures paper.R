
library(ggplot2)
library(patchwork)
library(dplyr)
library(RelDists)
library(gamlss)


# Figure 1 ----------------------------------------------------------------

# Define parameters
scale_param  <- 100 # Scale parameter (theta) (it was 1 originally)
shape_param1 <- 1   # Shape parameter for the first Gamma distribution
shape_param2 <- 1.5 # Shape parameter for the second Gamma distribution

# Generate data for the plots
x <- seq(200, 2000, length.out=1000) # X-axis values originally from 0 to 2 seconds

# CDFs of the Gamma distributions
cdf_gamma1 <- pgamma(x, shape=shape_param1, scale=scale_param)
cdf_gamma2 <- pgamma(x, shape=shape_param2, scale=scale_param)

# Hazard rate functions of the Gamma distributions
hazard_gamma1 <- dgamma(x, shape=shape_param1, scale=scale_param) / (1 - pgamma(x, shape=shape_param1, scale=scale_param))
hazard_gamma2 <- dgamma(x, shape=shape_param2, scale=scale_param) / (1 - pgamma(x, shape=shape_param2, scale=scale_param))

# Create data frames for plotting
cdf_data <- data.frame(
  x=rep(x, 2),
  cdf=c(cdf_gamma1, cdf_gamma2),
  Distribution=rep(c("Gamma(1, 100)", 
                     "Gamma(1.5, 100)"), each=length(x))
)

hazard_data <- data.frame(
  x=rep(x, 2),
  hazard=c(hazard_gamma1, hazard_gamma2),
  Distribution=rep(c("Gamma(1, 100)", "Gamma(1.5, 100)"), 
                     each=length(x))
)

# Create the plots
cdf_plot <- ggplot(cdf_data, aes(x=x, y=cdf, color=Distribution)) +
  geom_line(size=1.5) +
  labs(title="CDFs of Gamma Distributions", 
       x="Time (miliseconds)", 
       y="CDF") +
  theme(legend.position='none')

hazard_plot <- ggplot(hazard_data, aes(x=x, y=hazard, color=Distribution)) +
  geom_line(size=1.5) +
  labs(title="Hazard Rate Functions of Gamma Distributions",
       x="Time (miliseconds)", y="Hazard Rate") +
  theme(legend.position='none')

# Combine the plots in one row and two columns
combined_plot <- cdf_plot + hazard_plot &
  theme(legend.position="bottom")  # Place the legend at the bottom

# Extract the legend as a grob (graphical object)
legend <- cowplot::get_legend(cdf_plot + theme(legend.position="bottom"))

pa <- combined_plot +
  patchwork::plot_layout(guides="collect") &  # Collect the guides (legends)
  patchwork::plot_annotation(theme=theme(legend.position="bottom")) +  # Position the legend at the bottom
  inset_element(legend, left=0.5, bottom=0, right=0.5, top=0.1)  # Add the legend below

pa

###

# Define parameters
scale_param <- 1  # Scale parameter (theta) (it was 1 originally)
shape_param1 <- 1  # Shape parameter for the first Gamma distribution
shape_param2 <- 1.5  # Shape parameter for the second Gamma distribution

# Generate data for the plots
x <- seq(0, 2, length.out=1000)  # X-axis values originally from 0 to 2 seconds

# CDFs of the Gamma distributions
cdf_gamma1 <- pgamma(x, shape=shape_param1, scale=scale_param)
cdf_gamma2 <- pgamma(x, shape=shape_param2, scale=scale_param)

# Hazard rate functions of the Gamma distributions
hazard_gamma1 <- dgamma(x, shape=shape_param1, scale=scale_param) / (1 - pgamma(x, shape=shape_param1, scale=scale_param))
hazard_gamma2 <- dgamma(x, shape=shape_param2, scale=scale_param) / (1 - pgamma(x, shape=shape_param2, scale=scale_param))

# Create data frames for plotting
cdf_data <- data.frame(
  x=rep(x, 2),
  cdf=c(cdf_gamma1, cdf_gamma2),
  Distribution=rep(c("Gamma(1, 1)", "Gamma(1.5, 1)"), each=length(x))
)

hazard_data <- data.frame(
  x=rep(x, 2),
  hazard=c(hazard_gamma1, hazard_gamma2),
  Distribution=rep(c("Gamma(1, 1)", "Gamma(1.5, 1)"), each=length(x))
)

# Create the plots
cdf_plot <- ggplot(cdf_data, aes(x=x, y=cdf, color=Distribution)) +
  geom_line(size=1.5) +
  labs(title="CDFs of Gamma Distributions", x="Time (seconds)", y="CDF") +
  theme(legend.position='none')

hazard_plot <- ggplot(hazard_data, aes(x=x, y=hazard, color=Distribution)) +
  geom_line(size=1.5) +
  labs(title="Hazard Rate Functions of Gamma Distributions",
       x="Time (seconds)", y="Hazard Rate") +
  theme(legend.position='none')

# Combine the plots in one row and two columns
combined_plot <- cdf_plot + hazard_plot &
  theme(legend.position="bottom")  # Place the legend at the bottom

# Extract the legend as a grob (graphical object)
legend <- cowplot::get_legend(cdf_plot + theme(legend.position="bottom"))

pb <- combined_plot +
  patchwork::plot_layout(guides="collect") &  # Collect the guides (legends)
  patchwork::plot_annotation(theme=theme(legend.position="bottom")) +  # Position the legend at the bottom
  inset_element(legend, left=0.5, bottom=0, right=0.5, top=0.1)  # Add the legend below

final.plot <- pa / pb
final.plot

ggsave("mock_example.pdf", plot=final.plot, width=10, height=10)

# Densities ---------------------------------------------------------------

p1 <- ggplot() + 
  geom_function(aes(colour="Lin(0.1)"), fun=dLIN, 
                lwd=1.5, args=list(mu=0.1)) +
  geom_function(aes(colour="Exp(0.052)"), fun=dEXP, 
                lwd=1, args=list(mu=1/0.052)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 120) +  
  labs(x="x", y="Density")

p1

p2 <- ggplot() + 
  geom_function(aes(colour="Lin(1.3)"), fun=dLIN, 
                lwd=1.5, args=list(mu=1.3)) +
  geom_function(aes(colour="Exp(0.906)"), fun=dEXP, 
                lwd=1, args=list(mu=1/0.906)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 8) + 
  labs(x="x", y="Density")

p2

p3 <- ggplot() + 
  geom_function(aes(colour="Lin(2.5)"), fun=dLIN, 
                lwd=1.5, args=list(mu=2.5)) +
  geom_function(aes(colour="Exp(1.944)"), fun=dEXP, 
                lwd=1, args=list(mu=1/1.944)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 8) +
  labs(x="x", y="Density")

p3

p4 <- ggplot() + 
  geom_function(aes(colour="Lin(0.184)"), fun=dLIN, 
                lwd=1.5, args=list(mu=0.184)) +
  geom_function(aes(colour="Exp(0.1)"), fun=dEXP, 
                lwd=1, args=list(mu=1/0.1)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 60) +
  labs(x="x", y="Density")

p4

p5 <- ggplot() + 
  geom_function(aes(colour="Lin(1.769)"), fun=dLIN, 
                lwd=1.5, args=list(mu=1.769)) +
  geom_function(aes(colour="Exp(1.3)"), fun=dEXP, 
                lwd=1, args=list(mu=1/1.3)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 6) + 
  labs(x="x", y="Density")

p5

p6 <- ggplot() + 
  geom_function(aes(colour="Lin(3.108)"), fun=dLIN, 
                lwd=1.5, args=list(mu=3.108)) +
  geom_function(aes(colour="Exp(2.5)"), fun=dEXP, 
                lwd=1, args=list(mu=1/2.5)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 3) + 
  labs(x="x", y="Density")

p6


p10 <- (p1 + p2 + p3) / (p4 + p5 + p6)
p10
ggsave("Plots/Density.pdf", plot=p10, width=10, height=7)


# Cumulatives -------------------------------------------------------------

p1 <- ggplot() + 
  geom_function(aes(colour="Lin(0.1)"), fun=pLIN, 
                lwd=1.5, args=list(mu=0.1)) +
  geom_function(aes(colour="Exp(0.052)"), fun=pEXP, 
                lwd=1, args=list(mu=1/0.052)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 120) + ylim(0, 1) + 
  labs(x="x", y="Cumulative")

p2 <- ggplot() + 
  geom_function(aes(colour="Lin(1.3)"), fun=pLIN, 
                lwd=1.5, args=list(mu=1.3)) +
  geom_function(aes(colour="Exp(0.906)"), fun=pEXP, 
                lwd=1, args=list(mu=1/0.906)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 8) + ylim(0, 1) + 
  labs(x="x", y="Cumulative")

p3 <- ggplot() + 
  geom_function(aes(colour="Lin(2.5)"), fun=pLIN, 
                lwd=1.5, args=list(mu=2.5)) +
  geom_function(aes(colour="Exp(1.944)"), fun=pEXP, 
                lwd=1, args=list(mu=1/1.944)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 8) + ylim(0, 1) + 
  labs(x="x", y="Cumulative")

p4 <- ggplot() + 
  geom_function(aes(colour="Lin(0.184)"), fun=pLIN, 
                lwd=1.5, args=list(mu=0.184)) +
  geom_function(aes(colour="Exp(0.1)"), fun=pEXP, 
                lwd=1, args=list(mu=1/0.1)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 60) + ylim(0, 1) + 
  labs(x="x", y="Cumulative")

p5 <- ggplot() + 
  geom_function(aes(colour="Lin(1.769)"), fun=pLIN, 
                lwd=1.5, args=list(mu=1.769)) +
  geom_function(aes(colour="Exp(1.3)"), fun=pEXP, 
                lwd=1, args=list(mu=1/1.3)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 6) + ylim(0, 1) + 
  labs(x="x", y="Cumulative")

p6 <- ggplot() + 
  geom_function(aes(colour="Lin(3.108)"), fun=pLIN, 
                lwd=1.5, args=list(mu=3.108)) +
  geom_function(aes(colour="Exp(2.5)"), fun=pEXP, 
                lwd=1, args=list(mu=1/2.5)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 3) + ylim(0, 1) + 
  labs(x="x", y="Cumulative")


p10 <- (p1 + p2 + p3) / (p4 + p5 + p6)
p10
ggsave("Plots/Cumulative.pdf", plot=p10, width=10, height=7)



# Hazard ------------------------------------------------------------------

haz_lin <- function(x, mu) dLIN(x, mu)   / (1 - pLIN(x, mu))
haz_exp <- function(x, mu) mu
haz_exp <- Vectorize(haz_exp)

p1 <- ggplot() + 
  geom_function(aes(colour="Lin(0.1)"), fun=haz_lin, 
                lwd=1, args=list(mu=0.1)) +
  geom_function(aes(colour="Exp(0.052)"), fun=haz_exp, 
                lwd=1, args=list(mu=0.052)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 10) +
  labs(x="x", y="Hazard")

p1

p2 <- ggplot() + 
  geom_function(aes(colour="Lin(1.3)"), fun=haz_lin, 
                lwd=1, args=list(mu=1.3)) +
  geom_function(aes(colour="Exp(0.906)"), fun=haz_exp, 
                lwd=1, args=list(mu=0.906)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 10) +
  labs(x="x", y="Hazard")

p2

p3 <- ggplot() + 
  geom_function(aes(colour="Lin(2.5)"), fun=haz_lin, 
                lwd=1, args=list(mu=2.5)) +
  geom_function(aes(colour="Exp(1.944)"), fun=haz_exp, 
                lwd=1, args=list(mu=1.944)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 10) + 
  labs(x="x", y="Hazard")

p3

p4 <- ggplot() + 
  geom_function(aes(colour="Lin(0.184)"), fun=haz_lin, 
                lwd=1, args=list(mu=0.184)) +
  geom_function(aes(colour="Exp(0.1)"), fun=haz_exp, 
                lwd=1, args=list(mu=0.1)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 10) + 
  labs(x="x", y="Hazard")

p4

p5 <- ggplot() + 
  geom_function(aes(colour="Lin(1.769)"), fun=haz_lin, 
                lwd=1.5, args=list(mu=1.769)) +
  geom_function(aes(colour="Exp(1.3)"), fun=haz_exp, 
                lwd=1, args=list(mu=1.3)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 10) + 
  labs(x="x", y="Hazard")

p5

p6 <- ggplot() + 
  geom_function(aes(colour="Lin(3.108)"), fun=haz_lin, 
                lwd=1.5, args=list(mu=3.108)) +
  geom_function(aes(colour="Exp(2.5)"), fun=haz_exp, 
                lwd=1, args=list(mu=2.5)) +
  guides(color=guide_legend(title="")) +
  theme(legend.position="top") +
  xlim(0, 10) +
  labs(x="x", y="Hazard")

p6


p10 <- (p1 + p2 + p3) / (p4 + p5 + p6)
p10
ggsave("Plots/Hazard.pdf", plot=p10, width=10, height=7)


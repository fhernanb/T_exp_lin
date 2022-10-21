
library(ggplot2)
library(patchwork)
library(RelDists)
library(gamlss)


# Densities ---------------------------------------------------------------

p1 <- ggplot() + 
  geom_function(aes(colour="Lin(0.1)"), fun=dLIN, 
                lwd=1.5, args=list(mu=0.1)) +
  geom_function(aes(colour="Exp(0.052)"), fun=dEXP, 
                lwd=1, args=list(mu=1/0.052)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 120) +  
  labs(x="x", y="Density")

p1

p2 <- ggplot() + 
  geom_function(aes(colour="Lin(1.3)"), fun=dLIN, 
                lwd=1.5, args=list(mu=1.3)) +
  geom_function(aes(colour="Exp(0.906)"), fun=dEXP, 
                lwd=1, args=list(mu=1/0.906)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 8) + 
  labs(x="x", y="Density")

p2

p3 <- ggplot() + 
  geom_function(aes(colour="Lin(2.5)"), fun=dLIN, 
                lwd=1.5, args=list(mu=2.5)) +
  geom_function(aes(colour="Exp(1.944)"), fun=dEXP, 
                lwd=1, args=list(mu=1/1.944)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 8) +
  labs(x="x", y="Density")

p3

p4 <- ggplot() + 
  geom_function(aes(colour="Lin(0.184)"), fun=dLIN, 
                lwd=1.5, args=list(mu=0.184)) +
  geom_function(aes(colour="Exp(0.1)"), fun=dEXP, 
                lwd=1, args=list(mu=1/0.1)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 60) +
  labs(x="x", y="Density")

p4

p5 <- ggplot() + 
  geom_function(aes(colour="Lin(1.769)"), fun=dLIN, 
                lwd=1.5, args=list(mu=1.769)) +
  geom_function(aes(colour="Exp(1.3)"), fun=dEXP, 
                lwd=1, args=list(mu=1/1.3)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 6) + 
  labs(x="x", y="Density")

p5

p6 <- ggplot() + 
  geom_function(aes(colour="Lin(3.108)"), fun=dLIN, 
                lwd=1.5, args=list(mu=3.108)) +
  geom_function(aes(colour="Exp(2.5)"), fun=dEXP, 
                lwd=1, args=list(mu=1/2.5)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 3) + 
  labs(x="x", y="Density")

p6


p10 <- (p1 + p2 + p3) / (p4 + p5 + p6)
p10
ggsave("Plots/Density.pdf", plot = p10, width = 10, height = 7)



# Cumulatives -------------------------------------------------------------

p1 <- ggplot() + 
  geom_function(aes(colour="Lin(0.1)"), fun=pLIN, 
                lwd=1.5, args=list(mu=0.1)) +
  geom_function(aes(colour="Exp(0.052)"), fun=pEXP, 
                lwd=1, args=list(mu=1/0.052)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 120) + ylim(0, 1) + 
  labs(x="x", y="Cumulative")

p2 <- ggplot() + 
  geom_function(aes(colour="Lin(1.3)"), fun=pLIN, 
                lwd=1.5, args=list(mu=1.3)) +
  geom_function(aes(colour="Exp(0.906)"), fun=pEXP, 
                lwd=1, args=list(mu=1/0.906)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 8) + ylim(0, 1) + 
  labs(x="x", y="Cumulative")

p3 <- ggplot() + 
  geom_function(aes(colour="Lin(2.5)"), fun=pLIN, 
                lwd=1.5, args=list(mu=2.5)) +
  geom_function(aes(colour="Exp(1.944)"), fun=pEXP, 
                lwd=1, args=list(mu=1/1.944)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 8) + ylim(0, 1) + 
  labs(x="x", y="Cumulative")

p4 <- ggplot() + 
  geom_function(aes(colour="Lin(0.184)"), fun=pLIN, 
                lwd=1.5, args=list(mu=0.184)) +
  geom_function(aes(colour="Exp(0.1)"), fun=pEXP, 
                lwd=1, args=list(mu=1/0.1)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 60) + ylim(0, 1) + 
  labs(x="x", y="Cumulative")

p5 <- ggplot() + 
  geom_function(aes(colour="Lin(1.769)"), fun=pLIN, 
                lwd=1.5, args=list(mu=1.769)) +
  geom_function(aes(colour="Exp(1.3)"), fun=pEXP, 
                lwd=1, args=list(mu=1/1.3)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 6) + ylim(0, 1) + 
  labs(x="x", y="Cumulative")

p6 <- ggplot() + 
  geom_function(aes(colour="Lin(3.108)"), fun=pLIN, 
                lwd=1.5, args=list(mu=3.108)) +
  geom_function(aes(colour="Exp(2.5)"), fun=pEXP, 
                lwd=1, args=list(mu=1/2.5)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 3) + ylim(0, 1) + 
  labs(x="x", y="Cumulative")


p10 <- (p1 + p2 + p3) / (p4 + p5 + p6)
p10
ggsave("Plots/Cumulative.pdf", plot = p10, width = 10, height = 7)



# Hazard ------------------------------------------------------------------

haz_lin <- function(x, mu) dLIN(x, mu)   / (1 - pLIN(x, mu))
haz_exp <- function(x, mu) mu
haz_exp <- Vectorize(haz_exp)

p1 <- ggplot() + 
  geom_function(aes(colour="Lin(0.1)"), fun=haz_lin, 
                lwd=1, args=list(mu=0.1)) +
  geom_function(aes(colour="Exp(0.052)"), fun=haz_exp, 
                lwd=1, args=list(mu=0.052)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 10) +
  labs(x="x", y="Hazard")

p1

p2 <- ggplot() + 
  geom_function(aes(colour="Lin(1.3)"), fun=haz_lin, 
                lwd=1, args=list(mu=1.3)) +
  geom_function(aes(colour="Exp(0.906)"), fun=haz_exp, 
                lwd=1, args=list(mu=0.906)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 10) +
  labs(x="x", y="Hazard")

p2

p3 <- ggplot() + 
  geom_function(aes(colour="Lin(2.5)"), fun=haz_lin, 
                lwd=1, args=list(mu=2.5)) +
  geom_function(aes(colour="Exp(1.944)"), fun=haz_exp, 
                lwd=1, args=list(mu=1.944)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 10) + 
  labs(x="x", y="Hazard")

p3

p4 <- ggplot() + 
  geom_function(aes(colour="Lin(0.184)"), fun=haz_lin, 
                lwd=1, args=list(mu=0.184)) +
  geom_function(aes(colour="Exp(0.1)"), fun=haz_exp, 
                lwd=1, args=list(mu=0.1)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 10) + 
  labs(x="x", y="Hazard")

p4

p5 <- ggplot() + 
  geom_function(aes(colour="Lin(1.769)"), fun=haz_lin, 
                lwd=1.5, args=list(mu=1.769)) +
  geom_function(aes(colour="Exp(1.3)"), fun=haz_exp, 
                lwd=1, args=list(mu=1.3)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 10) + 
  labs(x="x", y="Hazard")

p5

p6 <- ggplot() + 
  geom_function(aes(colour="Lin(3.108)"), fun=haz_lin, 
                lwd=1.5, args=list(mu=3.108)) +
  geom_function(aes(colour="Exp(2.5)"), fun=haz_exp, 
                lwd=1, args=list(mu=2.5)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 10) +
  labs(x="x", y="Hazard")

p6


p10 <- (p1 + p2 + p3) / (p4 + p5 + p6)
p10
ggsave("Plots/Hazard.pdf", plot = p10, width = 10, height = 7)


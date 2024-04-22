
library(ggplot2)
library(patchwork)
library(RelDists)
library(gamlss)


# Densities ---------------------------------------------------------------

p1 <- ggplot() + 
  geom_function(aes(colour="Lin(0.1)"), fun=dLIN, 
                lwd=1.5, args=list(mu=0.1)) +
  geom_function(aes(colour="Exp(0.1)"), fun=dexp, 
                lwd=1, args=list(rate=0.1)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 20) +  
  labs(x="x", y="Density", title="A")

p1

p2 <- ggplot() + 
  geom_function(aes(colour="Lin(0.5)"), fun=dLIN, 
                lwd=1.5, args=list(mu=0.5)) +
  geom_function(aes(colour="Exp(0.5)"), fun=dexp, 
                lwd=1, args=list(rate=0.5)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 20) + 
  labs(x="x", y="Density", title="B")

p2

p3 <- ggplot() + 
  geom_function(aes(colour="Lin(0.9)"), fun=dLIN, 
                lwd=1.5, args=list(mu=0.9)) +
  geom_function(aes(colour="Exp(0.9)"), fun=dexp, 
                lwd=1, args=list(rate=0.9)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 20) +
  labs(x="x", y="Density", title="C")

p3

p4 <- ggplot() + 
  geom_function(aes(colour="Lin(1.3)"), fun=dLIN, 
                lwd=1.5, args=list(mu=1.3)) +
  geom_function(aes(colour="Exp(1.3)"), fun=dexp, 
                lwd=1, args=list(rate=1.3)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 20) +
  labs(x="x", y="Density", title="D")

p4

p5 <- ggplot() + 
  geom_function(aes(colour="Lin(1.5)"), fun=dLIN, 
                lwd=1.5, args=list(mu=1.5)) +
  geom_function(aes(colour="Exp(1.5)"), fun=dexp, 
                lwd=1, args=list(rate=1.5)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 20) + 
  labs(x="x", y="Density", title="E")

p5

p6 <- ggplot() + 
  geom_function(aes(colour="Lin(2.0)"), fun=dLIN, 
                lwd=1.5, args=list(mu=2.0)) +
  geom_function(aes(colour="Exp(2.0)"), fun=dexp, 
                lwd=1, args=list(rate=2.0)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 20) + 
  labs(x="x", y="Density", title="F")

p6

p7 <- ggplot() + 
  geom_function(aes(colour="Lin(2.5)"), fun=dLIN, 
                lwd=1.5, args=list(mu=2.5)) +
  geom_function(aes(colour="Exp(2.5)"), fun=dexp, 
                lwd=1, args=list(rate=2.5)) +
  guides(color = guide_legend(title="")) +
  theme(legend.position = "top") +
  xlim(0, 20) + 
  labs(x="x", y="Density", title="G")

p7


p10 <- (p1 + p2 + p3) / (p4 + p5 + p6) / p2
p10
ggsave("Plots/Density_to_simul.pdf", plot = p10, width = 10, height = 7)


#Wrap
#Layout
layout <- '
ABC
DEF
#G#
'
#Plot
p10 <- wrap_plots(A=p1, B=p2, C=p3, D=p4, E=p5, F=p6, g=p7,
           design = layout)
ggsave("Plots/Density_to_simul.pdf", plot = p10, width = 10, height = 12)

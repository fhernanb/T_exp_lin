library("parSim")

# Some function we might use:
obtain_one_T <- function(dist, n, beta0) {
  x1 <- runif(n=n)
  x2 <- runif(n=n)
  eta <- beta0 + 2 * x1 - 3 * x2
  mu <- exp(eta)
  if (dist == "EXP")
    y <- rEXP(n=n, mu=mu)
  else 
    y <- rLIN(n=n, mu=mu)
  datos <- data.frame(y=y, x1=x1, x2=x2)
  
  # To fit both models
  mod1 <- gamlss(y ~ x1 + x2, family=LIN, data=datos,
                 control=gamlss.control(trace=FALSE))
  mod2 <- gamlss(y ~ x1 + x2, family=EXP, data=datos,
                 control=gamlss.control(trace=FALSE))
  # To obtain T
  T <- as.numeric(logLik(mod2) - logLik(mod1))
  return(T)
}

# Run the simulation:
Results <- parSim(
  # Any number of conditions:
  dist = c("EXP", "LIN"),
  n = seq(from=10, to=700, by=30),
  beta0 = c(-1.8, -0.2, 0.4, 0.8, 0.9, 1.2, 1.4),
  
  # should a progress bar be shown
  progressbar = FALSE,
  
  # Number of repititions?
  reps = 1000,
  
  # Parallel?
  nCores = 4,
  
  # Write to file?
  write = TRUE,
  
  # Name of file:
  name = "T_for_reg_mod",
  
  # Export objects (only needed when nCores > 1):
  export = c("obtain_one_T"),
  
  # R expression:
  expression = {
    # Load all R packages in the expression if needed
    library(gamlss)
    library(RelDists)
    
    # Want to debug? Enter browser() and run the function. Only works with nCores = 1!
    # browser()
    
    # Enter whatever codes you want. I can use the conditions as objects.
    T <- obtain_one_T(dist=dist, n=n, beta0=beta0)
    
    # Make a data frame with one row to return results (multple rows is also possible but not reccomended):
    data.frame(t=T)
  }
)

datos <- read.table("T_for_reg_mod.txt", header = TRUE)
dim(datos)
head(datos)


library(dplyr)
library(magrittr)

# Adding the variable mu
datos$mu <- round(exp(datos$beta0 + 2 * 0.5 -3 * 0.5), digits=1)
head(datos)

# If Exp then pcs = mean(T > 0)
# if Lin then pcs = mean(T <= 0)
# so we do converting T for Lin multiplying by -1
datos %<>% mutate(new_t = ifelse(dist == "LIN", t <= 0, t > 0))

datos %>% 
  group_by(dist, n, mu) %>% 
  summarise(pcs = mean(new_t)) -> res

res

library(ggplot2)
library(animation)

## make sure ImageMagick has been installed in your system
saveGIF(
  movie.name = "animation_pcs_for_mu.gif",
  expr = {
  mus <- unique(res$mu)
  for (the_mu in mus) {
    sub_res <- res %>% filter(mu == the_mu)
    print(
      ggplot(sub_res, aes(x=n, y=pcs, colour=dist)) +
        geom_line() +
        labs(y = "Probability of correct model selection (PCS)",
             x = "Sample size (n)",
             title = bquote(mu == .(the_mu))) +
        ylim(0, 1)
    )
  }
})


# El siguiente codigo es para consolidar los datos simulados

d1 <- read.table("T_for_reg_mod01.txt", header = TRUE)
d2 <- read.table("T_for_reg_mod02.txt", header = TRUE)

d3 <- dplyr::bind_rows(d1, d2)

write(t(d3), file="T_for_reg_mod.txt", ncolumns=8)




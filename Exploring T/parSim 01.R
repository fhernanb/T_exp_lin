library("parSim")

# Some function we might use:
bias <- function(x,y){abs(x-y)}
my_rnorm <- function(n) rnorm(n=n, mean=170, sd=2)

# Run the simulation:
Results <- parSim(
  # Any number of conditions:
  sampleSize = c(50, 100, 250),
  beta = c(0, 0.5, 1),
  sigma = c(0.25, 0.5, 1),
  
  # Number of repititions?
  reps = 5,
  
  # Parallel?
  nCores = 16,
  
  # Write to file?
  write = TRUE,
  
  # Name of file:
  name = "hola",
  
  # Export objects (only needed when nCores > 1):
  export = c("bias", "my_rnorm"),
  
  # R expression:
  expression = {
    # Load all R packages in the expression if needed
    # library(...)
    
    # Want to debug? Enter browser() and run the function. Only works with nCores = 1!
    # browser()
    
    # Enter whatever codes you want. I can use the conditions as objects.
    X <- my_rnorm(sampleSize)
    Y <- beta * X + rnorm(sampleSize, sigma)
    fit <- lm(Y ~ X)
    betaEst <- coef(fit)[2]
    Rsquared <- summary(fit)$r.squared
    
    # Make a data frame with one row to return results (multple rows is also possible but not reccomended):
    data.frame(
      betaEst = betaEst,
      bias = bias(beta,betaEst),
      Rsquared = Rsquared
    )
  }
)

datos <- read.table("hola.txt", header = TRUE)
head(datos)


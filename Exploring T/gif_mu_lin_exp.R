
library(animation)
## make sure ImageMagick has been installed in your system
saveGIF({
  mus <- seq(from=0.5, to=15, by=0.5)
  #mus <- c(0.1, 0.5, 0.9, 1.3, 1.5, 2, 2.5)
  for (mu in mus) {
    curve(dEXP(x, mu=mu), from=0, to=30, col="red", ylim=c(0, 1),
          ylab="Density", main=bquote(mu == .(mu)), las=1)
    curve(dLIN(x, mu=mu), from=0, to=30, col="blue", add=TRUE)
    legend("topright", legend=c("Exp", "Lin"), lty=1, col=c("red", "blue"))
    
    media_exp <- round(1/mu, digits=3)
    varia_exp <- round(1/mu^2, digits=3)
    media_lin <- round((mu+2) / (mu*(mu+1)), digits=3)
    varia_lin <- round(2 * (mu + 3) / (mu^2 * (mu + 1)) - ((mu + 2) / (mu * (mu + 1)))^2, digits=3)
    
    legend(x=7, y=0.8, bty="n", lty=1, col="red",
           legend=paste("Exp: mean=", media_exp, " and var=", varia_exp, 
                        collapse="", sep=""))
    
    legend(x=7, y=0.7, bty="n", lty=1, col="blue",
           legend=paste("Lin: mean=", media_lin, " and var=", varia_lin, 
                        collapse="", sep=""))
  }
})


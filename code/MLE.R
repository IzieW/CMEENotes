## Graphics for JPNB MLE study guide

require(tidyverse)

pmf_bar <- function(){
  # Make pmf plots
coin <- c(0.5, 0.5)
dice <- rep(1/6, 6)

par(mfrow=c(1, 2))

barplot(coin, 
        main = "PMF of fair coin", 
        names.arg = c("heads", "tails"), 
        xlab = "outcomes", 
        ylab = "probability", 
        ylim=c(0, 1))
barplot(dice, 
         main = "PMF six-sided dice",
         names.arg = seq(1, 6, 1),
         xlab = "outcomes",
         ylab = "probability",
         ylim=c(0,1))
}

## PMF/PDF FUNCTIONS
bernoulli <- function(x, p){
  # Paramters: p= probability of success
  # x = outcome
  pmf <- p^x*(1-p)^(1-x)
  return(pmf)
}

binomial <- function(x, n, p){
  # Parameters: n= total trials, p= probability (same for each)
  # x = outcome
  bc <- factorial(n)/(factorial(x)*factorial(n-x))
  pmf <- bc*p^x*(1-p)^(1-x)
  
  return(pmf)
}

poisson <- function(x, lambda){
  # x= outcome
  # lambda= "rate" how frequently event occures in set time interval
  
  pmf <- (lambda^x*exp(-lambda))/factorial(x)
  
  return(pmf)
}


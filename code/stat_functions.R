### Basic Statistical Functions in R

get_se <- function(data){
  # Return standard error of input data vector
  sd(data)/sqrt(length(data))
}

get_CI <- function(data, interval=1.96){
  # Return 95% CI 
  intervals = c(-1*interval, interval)
  se <- get_se(data)
  CI <- intervals*se+mean(data)
  return(CI)
}

example_variance <- function() {
  # plots variance 
  x <- seq(1, 10, 1)
  y1 <- rnorm(10, 0, 1)
  y2 <- rnorm(10, 0, 10)
  yinterval <- c(min(y2), max(y2))
  
  par(mfrow=c(1, 2))
  plot(x, y1, ylim=yinterval, main = var(y1))
  abline(h=0, col="blue", lty=3)
  plot(x, y2, ylim=yinterval, main=var(y2))
  abline(h=0, col="blue", lty=3)
}

## GLM
psuedoR <- function(model){
  # Return psuedo r-squared value
  1- (as.numeric(model$deviance)/as.numeric(model$null.deviance))
}

get_dispersion <- function(model){
  model$deviance/model$df.residual
}

# Purpose: calculate absolute confidence intervals for Poisson rates

# Author: Matthew Shane Loop

poisson_lower <- function(x, alpha){
  l <- (qchisq(df = 2*x, p = alpha/2))/2
  return(l)
}

poisson_upper <- function(x, alpha){
  u <- (qchisq(df = 2*(x+1), p = (1 - alpha/2)))/2
  return(u)
}
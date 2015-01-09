# Purpose: Exchange a proportion of cases for clusters of cases

# Author: Matthew Shane Loop

cluster_genesis <- function(cases_ppp, q, v, sigma){
  library(spatstat)
  clusters <- ppp()
  for(i in 1:(cases_ppp$n*q)){
    center <- runifpoint(1)
    cluster <- rpoint(v, function(x,y) {1/2/pi/sigma^2*exp((-1)/(2*sigma^2)*((x - center$x)^2 + (y - center$y)^2))})
    clusters <- superimpose(cluster, clusters)
  }
  new_cases <- superimpose(clusters, cases_ppp[-c(1:(cases_ppp$n*q*v))])
  return(new_cases)
}
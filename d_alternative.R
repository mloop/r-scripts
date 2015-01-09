# Purpose: calculate D(h) under alternative hypothesis, with point process created from `create_alternative.R'

# Author: Matthew Shane Loop

d_alternative <- function(h, n, q, v, sigma){
 d <- v*(v - 1)*q/n*(1 - exp(-(h^2)/(4*sigma)^2))
 return(d)
}
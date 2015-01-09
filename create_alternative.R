# Purpose: Create a point process with clustering (under the alternative hypothesis)

# The value of the difference in K functions between the cases and controls should be D(h) = v(v - 1)q/n (1-exp(-(h^2)/(4sigma)^2))

# Author: Matthew Shane Loop

create_alternative <- function(n, q, v, sigma){
  library(spatstat)
  controls <- runifpoint(n)
  cases <- runifpoint(n)
  cases_clusters <- cluster_genesis(cases, q = q, v = v, sigma = sigma)
  marks(cases_clusters) <- 'case'
  marks(controls) <- 'control'
  data <- superimpose(controls, cases_clusters)
  return(data)
}

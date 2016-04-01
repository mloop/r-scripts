# Purpose: calculate the power for an interaction term in a Cox proportional hazards model, where each covariate is binary

# Author: Matthew Shane Loop

# References: Schmoor C, Sauerbrei W, and Schumacher M (2000). "Sample size considerations for the evaluation of prognostic factors in survival analysis." Statistics in Medicine 19:441 - 452.; documentation for powerSurvEpi package on CRAN

cox_interaction_power <- function(sample_size, p_00, p_01, p_10, p_11, hazard_ratio, phi, alpha){
  # p_** is the proportion of participants that have the specified value for covariate 1 and covariate 2
  # phi is the proportion of participants that have the failure of interest
  
  delta <- 1 / p_00 + 1 / p_01 + 1 / p_10 + 1 / p_11
  z <- qnorm(p = 1 - (alpha / 2))
  power <- pnorm(-z + sqrt(sample_size / delta * (log(hazard_ratio)) ^ 2 * phi))
  return(power)
}

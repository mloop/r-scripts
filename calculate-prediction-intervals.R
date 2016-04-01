# Purpose: calculate predicted probabilities, and 95% prediction intervals for those predicted probabilities, for a gam.object

# Author: Matthew Shane Loop

# Warning: This code is not efficient. It takes about 5.5 hours on a desktop PC run this function on a dataset with 78,075 observations and 4 variables.

predict_prevalence <- function(newdata, fitted_object){
  library(mgcv)
  prevalences <- data.frame()
  for(i in 1:nrow(newdata)){
    x_matrix <- predict(fitted_object, newdata = newdata[i, ], type = 'lpmatrix')  # Obtain full design (X) matrix, which contains smooth term function values
    se <- sqrt(x_matrix %*% fitted_object$Vp %*% t(x_matrix))  # Calculate standard error assuming var(Xb) = Xvar(b)X'
    yhat <- predict(fitted_object, newdata = newdata[i, ], type = 'link')
    lower <- yhat[[1]] - 1.96 * se[[1]]
    upper <- yhat[[1]] + 1.96 * se[[1]]
    data <- data.frame(prevalence = exp(yhat[[1]])/(1 + exp(yhat[[1]])), logit_se = se[[1]], lower = exp(lower) / (1 + exp(lower)), upper = exp(upper) / (1 + exp(upper)))  # Transform predicted values and intervals from logit function scale to predicted probability scale
  prevalences <- rbind(prevalences, data)
  }
  return(prevalences)
}

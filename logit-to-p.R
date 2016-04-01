logit_to_p <- function(x){
  y <- exp(x) / (1 + exp(x))
  return(y)
}

logit_to_p(1)

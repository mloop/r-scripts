# Requires a ppp object from spatstat package, as well as data.frame of centroid coordinates with one column labeled 'x' and one column labeled 'y'
 
snap <- function(ppp, centroid_coordinates, prob){
  library(spatstat)
  centroids <- ppp(x = centroid_coordinates$x, y = centroid_coordinates$y)
  nearest_centroid <- nncross(ppp, centroids, what = 'which')
  snap <- rbinom(ppp$n, 1, p = prob)
  coords(ppp) <- coords(centroids[nearest_centroid])*ifelse(snap == 1, 1, 0) + coords(ppp)*ifelse(snap == 0, 1, 0)
  snapped_ppp <- ppp
  return(snapped_ppp)
  }
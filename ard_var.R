# Purpose: calculate variance of an absolute rate difference

# Author: Matthew Shane Loop

ard_var <- function(events1, events2, person_time1, person_time2){
  var <- events1/(person_time1)^2 + events2/(person_time2)^2
  return(var)
}
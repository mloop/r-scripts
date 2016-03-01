# Purpose: to create an "exclusion cascade" dataset for epidemiology analyses

# Author: Matthew Shane Loop

exclusion_cascade <- function(source_dataset, criteria, reason){
  library(dplyr)
  cascade <- data_frame()
  number_of_exclusions <- length(criteria)
  for(i in 1:number_of_exclusions){
  excluded_dataset <- filter_(source_dataset, paste(criteria[1:i], sep = " & ", collapse = " & "))  # Use standard evaluation
  cascade <- rbind(cascade, data_frame(
                   beginning_ss = ifelse(i == 1, 
                                         nrow(source_dataset), 
                                         nrow(filter_(source_dataset, criteria[1:(i - 1)]))),
                   ending_ss = nrow(excluded_dataset),
                   reason = reason[i],
                   number_excluded = NA
                     )) %>%
    mutate(number_excluded = beginning_ss - ending_ss)
  }
  return(cascade)
}

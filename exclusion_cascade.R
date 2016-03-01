# Purpose: to create an "exclusion cascade" dataset for epidemiology analyses

# Author: Matthew Shane Loop
library(dplyr)

data <- data_frame(id = seq(1, 10, 1), bmi = rnorm(10))

data_b <- filter(data, id > 2)

cascade <- data_frame(
  starting_ss = nrow(data),
  ending_ss = nrow(data_b),
  reason = "Missing BMI"
  ) %>%
  mutate(number_excluded = starting_ss - ending_ss)

data_c <- filter(data_b, bmi > 0)
cascade <- rbind(cascade, 
                 data_frame(
                   starting_ss = nrow(data_b),
                   ending_ss = nrow(data_c),
                   reason = "Negative BMI",
                   number_excluded = NA
                   )) %>%
  mutate(number_excluded = starting_ss - ending_ss)

exclusion_cascade <- function(source_dataset, criteria, reason){
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
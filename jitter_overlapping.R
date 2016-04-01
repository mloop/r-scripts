# Purpose: jitter only overlapping points in a point process

# Author: Matthew Shane Loop

jitter_overlapping <- function(ppp, r){
  jittered <- superimpose(ppp[duplicated(ppp, rule == 'unmark') == FALSE],rjitter(ppp[duplicated(ppp, rule = 'unmark') == TRUE], radius = r))
}
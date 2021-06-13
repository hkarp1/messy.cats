#state_replace: Special case of cat_replace. Calls cat_replace on input with clean vector state.name, a vector of the names of all U.S. states

library(dplyr)
library(stringdist)
library(readxl)
library(tidyverse)
library(varhandle)
library(rapportools)



state_replace <- function(messy_states, threshold = NA,
                          method = "jw", q = 1, p = 0, bt = 0,
                          useBytes = FALSE, weight=c(d=1, i=1, t=1)){
  cat_replace(messy_states, state.name, threshold, method = "jw", q, p, bt, useBytes, weight)
}

#Example
lst <- c("Indianaa", "Wisvconsin", "aLaska", "NewJersey", "Claifoarni")
fixed <- state_replace(lst)

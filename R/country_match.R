
library(dplyr)
library(stringdist)
library(readxl)
library(tidyverse)
library(varhandle)
library(rapportools)

country_match <- function(messy_countries,return_dists = FALSE, return_lists = NA,
                        threshold = NA,pick_lists=F, p = 0){
  cat_match(messy_countries,country.names[[1]],method = "jw",return_dists = return_dists,return_lists = return_lists,
            threshold = threshold,pick_lists=pick_lists,p=p)
}




library(dplyr)
library(stringdist)
library(readxl)
library(tidyverse)
library(varhandle)
library(rapportools)



state_match <- function(messy_states,return_dists = FALSE, return_lists = NA,
                        threshold = NA,pick_lists=F, p = 0){
cat_match(messy_states,state.name,method = "jw",return_dists = return_dists,return_lists = return_lists,
          threshold = threshold,pick_lists=pick_lists,p=p)
}


#### Examples ####
messy_states1 <- readRDS("C:/Users/abhen/Desktop/messy.cats/data/messy_states1.rds")
messy_states2 <- readRDS("C:/Users/abhen/Desktop/messy.cats/data/messy_states2.rds")

# normal match with not very messy states list
state_match(messy_states1,return_dists=T,p=0.05) -> df

# normal match with messier states
state_match(messy_states2,return_dists=T,p=0.05) -> df2

# match returning lists with messier states
state_match(messy_states2,return_dists=T,p=0.05,return_lists = 3) -> df3

# match by choosing with messier states
state_match(messy_states2,return_dists=T,threshold=0.2,p=0.05,return_lists = 5,pick_lists=T) -> df4

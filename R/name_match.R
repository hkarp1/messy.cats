
library(dplyr)
library(magrittr)

readRDS("data/messy_names.rda") -> messy_names.df
readRDS("data/clean_names.rda") -> clean_names.df


name_match <- function(messy_names,clean_names,extract=NULL){
  messy_names %<>% mutate(full = tolower(paste(first,last,sep = " ")))
  clean_names %<>% mutate(full = tolower(paste(first,last,sep = " ")))
  cat_match(messy_names$full,clean_names$full) -> t
  min_dists = data.frame()

  if(is.numeric(extract) == T) {
    for (name in unique(clean_names$full)){
      filter(t, match == name) %>% arrange(dists) -> desc_dist
      desc_dist <- desc_dist[1:extract,]
      min_dists <- rbind(min_dists,desc_dist)
    }
    return(min_dists)
  }
  else {
    return(t)
  }
}


name_match(messy_names.df,clean_names.df) -> t

name_match(messy_names.df,clean_names.df,extract = 2) -> x


# loop takes the output and finds the closest match, if the 1 in brackets is changed it returns the x closest matches





# SCRAP ----
# read.delim("us-lastnames") -> us_lastnames
# read.delim("us-firstnames") -> us_firstnames
#
#
#
#
#
# # floor(runif(20,min=1,max=5162)) -> first_index.v
# # floor(runif(20,min=1,max=88798)) -> last_index.v
# # floor(runif(20,min=1,max=5162)) -> first_index2.v
# # floor(runif(20,min=1,max=88798)) -> last_index2.v
#
#
# us_firstnames[first_index.v,] -> first
# us_lastnames[last_index.v,] -> last
# us_firstnames[first_index2.v,] -> first2
# us_lastnames[last_index2.v,] -> last2
#
#
# data.frame(first,last) -> clean_names.df
#
# saveRDS(clean_names.df,"data/clean_names.rda")
#
#
# data.frame(first,last) -> x
# data.frame(first2,last2) -> y
# data.frame(first,last2) -> z
# data.frame(first2,last) -> t
#
#
# names(x) <- c("first","last")
# names(t) <- c("first","last")
# names(y) <- c("first","last")
# names(z) <- c("first","last")
#
#
# nmz <- list(x,y,z,t)
#
#
# do.call(rbind,nmz) -> messy_names.df
#
# saveRDS(messy_names.df,"data/messy_names.rda")

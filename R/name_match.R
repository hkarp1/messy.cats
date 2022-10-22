library(messy.cats)
library(dplyr)
library(magrittr)


readRDS("data/messy_names.rda") -> messy_names.df
readRDS("data/clean_names.rda") -> clean_names.df
messy_names.df %>% mutate(full = tolower(paste(first,last,sep = " "))) -> mn_full.df
clean_names.df %>% mutate(full = tolower(paste(first,last,sep = " "))) -> cn_full.df

create_full <- function(df){ # takes dataframe and creates full column
  names(df) -> vec #vector of column names
  if(TRUE %in% stringr::str_detect(vec,"full")){
    df[,stringr::str_detect(vec,"full")] -> full #dataframe had full column so just take that
    df$full = full
  }
  else {
    df[,stringr::str_detect(vec,"first")] -> first #find the first column
    df[,stringr::str_detect(vec,"last")] -> last #find the last column

    df$full = paste(first,last,sep = " ") #combine into full column
  }
  return(df)
}


name_match <- function(messy_names,clean_names,extract=NULL){
  if (is.data.frame(messy_names) == T){ #messy_names is dataframe
    # df w/ first&last
    messy_names %<>% create_full() #take df input and create full column
    if(is.data.frame(clean_names) == T){ #clean_names is dataframe
      # df w/ first&last
      clean_names %<>% create_full() #take df input and create full column
      cat_match(messy_names$full,clean_names$full) -> t #match newly made full columns
    }
    if(is.vector(clean_names)){ #clean_names is full name column
      cat_match(messy_names$full,clean_names) -> t #match newly made full columns

    }
    else {
    }
  }
  if(is.vector(messy_names)){ #messy_names is full name column
    if(is.data.frame(clean_names) == T){ #clean_names is dataframe
      clean_names %<>% create_full() #take df input and create full column
      cat_match(messy_names,clean_names$full) -> t #match newly made and inputted full columns
    }
    if(is.vector(clean_names) == T){ #clean_names is full name column
      cat_match(messy_names,clean_names) -> t #match inputted made full columns
    }
    else {
    }
  }

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

# TODO: ----

#1. allow for multiple input types
#   1.


#2. conditional statement for messy/clean_names args to be:
#   1. full name column
#   2. first and last name columns
#   3. dataframe where we find first/last/full column
#   4. any combination of the two


name_match(messy_names.df,clean_names.df) -> t
name_match(mn_full.df$full,clean_names.df) -> t1
name_match(messy_names.df,cn_full.df$full) -> t2


name_match(messy_names.df,clean_names.df,extract = 2) -> x
name_match(mn_full.df$full,clean_names.df,extract = 2) -> x1
name_match(messy_names.df,clean_names.df,extract = 2) -> x2







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

library(messy.cats)
library(dplyr)
library(magrittr)


# TODO: ----
# want to let people keep the names formatted how they want
#     upper, lower, Proper Capitalization,Camel/snake/whatever the fuck else-case
#     let them choose separator on full name:
#         " ","_","-","",

#1. conditional statement for messy/clean_names args to be:
#   1. full name column
#   2. first and last name columns
#   3. dataframe where we find first/last/full column
#   4. any combination of the two

#2. rn you give it a df it can tell if it has first and last or full
#   need to spit errors if it only has first or last and no full

#3. want to clean up the stringr::str_detect() calls with regex


readRDS("data/messy_names.rda") -> messy_names.df
readRDS("data/clean_names.rda") -> clean_names.df
messy_names.df %>% mutate(full = tolower(paste(first,last,sep = " "))) -> mn_full.df
clean_names.df %>% mutate(full = tolower(paste(first,last,sep = " "))) -> cn_full.df

messycols <- c("first_name","firstname","FIRSTNAME","FIRST_NAME",
               "last_name", "lastname", "LASTNAME", "LAST_NAME")


names(messy_names.df) <- c("FIRST_NAME","LASTNAME")
names(clean_names.df) <- c(" first names  ", "...last...")

create_full <- function(df){ # takes dataframe and creates full column
  tolower(names(df)) -> vec #vector of lowercase column names
  # I think just lowering the column names will work but I might be missing something
  # i think janitor::clean_names() might be good
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
    # take df input and create full column
    messy_names %<>% create_full()
    if(is.data.frame(clean_names) == T){ #clean_names is dataframe
      # take df input and create full column
      clean_names %<>% create_full()
      cat_match(messy_names$full,clean_names$full) -> t
    }
    if(is.vector(clean_names)){ #clean_names is full name column
      cat_match(messy_names$full,clean_names) -> t

    }
    else {
    }
  }
  if(is.vector(messy_names)){ #messy_names is full name column
    if(is.data.frame(clean_names) == T){ #clean_names is dataframe
      #take df input and create full column
      clean_names %<>% create_full()
      cat_match(messy_names,clean_names$full) -> t
    }
    if(is.vector(clean_names) == T){ #clean_names is full name column
      cat_match(messy_names,clean_names) -> t
    }
    else {
    }
  }
  # this would be the part to put in as argument to cat match
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



#tests -----

name_match(messy_names.df,clean_names.df) -> t #two dataframes w/ first and last
name_match(messy_names.df,cn_full.df$full) -> t1 #df w/ first and last, full column directly
name_match(messy_names.df,cn_full.df) -> t2 #df w/ first and last, df w/ full column
name_match(mn_full.df$full,clean_names.df) -> t3 #full column, df w/ first and last
name_match(mn_full.df$full,cn_full.df) -> t4 #full column directly, df w/ full column
name_match(mn_full.df$full,cn_full.df$full) -> t5 #2 full columns directly
name_match(mn_full.df,cn_full.df) -> t6 #two dataframes w/ full
name_match(mn_full.df,cn_full.df$full) -> t7 #dataframe w/ full, full column directly
name_match(mn_full.df,clean_names.df) -> t8 #df w/full, df w/first and last



name_match(messy_names.df,clean_names.df, extract = 2) -> x #two dataframes w/ first and last
name_match(messy_names.df,cn_full.df$full, extract = 2) -> x1 #df w/ first and last, full column directly
name_match(messy_names.df,cn_full.df, extract = 2) -> x2 #df w/ first and last, df w/ full column
name_match(mn_full.df$full,clean_names.df, extract = 2) -> x3 #full column, df w/ first and last
name_match(mn_full.df$full,cn_full.df, extract = 2) -> x4 #full column directly, df w/ full column
name_match(mn_full.df$full,cn_full.df$full, extract = 2) -> x5 #2 full columns directly
name_match(mn_full.df,cn_full.df, extract = 2) -> x6 #two dataframes w/ full
name_match(mn_full.df,cn_full.df$full, extract = 2) -> x7 #dataframe w/ full, full column directly
name_match(mn_full.df,clean_names.df, extract = 2) -> x8 #df w/full, df w/first and last



t %>% filter(dists == 0) %>% count()
t1 %>% filter(dists == 0) %>% count()
t2 %>% filter(dists == 0) %>% count()
t3 %>% filter(dists == 0) %>% count()
t4 %>% filter(dists == 0) %>% count()
t5 %>% filter(dists == 0) %>% count()
t6 %>% filter(dists == 0) %>% count()
t7 %>% filter(dists == 0) %>% count()
t8 %>% filter(dists == 0) %>% count()




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

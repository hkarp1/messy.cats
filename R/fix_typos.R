# find typos

# idea from dk:
# string distance calculation that weights letters that are close in qwerty-space

library(tidyverse)
library(stringdist)
data("clean_caterpillars")
data("messy_caterpillars")
load("~/Desktop/messy.cats/data/clean_names.rda")
load("~/Desktop/messy.cats/data/messy_names.rda")

# testing dataset ----
rep(clean_caterpillars$species,clean_caterpillars$count) -> clean_caterpillars_rep

append(clean_caterpillars_rep,messy_caterpillars$CaterpillarSpecies) -> typo_caterpillars

typo_df <- as.data.frame(typo_caterpillars)
# OSA / DL
  # use DL, probably give option for OSA

# Number of Edits
  # guess n check
  # find the most fucked up typo and count number of edits
  # keep it simple to start
  # have threshold be percentage of the word
    # change >= ~90% of letters, probably not a typo

# As prominence / how much more common the correctly spelled word is versus wrong word
  # as either number or percent
  # enter as integer for number and string for percent

# applications
  # names / words not in US dictionary
  # if using basic EN words just use hunspell (https://www.rdocumentation.org/packages/hunspell/versions/3.0.2/topics/hunspell)
library(tidytext)
a <- typo_df$typo_caterpillars




fix_typos <- function(typo_v, thr, occ_ratio) {
  # string <- str_split(paste0(typo_v, collapse = " "), " ")[[1]]
  # tbl <- table(string)
  # df <- data.frame(word = names(tbl), times = as.integer(tbl))

  tbl <- table(typo_v)
  df <- data.frame(word = names(tbl), times = as.integer(tbl))

  dist_mat <- stringdistmatrix(df$word, df$word, method = "dl")

  scaled_dist_mat <- dist_mat / nchar(df$word)

  scaled_dist_mat[lower.tri(scaled_dist_mat, diag=T)] <- 1

  as.data.frame(which(scaled_dist_mat<thr, arr.ind = T)) -> under_thresh_mat
  under_thresh_mat$row2 <- NA
  under_thresh_mat$col2 <- NA
  # to optimize later
  for (i in 1:nrow(under_thresh_mat)) {
    under_thresh_mat$row2[[i]] <- df$times[under_thresh_mat$row[[i]]]
    under_thresh_mat$col2[[i]] <- df$times[under_thresh_mat$col[[i]]]
  }

  under_thresh_mat$prom_ratio <- under_thresh_mat$row2 / under_thresh_mat$col2

  under_thresh_mat %>% filter(prom_ratio > occ_ratio | prom_ratio < 1/occ_ratio) -> under_thresh_mat

  # to optimize later
  for (i in 1:nrow(under_thresh_mat)) {
    if(under_thresh_mat$prom_ratio[i] > 1){
      typo_v[typo_v==df$word[under_thresh_mat$col[[i]]]] = df$word[under_thresh_mat$row[[i]]]
    }
    if(under_thresh_mat$prom_ratio[i] < 1){
      typo_v[typo_v==df$word[under_thresh_mat$row[[i]]]] = df$word[under_thresh_mat$col[[i]]]
    }
  }
  typo_v
}

fix_typos(typo_v = a, thr = 0.3, occ_ratio = 10) %>% unique



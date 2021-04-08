library(dplyr)
library(stringdist)
library(readxl)
library(tidyverse)
library(varhandle)
library(rapportools)

## can get rid of all the return stuff


cat_replace <- function(b_v, g_v, threshold = NA,
                      method = "jw", q = 1, p = 0, bt = 0,
                      useBytes = FALSE, weight=c(d=1, i=1, t=1)) {

  if (is.factor(b_v)) {
    b_v = unfactor(b_v)
  }

  if (is.factor(g_v)) {
    g_v = unfactor(g_v)
  }

  if (!is.vector(b_v)) {
    stop("Please use a vector for argument b_v")
  } else if (!is.vector(g_v)) {
    stop("Please use a vector for argument g_v")
  } else if (!is.numeric(p)) {
    stop("Argument p must be a number")
  } else if (p > .25) {
    stop("Argument p must be less than or equal to .25")
  } else if (!is.numeric(q)) {
    stop("Argument q must be a number")
  } else if (!is.numeric(bt)) {
    stop("Argument bt must be a number")
  } else if (!is.boolean(useBytes)) {
    stop("Argument unique must be a boolean")
  } else if (!(method %in% c("osa", "lv", "dl", "hamming", "lcs", "qgram",
                             "cosine", "jaccard", "jw","soundex"))) {
    stop("Please use only a method available to the stringdist function in the
         stringdist package")
  } else if (!is.na(threshold) & !is.numeric(threshold)) {
    stop("Argument threshold must be numeric")
  }


  x <- as.data.frame(stringdistmatrix(tolower(g_v), tolower(b_v),method = "jw"))

  #rownames(x) = g_v
  #colnames(x) = b_v

  new_var = c()

  if (!is.na(threshold)) {

    for (i in 1:length(b_v)) {
      if (min(x[[i]]) <= threshold) {
        new_var = append(new_var, g_v[which.min(x[[i]])])
      } else {
        new_var = append(new_var, NA)
      }
    }

  } else {

    for (i in 1:length(b_v)) {
      new_var = append(new_var, g_v[which.min(x[[i]])])
    }

  }

  return(new_var)

}

data(dmu)

threshold = .3

g_v = deerpop$DMU
b_v = dmu$DEER_MGM_1

dmu$new_var = cat_replace(b_v, g_v, threshold)


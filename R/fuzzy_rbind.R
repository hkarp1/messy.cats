library(dplyr)
library(stringdist)
library(tidyverse)
library(varhandle)
library(rapportools)

#' @title fuzzy_rbind
#' @description fuzzy_rbind() binds dataframes based on columns with slightly different names.
#' @param df1 The first dataframe to be bound.
#' @param df2 The second dataframe to be bound.
#' @param threshold The maximum string distance between column names, if the distance
#' between columns is greater than this threshold the columns will not be bound.
#' @param method The type of string distance calculation to use. Possible methods
#'  are : osa, lv, dl, hamming, lcs, qgram, cosine, jaccard, jw, and soundex.
#'  See package stringdist for more information. Default: 'jw', Default: 'jw'
#' @param q Size of the q-gram used in string distance calculation. Default: 1
#' @param p Only used with method "jw", the Jaro-Winkler penatly size. Default: 0
#' @param bt Only used with method "jw" with p > 0, Winkler's boost threshold. Default: 0
#' @param useBytes Whether or not to perform byte-wise comparison. Default: FALSE
#' @param weight Only used with methods "osa" or "dl", a vector representing the
#' penalty for deletion, insertion, substitution, and transposition,
#' in that order. Default: c(d = 1, i = 1, t = 1)
#' @return fuzzy_rbind() returns a dataframe that has bound the two inputted dataframes based on
#' the closest matching columns, column names from dataframe 1 are preserved.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  mtcars_colnames_messy = mtcars
#'  colnames(mtcars_colnames_messy)[1:5] = paste0(colnames(mtcars)[1:5], "_17")
#'  colnames(mtcars_colnames_messy)[6:11] = paste0(colnames(mtcars)[6:11], "_2017")
#'  x = fuzzy_rbind(mtcars, mtcars_colnames_messy, .5)
#'  x = fuzzy_rbind(mtcars, mtcars_colnames_messy, .2)
#'  }
#' }
#' @rdname fuzzy_rbind
#' @export

fuzzy_rbind <- function(df1, df2, threshold, method = "jw", q = 1, p = 0, bt = 0,
                        useBytes = FALSE, weight=c(d=1, i=1, t=1)) {
  if (!is.data.frame(df1)) {
    stop("Please use a dataframe for argument df1")
  } else if (!is.data.frame(df1)) {
    stop("Please use a dataframe for argument df2")
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
  } else if (!is.numeric(threshold)) {
    stop("Argument threshold must be numeric")
  } else if (!is.vector(weight) | length(weight) != 3) {
    stop("Argument weight must be a vector of length 3")
  }

  colnms1 = colnames(df1)
  colnms2 = colnames(df2)

  acc = list()
  count = 1

  for (i in 1:length(colnms1)) {
    for (e in 1:length(colnms2)) {

      x = stringdist(tolower(colnms1[[i]]), tolower(colnms2[[e]]),
                     method = method, p = p, q = q, bt = bt,
                     useBytes = useBytes,
                     weight = weight)

      if (x < threshold) {

        acc[[count]] = list(i,e)
        count = count + 1
      }
    }
  }


  df = data.frame(rep(NA, times = (nrow(df1) + nrow(df2))))

  for (i in 1:length(acc)) {
    df[colnms1[acc[[i]][[1]]]] = rbind(data.frame(x = df1[[acc[[i]][[1]]]]), data.frame(x = df2[[acc[[i]][[2]]]]))
  }

  if (ncol(df) == 0) {
    stop("None of the columns of the two dataframes match at the given threshold")
  }

  df = df[-1]


  return(df)
}




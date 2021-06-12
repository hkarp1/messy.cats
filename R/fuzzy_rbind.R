library(dplyr)
library(stringdist)
library(tidyverse)
library(varhandle)
library(rapportools)

#' @title fuzzy_rbind
#' @description FUNCTION_DESCRIPTION
#' @param df1 PARAM_DESCRIPTION
#' @param df2 PARAM_DESCRIPTION
#' @param lvl PARAM_DESCRIPTION
#' @param method PARAM_DESCRIPTION, Default: 'jw'
#' @param q PARAM_DESCRIPTION, Default: 1
#' @param p PARAM_DESCRIPTION, Default: 0
#' @param bt PARAM_DESCRIPTION, Default: 0
#' @param useBytes PARAM_DESCRIPTION, Default: FALSE
#' @param weight PARAM_DESCRIPTION, Default: c(d = 1, i = 1, t = 1)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname fuzzy_rbind
#' @export

fuzzy_rbind <- function(df1, df2, lvl, method = "jw", q = 1, p = 0, bt = 0,
                        useBytes = FALSE, weight=c(d=1, i=1, t=1)) {

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

      if (x < lvl) {

        acc[[count]] = list(i,e)
        count = count + 1
      }
    }
  }


  df = data.frame(rep(NA, times = (nrow(df1) + nrow(df2))))

  for (i in 1:length(acc)) {
    df[colnms1[acc[[i]][[1]]]] = rbind(data.frame(x = df1[[acc[[i]][[1]]]]), data.frame(x = df2[[acc[[i]][[2]]]]))
  }

  df = df[-1]

  return(df)
}





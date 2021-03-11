library(dplyr)
library(stringdist)
library(readxl)
library(tidyverse)
library(varhandle)
library(rapportools)


#' @title cat_match
#' @description FUNCTION_DESCRIPTION
#' @param b_v PARAM_DESCRIPTION
#' @param g_v PARAM_DESCRIPTION
#' @param return_dists PARAM_DESCRIPTION, Default: FALSE
#' @param return_lists PARAM_DESCRIPTION, Default: NA
#' @param pick_lists PARAM_DESCRIPTION, Default: F
#' @param threshold PARAM_DESCRIPTION, Default: NA
#' @param method PARAM_DESCRIPTION, Default: 'jw'
#' @param q PARAM_DESCRIPTION, Default: 1
#' @param p PARAM_DESCRIPTION, Default: 0
#' @param bt PARAM_DESCRIPTION, Default: 0
#' @param useBytes PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#' cat_match(bad$DEER_MGM_1, good$DMU, p = 0.1, return_dists = T, return_lists = 3, pick_lists = F)
#'  }
#' }
#' @rdname cat_match
#' @export

cat_match <- function(b_v, g_v, return_dists = FALSE, return_lists = NA, pick_lists = F,
                      threshold = NA, method = "jw", q = 1, p = 0, bt = 0,
                      useBytes = FALSE) {

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
  } else if (!is.boolean(return_dists)) {
    stop("Argument unique must be a boolean")
  } else if (!is.boolean(pick_lists)) {
    stop("Argument unique must be a boolean")
  } else if (!is.boolean(useBytes)) {
    stop("Argument unique must be a boolean")
  } else if (!(method %in% c("osa", "lv", "dl", "hamming", "lcs", "qgram",
                             "cosine", "jaccard", "jw","soundex"))) {
    stop("Please use only a method available to the stringdist function in the
         stringdist package")
  } else if (!is.na(return_lists) & !is.numeric(return_lists)) {
    stop("Argument return_lists must be either NA or numeric")
  } else if (!is.na(threshold) & !is.numeric(threshold)) {
    stop("Argument threshold must be either NA or numeric")
  }


  u_g_v = unique(g_v)
  u_b_v = unique(b_v)

  x <- as.data.frame(stringdistmatrix(u_g_v, u_b_v, method = method,
                                      p = p, useNames = TRUE, useBytes = useBytes))
  new_var <- c()







  if (!is.na(threshold)) {
    if (!is.na(return_lists)) {
      acc_dists = c()
      new_var = c()

      for (i in 1:ncol(x)) {
        x[i] %>% arrange(x[i]) %>% slice(1:return_lists) -> t
        t$matches = row.names(t)
        colnames(t)[1] = "dists"
        if (min(unlist(t$dists)) <= threshold) {
          t %>% slice(1) -> t
          new_var[[i]] = unlist(t$matches)
        } else {
          new_var[[i]] = unlist(t$matches)
        }

        if (return_dists == TRUE) {
          acc_dists[[i]] = round(unlist(t$dists), 4)
        }
      }

    } else {
      for (i in 1:ncol(x)) {
        min <- min(x[[i]])
        min.plc <- which.min(x[[i]])
        if (min <= threshold) {
          new_var <- new_var %>% append(u_g_v[min.plc])
        } else {
          new_var <- new_var %>% append(NA)
        }
      }
    }

  } else {
    if (!is.na(return_lists)) {
      acc_dists = c()
      new_var = c()

      for (i in 1:ncol(x)) {
        x[i] %>% arrange(x[i]) %>% slice(1:return_lists) -> t
        t$matches = row.names(t)
        colnames(t)[1] = "dists"
        new_var[[i]] = unlist(t$matches)
        if (return_dists == TRUE) {
          acc_dists[[i]] = round(unlist(t$dists), 4)
        }
      }

    } else {
      for (i in 1:ncol(x)) {
        min <- which.min(x[[i]])
        new_var <- new_var %>% append(u_g_v[min])
      }
    }
  }

  df = data.frame(y = u_b_v)
  df$x = new_var

  if (return_dists == TRUE & is.na(return_lists)) {
    df$dists = round(sapply(x, extract_min), 4)
  } else if (return_dists == TRUE & !is.na(return_lists)) {
    df$dists = acc_dists
  }

  colnames(df)[1:2] = c("bad", "match")

  if (!is.na(return_lists) & pick_lists == TRUE) {
    changing = df[lapply(df$match, length) > 1, ]
    df = df[!lapply(df$match, length) > 1, ]


    for (i in 1:nrow(changing)) {
      bad = changing[i, "bad"]
      matches = unlist(changing[i, "match"])
      dists = unlist(changing[i, "dists"])
      for (p in 1:return_lists) {
        print(paste0(matches[[p]], " has a distance of ", dists[[p]], " from ", bad))
        print(paste0("Enter ", p, " to keep this match"))
      }
      print("Enter 0 to force a non-match (make the string match to NA)")
      which = as.numeric(readline(prompt = "Which one would you like to keep?"))
      if (which == 0) {
        changing[i, "match"] = NA
        changing[i, "dists"] = NA
      } else {
        changing[i, "match"] = changing[i, "match"][[1]][which]
        changing[i, "dists"] = changing[i, "dists"][[1]][which]
      }
    }


    df = rbind(df, changing)
    df$match = unlist(df$match)
    df$dists = unlist(df$dists)
  }



  return(df)

}









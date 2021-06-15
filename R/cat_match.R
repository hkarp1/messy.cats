library(dplyr)
library(stringdist)
library(tidyverse)
library(varhandle)
library(rapportools)


#' @title cat_match
#' @description cat_match() matches the contents of a messy vector with
#' the closest match in a clean vector. The closest match can be found
#' using a variety of different string distance measurement options.
#' @param messy_v messy_vector: the vector of messy categories or strings
#' @param clean_v clean_vector: the vector of clean categories or strings to compare with messy_v
#' @param return_dists Returns the distance between the matched values. Range of values depends on method used: see help for stringdist::stringdist, Default: FALSE
#' @param return_lists Instead of the closest match, return a list of the top number of matches that you specify with this argument, Default: NA
#' @param pick_lists If TRUE cat_match() asks for user input and allows you to choose between the list of matches created by return_lists, Default: F
#' @param threshold Sets a threshold for an acceptable match. If return_lists != NA then lists are only returned for matches above the threshold, Default: NA
#' @param method method to calculate string distances. See help for stringdist::stringdist, Default: 'jw'
#' @param q Size of the q-gram; must be nonnegative. Only applies to method='qgram', 'jaccard' or 'cosine'., Default: 1
#' @param p Penalty factor for Jaro-Winkler distance. The valid range for p is 0 <= p <= 0.25. If p=0 (default), the Jaro-distance is returned. Applies only to method='jw', Default: 0
#' @param btWinkler's boost threshold. Winkler's penalty factor is only applied when the Jaro distance is larger than bt. Applies only to method='jw' and p>0., Default: 0, Default: 0
#' @param useBytes If TRUE, the matching is done byte-by-byte rather than character-by-character, Default: FALSE
#' @param weight For method='osa' or 'dl', the penalty for deletion, insertion, substitution and transposition, in that order. When method='lv', the penalty for transposition is ignored. When method='jw', the weights associated with characters of a, characters from b and the transposition weight, in that order. Weights must be positive and not exceed 1. weight is ignored completely when method='hamming', 'qgram', 'cosine', 'Jaccard', 'lcs', or soundex., Default: c(d = 1, i = 1, s = 1, t = 1)
#' @return Returns a dataframe with each unique value in the bad vector and it's closest match in the good vector. If return_dists is TRUE the distances between the matches are added as a column.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#' cat_match(bad$DEER_MGM_1, good$DMU, p = 0.1, return_dists = T, return_lists = 3, pick_lists = F)
#'  }
#' }
#' @rdname cat_match
#' @export

cat_match <- function(messy_v, clean_v, return_dists = FALSE, return_lists = NA, pick_lists = F,
                      threshold = NA, method = "jw", q = 1, p = 0, bt = 0,
                      useBytes = FALSE, weight=c(d=1, i=1, t=1)) {

  if (is.factor(messy_v)) {
    messy_v = unfactor(messy_v)
  }

  if (is.factor(clean_v)) {
    clean_v = unfactor(clean_v)
  }

  if (!is.vector(messy_v)) {
    stop("Please use a vector for argument messy_v")
  } else if (!is.vector(clean_v)) {
    stop("Please use a vector for argument clean_v")
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
  } else if (!is.vector(weight) | length(weight) != 3) {
    stop("Argument weight must be a vector of length 3")
  }


  u_clean_v = unique(clean_v)
  u_messy_v = unique(messy_v)

  x <- as.data.frame(stringdistmatrix(tolower(u_clean_v), tolower(u_messy_v),
                                      method = method, p = p,
                                      useBytes = useBytes,
                                      weight = weight,
                                      q = q, bt = bt))
  rownames(x) = u_clean_v
  colnames(x) = u_messy_v
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
          new_var <- new_var %>% append(u_clean_v[min.plc])
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
        new_var <- new_var %>% append(u_clean_v[min])
      }
    }
  }

  df = data.frame(y = u_messy_v)
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









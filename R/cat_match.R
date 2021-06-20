#' @title cat_match
#' @description `cat_match()` matches the contents of a messy vector with
#' the closest match in a clean vector. The closest match can be found
#' using a variety of different string distance measurement options.
#' @param messy_v The messy string vector that will be restructured. This can come in the form
#' of a column of a dataframe or a lone vector.
#' @param clean_v The clean string vector that will be referenced to perform the restructing.
#' Again, this argument can be a dataframe column or vector.
#' @param return_dists If set to TRUE the distance between the matched strings will
#' be returned as a third column in the output dataframe, Default: FALSE
#' @param return_lists Return list of top X matches, Default: NA
#' @param pick_lists Set to TRUE to manually choose matches, Default: F
#' @param threshold The maximum distance that will form a match. If this argument
#' is specified, any element in the messy vector that has no match closer than
#' the threshold distance will be replaced with NA. Default: NA
#' @param method The type of string distance calculation to use. Possible methods
#' are : osa, lv, dl, hamming, lcs, qgram, cosine, jaccard, jw, and soundex.
#' See package stringdist for more information. Default: 'jw'
#' @param q Size of the q-gram used in string distance calculation. Default: 1
#' @param p Only used with method "jw", the Jaro-Winkler penatly size. Default: 0
#' @param bt Only used with method "jw" with p > 0, Winkler's boost threshold. Default: 0
#' @param useBytes Whether or not to perform byte-wise comparison. Default: FALSE
#' @param weight Only used with methods "osa" or "dl", a vector representing the
#' penalty for deletion, insertion, substitution, and transposition,
#' in that order. Default: c(d = 1, i = 1, t = 1)
#' @return Returns a dataframe with each unique value in the bad vector and it's
#' closest match in the good vector. If return_dists is TRUE the distances between
#' the matches are added as a column.
#' @details cat_match() is meant as an exploratory tool to discover how two vectors
#' will match, and has added functionality to solve issues by hand and create a
#' dataframe that can be used to create custom matches between the clean and messy
#' vectors.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  messy_trees = c("red oak", "williw", "hemluck", "white elm", "fir tree", "birch tree", "pone", "dagwood", "mople")
#'  clean_trees = c("oak", "willow", "hemlock", "elm", "fir", "birch", "pine", "dogwood", "maple")
#'  matched_trees = cat_match(messy_trees, clean_trees)
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









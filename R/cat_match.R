#' @title cat_match
#' @description `cat_match()` matches the contents of a messy vector with
#' the closest match in a clean vector. The closest match can be found
#' using a variety of different string distance measurement options.
#' @param messy_v The messy string vector that will be restructured. This can come in the form
#' of a column of a dataframe or a lone vector.
#' @param clean_v The clean string vector that will be referenced to perform the restructing.
#' Again, this argument can be a dataframe column or vector.
#' @param return_dists If set to TRUE the distance between the matched strings will
#' be returned as a third column in the output dataframe, Default: TRUE
#' @param threshold The maximum distance that will form a match. If this argument
#' is specified, any element in the messy vector that has no match closer than
#' the threshold distance will be replaced with NA, Default: NA
#' @param method The type of string distance calculation to use. Possible methods
#' are : osa, lv, dl, hamming, lcs, qgram, cosine, jaccard, jw, and soundex. A method
#' can be automatically selected by setting method to auto.
#' See package stringdist for more information, Default: 'auto'
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
#' @details When dealing with messy categorical string data, string distance
#' matching can be an easy and efficient cleaning tool. A variety of string
#' distance calculation algorithms have been developed for different types of data,
#' and these algorithms can be used to detect and remedy problems with categorical
#' string data.
#'
#' By providing a correctly spelled and specified vector of categories to be compared
#' against a vector of messy strings, a cleaned vector of categories can be generated
#' by finding the correctly specificed string most similar to a messy string. This
#' method works particularly well for messy user-inputted data that often suffers
#' from transposition or misspelling errors.
#'
#' @examples
#' if(interactive()){
#'  messy_trees = c("red oak", "williw", "hemluck", "white elm",
#'  "fir tree", "birch tree", "pone", "dagwood", "mople")
#'  clean_trees = c("oak", "willow", "hemlock", "elm", "fir", "birch", "pine", "dogwood", "maple")
#'  matched_trees = cat_match(messy_trees, clean_trees)
#'  }
#' @rdname cat_match
#' @import dplyr
#' @export



cat_match <- function(messy_v, clean_v, return_dists = TRUE,
                      threshold = NA, method = "auto", q = 1, p = 0, bt = 0,
                      useBytes = FALSE, weight=c(d=1, i=1, t=1)) {

  if (is.factor(messy_v)) {
    messy_v = varhandle::unfactor(messy_v)
  }

  if (is.factor(clean_v)) {
    clean_v = varhandle::unfactor(clean_v)
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
  } else if (!rapportools::is.boolean(return_dists)) {
    stop("Argument unique must be a boolean")
  } else if (!rapportools::is.boolean(useBytes)) {
    stop("Argument unique must be a boolean")
  } else if (!(method %in% c("osa", "lv", "dl", "hamming", "lcs", "qgram",
                             "cosine", "jaccard", "jw","soundex", "auto"))) {
    stop("Please use only a method available to the stringdist function in the
         stringdist package or automatically select a metric with the auto method")
  } else if (!is.na(threshold) & !is.numeric(threshold)) {
    stop("Argument threshold must be either NA or numeric")
  } else if (!is.vector(weight) | length(weight) != 3) {
    stop("Argument weight must be a vector of length 3")
  }

  u_clean_v <- unique(clean_v)
  u_messy_v <- unique(messy_v)

  if (method == "auto") {
    method <- select_metric(messy_v, clean_v)
  }

  x <- as.data.frame(stringdist::stringdistmatrix(tolower(u_clean_v), tolower(u_messy_v),
                                                  method = method, p = p,
                                                  useBytes = useBytes,
                                                  weight = weight,
                                                  q = q, bt = bt))

  rownames(x) = u_clean_v
  colnames(x) = u_messy_v
  new_var <- c()

  if (!is.na(threshold)) {
    for (i in 1:ncol(x)) {
      min <- rapportools::min(x[[i]])
      min.plc <- which.min(x[[i]])
      if (min <= threshold) {
        new_var <- new_var %>% append(u_clean_v[min.plc])
      } else {
        new_var <- new_var %>% append(NA)
      }
    }

  } else {

    for (i in 1:ncol(x)) {
      min <- which.min(x[[i]])
      new_var <- new_var %>% append(u_clean_v[min])
    }

  }

  df = data.frame(y = u_messy_v)
  df$x = new_var

  if (return_dists == TRUE) {
    df$dists = round(sapply(x, extract_min), 4)
  }

  colnames(df)[1:2] = c("bad", "match")

  return(df)

}


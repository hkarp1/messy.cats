
#' @title cat_replace
#' @description cat_replace() replaces the contents of a messy vector with
#' the closest match in a clean vector. The closest match can be found
#' using a variety of different string distance measurement options.
#' @param messy_v The messy string vector that will be restructured. This can come in the form
#' of a column of a dataframe or a lone vector.
#' @param clean_v The clean string vector that will be referenced to perform the restructing.
#' Again, this argument can be a dataframe column or vector.
#' @param threshold The maximum distance that will form a match. If this argument
#' is specified, any element in the messy vector that has no match closer than
#' the threshold distance will be replaced with NA. Default: NA
#' @param method The type of string distance calculation to use. Possible methods
#'  are : osa, lv, dl, hamming, lcs, qgram, cosine, jaccard, jw, and soundex.
#'   See package stringdist for more information. Default: 'jw'
#' @param q Size of the q-gram used in string distance calculation. Default: 1
#' @param p Only used with method "jw", the Jaro-Winkler penatly size. Default: 0
#' @param bt Only used with method "jw" with p > 0, Winkler's boost threshold. Default: 0
#' @param useBytes Whether or not to perform byte-wise comparison. Default: FALSE
#' @param weight Only used with methods "osa" or "dl", a vector representing the
#' penalty for deletion, insertion, substitution, and transposition,
#' in that order. Default: c(d = 1, i = 1, t = 1)
#' @return cat_replace() returns a cleaned version of the bad vector, with each
#'  element replaced by the most similar element of the good vector.
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
#' @examples
#' \dontrun{
#' if(interactive()){
#'  messy_trees = c("red oak", "williw", "hemluck", "white elm", "fir tree", "birch tree", "pone", "dagwood", "mople")
#'  clean_trees = c("oak", "willow", "hemlock", "elm", "fir", "birch", "pine", "dogwood", "maple")
#'  cleaned_trees = cat_replace(messy_trees, clean_trees)
#'
#'  }
#' }
#' @rdname cat_replace
#' @export

cat_replace <- function(messy_v, clean_v, threshold = NA,
                        method = "jw", q = 1, p = 0, bt = 0,
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
  } else if (!is.boolean(useBytes)) {
    stop("Argument unique must be a boolean")
  } else if (!(method %in% c("osa", "lv", "dl", "hamming", "lcs", "qgram",
                             "cosine", "jaccard", "jw","soundex"))) {
    stop("Please use only a method available to the stringdist function in the
         stringdist package")
  } else if (!is.na(threshold) & !is.numeric(threshold)) {
    stop("Argument threshold must be numeric")
  }


  x <- as.data.frame(stringdistmatrix(tolower(clean_v), tolower(messy_v),
                                      method = method, p = p,
                                      useBytes = useBytes,
                                      weight = weight,
                                      q = q, bt = bt))

  #rownames(x) = clean_v
  #colnames(x) = messy_v

  new_var = c()

  if (!is.na(threshold)) {

    for (i in 1:length(messy_v)) {
      if (min(x[[i]]) <= threshold) {
        new_var = append(new_var, clean_v[which.min(x[[i]])])
      } else {
        new_var = append(new_var, NA)
      }
    }

  } else {

    for (i in 1:length(messy_v)) {
      new_var = append(new_var, clean_v[which.min(x[[i]])])
    }

  }

  return(new_var)

}

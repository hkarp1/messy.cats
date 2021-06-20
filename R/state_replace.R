#' @title state_replace
#' @description A wrapper function for cat_replace() that only requires an inputted
#' vector of messy US state names. state_replace() uses the built-in character
#' vector state.name as the reference clean vector.
#' @param messy_states Vector containing the messy state names that will be replaced
#' by the closest match from state.name
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
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  lst <- c("Indianaa", "Wisvconsin", "aLaska", "NewJersey", "Claifoarni")
#'  fixed <- state_replace(lst)
#'  }
#' }
#' @rdname state_replace
#' @export

state_replace <- function(messy_states, threshold = NA,
                          method = "jw", q = 1, p = 0, bt = 0,
                          useBytes = FALSE, weight=c(d=1, i=1, t=1)){
  cat_replace(messy_states, state.name, threshold, method = "jw", q, p, bt, useBytes, weight)
}


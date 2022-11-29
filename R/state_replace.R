#' @title state_replace
#' @description A wrapper function for `cat_replace()` that only requires an inputted
#' vector of messy US state names. `state_replace()` uses the built-in character
#' vector `state.name` as the reference clean vector.
#' @param messy_states Vector containing the messy state names that will be replaced
#' by the closest match from `state.name`
#' @param threshold The maximum distance that will form a match. If this argument
#' is specified, any element in the messy vector that has no match closer than
#' the threshold distance will be replaced with NA. Default: NA
#' @param p Only used with method "jw", the Jaro-Winkler penatly size. Default: 0
#' @return `state_replace()` returns a cleaned version of the bad vector, with each
#'  element replaced by the most similar element of the good vector.
#' @details State names are often misspelled or abbreviated in datasets, especially datasets that have been
#' manually digitized or created. `state_replace()` is a warpper function of `cat_replace()` that quickly solves
#' this common issue of mispellings or different formats of state names across datasets. This wrapper
#' function uses a built in clean list of country names `state.name` as the reference clean vector and
#' replaces your inputted messy vector of names to their nearest match in `state.name`.
#' @examples
#' if(interactive()){
#'  #EXAMPLE1
#'  lst <- c("Indianaa", "Wisvconsin", "aLaska", "NewJersey", "Claifoarni")
#'  fixed <- state_replace(lst)
#'  }
#' @rdname state_replace
#' @import datasets
#' @export

state_replace <- function(messy_states,threshold = NA, p = 0){

  cat_replace(messy_states,state.name, method = "jw", threshold, p)
}

#' @title state_match
#' @description A wrapper function for `cat_match()`hat only requires an inputted
#' vector of messy states. `state_match()` uses a built in clean list of
#' state names `state.name` as the reference clean vector.
#' @param messy_states Vector containing the messy state names that will be replaced
#' by the closest match from `state.name`
#' @param threshold The maximum distance that will form a match. If this argument
#' is specified, any element in the messy vector that has no match closer than
#' the threshold distance will be replaced with NA. Default: NA
#' @param p Only used with method "jw", the Jaro-Winkler penatly size. Default: 0
#' @return `state_match()` returns a cleaned version of the bad vector, with each
#'  element replaced by the most similar element of the good vector.
#' @details State names are often misspelled or abbreviated in datasets, especially datasets that have been
#' manually digitized or created. `state_match()` is a warpper function of `cat_match()` that quickly solves
#' this common issue of mispellings or different formats of country names across datasets. This wrapper
#' function uses a built in clean list of country names `state.name` as the reference clean vector and
#' matches your inputted messy vector of names to their nearest state in `state.name`.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  lst <- c("Indianaa", "Wisvconsin", "aLaska", "NewJersey", "Claifoarni")
#'  matched <- state_match(lst)
#'  }
#' }
#' @rdname state_match
#' @export

state_match <- function(messy_states,threshold = NA, p = 0){

    cat_match(messy_states,state.name, return_dists = FALSE, return_lists = NA,
              pick_lists = F, method = "jw", threshold, p)
}

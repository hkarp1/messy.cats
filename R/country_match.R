#' @title country_match
#' @description A wrapper function for `cat_match()` that only requires an inputted
#' vector of messy country names. `country_match()` uses a built in clean list of
#' country names `country.names` as the reference clean vector.
#' @param messy_countries Vector containing the messy country names that will be replaced
#' by the closest match from `country.names`
#' @param threshold The maximum distance that will form a match. If this argument
#' is specified, any element in the messy vector that has no match closer than
#' the threshold distance will be replaced with NA. Default: NA
#' @param p Only used with method "jw", the Jaro-Winkler penatly size. Default: 0
#' @return `country_match()` returns a cleaned version of the bad vector, with each
#'  element replaced by the most similar element of the good vector.
#' @details Country names are often misspelled or abbreviated in datasets, especially datasets that have been
#' manually digitized or created. `country_match()` is a warpper function of `cat_match()` that quickly solves
#' this common issue of mispellings or different formats of country names across datasets. This wrapper
#' function uses a built in clean list of country names `country.names` as the reference clean vector and
#' matches your inputted messy vector of names to their nearest country in `country.names`.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  lst <- c("Conagoa", "Blearaus", "Venzesual", "Uruagsya", "England")
#'  matched <- country_match(lst)
#'  }
#' }
#' @rdname country_match
#' @export

country_match <- function(messy_countries,threshold = NA, p = 0){

    cat_match(messy_countries,country.names[[1]],return_dists = FALSE, return_lists = NA,
            pick_lists = FALSE, method = "jw", threshold, p)
}

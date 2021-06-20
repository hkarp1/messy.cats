#' @title country_match
#' @description FUNCTION_DESCRIPTION
#' @param messy_countries PARAM_DESCRIPTION
#' @param return_dists PARAM_DESCRIPTION, Default: FALSE
#' @param return_lists PARAM_DESCRIPTION, Default: NA
#' @param threshold PARAM_DESCRIPTION, Default: NA
#' @param pick_lists PARAM_DESCRIPTION, Default: F
#' @param p PARAM_DESCRIPTION, Default: 0
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname country_match
#' @export

load("data/country.names.rda")
country_match <- function(messy_countries,return_dists = FALSE, return_lists = NA,
                        threshold = NA,pick_lists=F, p = 0){
  cat_match(messy_countries,country.names[[1]],method = "jw",return_dists = return_dists,return_lists = return_lists,
            threshold = threshold,pick_lists=pick_lists,p=p)
}




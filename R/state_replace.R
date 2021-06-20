
#' @title state_replace
#' @description Wrapper of cat_replace() Calls cat_replace on input with clean vector state.name, a vector of the names of all U.S. states
#' @param messy_states PARAM_DESCRIPTION
#' @param threshold PARAM_DESCRIPTION, Default: NA
#' @param method PARAM_DESCRIPTION, Default: 'jw'
#' @param q PARAM_DESCRIPTION, Default: 1
#' @param p PARAM_DESCRIPTION, Default: 0
#' @param bt PARAM_DESCRIPTION, Default: 0
#' @param useBytes PARAM_DESCRIPTION, Default: FALSE
#' @param weight PARAM_DESCRIPTION, Default: c(d = 1, i = 1, t = 1)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname state_replace
#' @export

state_replace <- function(messy_states, threshold = NA,
                          method = "jw", q = 1, p = 0, bt = 0,
                          useBytes = FALSE, weight=c(d=1, i=1, t=1)){
  cat_replace(messy_states, state.name, threshold, method = "jw", q, p, bt, useBytes, weight)
}

#Example
lst <- c("Indianaa", "Wisvconsin", "aLaska", "NewJersey", "Claifoarni")
fixed <- state_replace(lst)

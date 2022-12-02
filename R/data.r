#' @title clean_caterpillars
#' @description Dataframe with caterpillar counts from three summers.
#' @format A data frame with 74 rows and 3 variables:
#' \describe{
#'   \item{\code{species}}{character Full latin names of 29 caterpillar species.}
#'   \item{\code{count}}{integer Randomly generated fake counts of the caterpillars.}
#'   \item{\code{year}}{double Year of caterpillar observations.}
#'}
#' @details An example dataset with clean caterpillar species names.
"clean_caterpillars"

#' @title clean_names.df
#' @description Data set of clean names
#' @format A data frame with 20 rows and 2 variables:
#' \describe{
#'   \item{\code{first}}{character Clean first names}
#'   \item{\code{last}}{character Clean last names}
#'}
#' @details An example data that can be used in testing messy.cats functions
"clean_names.df"


#' @title country.names
#' @description Dataframe with country names as only variable, contains many popular and official names for countries.
#' @format A data frame with 203 rows and 1 variables:
#' \describe{
#'   \item{\code{name}}{character Names of countries}
#'}
#' @details This dataframe contains a list of clean country names with many popular and official names for countries.
"country.names"

#' @title messy_caterpillars
#' @description DATASET_DESCRIPTION
#' @format A data frame with 39 rows and 3 variables:
#' \describe{
#'   \item{\code{CaterpillarSpecies}}{character Full latin names of 39 caterpillar
#'   species with spelling and formatting errors.}
#'   \item{\code{Avg Weight (mg)}}{double Randomly generated fake weight data for each caterpillar species.}
#'   \item{\code{Avg Length (cm)}}{double Randomly generated fake length data for each caterpillar species.}
#'}
#' @details An example dataset with messy caterpillar species names.
"messy_caterpillars"

#' @title messy_names.df
#' @description Data set of messy names
#' @format A data frame with 80 rows and 2 variables:
#' \describe{
#'   \item{\code{first}}{character Messy first names}
#'   \item{\code{last}}{character MEssy last names}
#'}
#' @details An example data set of messy names that can be used in testing messy.cats functions.
"messy_names.df"


#' @title messy_states1
#' @description US State names with 1 character randomly changed.
#' @format A data frame with 50 rows and 1 variables:
#' \describe{
#'   \item{\code{messy_states1}}{character All 50 US states with 1 randomly changed character.}
#'}
#' @details An example dataset with mispelled US state names. The names have had 1 character randomly changed.
"messy_states1"

#' @title messy_states2
#' @description US State names with 2 characters randomly changed.
#' @format A data frame with 50 rows and 1 variables:
#' \describe{
#'   \item{\code{messy_states2}}{character All 50 US states with 2 randomly changed characters.}
#'}
#' @details An example dataset with mispelled US state names. The names have had 2 characters randomly changed.
"messy_states2"

#' @title picked_list
#' @description Handpicked matches from the datasets in intro.rmd.
#' @format A data frame with 15 rows and 3 variables:
#' \describe{
#'   \item{\code{bad}}{character column of bad car names}
#'   \item{\code{match}}{character column of good car names}
#'   \item{\code{dists}}{double string distance between the good and bad car names}
#'}
#' @details  An example dataset of matched car names.
"picked_list"


#' @title state.name
#' @description Testing data set of state names
#' @format A data frame with 50 rows and 1 variables:
#' \describe{
#'   \item{\code{states}}{character State names}
#'}
#' @details Testing data set of state names
"state.name"

#' @title typos
#' @description Data set of words, some correctly spelled, some typos, with their occurrence in text
#' @format A data frame with 27 rows and 2 variables:
#' \describe{
#'   \item{\code{occurrence}}{double number of times word appears in text}
#'   \item{\code{species}}{character words in text}
#'}
#' @details An example data set that can be used in testing fix_typos().
"typos"




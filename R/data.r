#' @title country.names
#' @description Dataframe with country names as only variable, contains many popular and official names for countries.
#' @format A data frame with 203 rows and 1 variables:
#' \describe{
#'   \item{\code{name}}{character Names of countries}
#'}
#' @details This dataframe contains a list of clean country names with many popular and official names for countries.
"country.names"

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
#'   \item{\code{picked_list}}{character Handpicked matches of cars.}
#'}
#' @details An example dataset of matched car names.
"picked_list"

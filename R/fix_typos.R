#' @title fix_typos
#' @description This function is meant to allow users to fix typos in strings
#'  that are not normally found in dictionaries.
#' @param typo_v vector of strings or 2 column data frame containing strings and the
#' count of their occurrence that will have its typos cleaned
#' @param threshold the string distance maximum used to determine typos. This argument
#' is specified as the percentage of a typo that should at most be expected to be
#' insertions, additons, deletions, and transpositions.
#' @param occ_ratio the minimum ratio of correctly spelled words to their typo.
#' This argument helps to weed out words that are similar but valid. For example
#' commonly occurring valid names such as Adam and Amy will not be recognized as typos
#' even though they are similar because they both appear often. Typos are recognized by
#' their similarity in addition to their infrequent occurrence.
#' @return reformatted vector with typos replaced with correctly spelled words
#' @details  There are great tools like the
#'  hunspell package that allow users to fix typos for words found in dictionaries,
#'  but these functions struggle to work for strings like proper nouns and other
#'  specific terminology not usually found in common dictionaries. This function
#'  uses the text being cleaned as a dictionary. It finds probable correctly spelled
#'  words based on their high occurrence and finds typos based on their low occurence.
#'  This is based on the theory that typos will appear as infrequently used words due
#'  no one using them on purpose, and they will be a short string distance from commonly
#'  occurring correctly spelled words.
#' @examples
#' if(interactive()){
#'  rep(clean_caterpillars$species,clean_caterpillars$count) -> clean_cat_rep
#'  append(clean_caterpillars_rep,messy_caterpillars$CaterpillarSpecies) -> typo_cat
#'
#'  fix_typos(typo_v = typo_cat, thr = 0.3, occ_ratio = 10)
#'
#'
#'  typo_df <- as.data.frame(typo_cat) %>% group_by(typo_cat) %>% count
#'
#'  fix_typos(typo_v = typo_df, thr = 0.3, occ_ratio = 10)
#'  }
#' @rdname fix_typos
#' @import dplyr
#' @export


fix_typos <- function(typo_v, threshold, occ_ratio) {
  if (!is.numeric(threshold)) {
    stop("Argument threshold must be numeric")
  } else if (is.numeric(threshold) & threshold > 1) {
    stop("Argument threshold must be less than 1")
  } else if (!is.numeric(occ_ratio)) {
    stop("Argument occ_ratio must be numeric")
  } else if(!is.character(typo_v) & !is.data.frame(typo_v)){
    stop("Argument typo_v must be a character vector or a dataframe of strings and their counts")
  } else if(!is.character(typo_v) & is.data.frame(typo_v) & length(typo_v) > 2) {
    stop("Argument typo_v can be a dataframe, but must have 2 columns: \n  one containing strings and the second containing their counts")
  }

  if(is.data.frame(typo_v)){
    orig_df <- TRUE
    typo_v = rep(unlist(typo_v[,1]),unlist(typo_v[,2]))
    names(typo_v) <- NULL
  } else {
    orig_df <- FALSE
  }
  prom_ratio <- NULL

  tbl <- table(typo_v)
  df <- data.frame(word = names(tbl), times = as.integer(tbl))

  dist_mat <- stringdist::stringdistmatrix(df$word, df$word, method = "dl")

  scaled_dist_mat <- dist_mat / nchar(df$word)

  scaled_dist_mat[lower.tri(scaled_dist_mat, diag=T)] <- 1

  as.data.frame(which(scaled_dist_mat<threshold, arr.ind = T)) -> under_thresh_mat
  under_thresh_mat$row2 <- NA
  under_thresh_mat$col2 <- NA
  # to optimize later
  for (i in 1:nrow(under_thresh_mat)) {
    under_thresh_mat$row2[[i]] <- df$times[under_thresh_mat$row[[i]]]
    under_thresh_mat$col2[[i]] <- df$times[under_thresh_mat$col[[i]]]
  }

  under_thresh_mat$prom_ratio <- under_thresh_mat$row2 / under_thresh_mat$col2

  under_thresh_mat %>% dplyr::filter(prom_ratio > occ_ratio | prom_ratio < 1/occ_ratio) -> under_thresh_mat

  # to optimize later
  for (i in 1:nrow(under_thresh_mat)) {
    if(under_thresh_mat$prom_ratio[i] > 1){
      typo_v[typo_v==df$word[under_thresh_mat$col[[i]]]] = df$word[under_thresh_mat$row[[i]]]
    }
    if(under_thresh_mat$prom_ratio[i] < 1){
      typo_v[typo_v==df$word[under_thresh_mat$row[[i]]]] = df$word[under_thresh_mat$col[[i]]]
    }
  }

  if(orig_df){
    typo_df = as.data.frame(typo_v) %>% group_by(typo_v) %>% count
    typo_df
  } else typo_v
}





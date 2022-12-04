# find typos

find_typos <- function(typo_v, threshold, occ_ratio) {
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

  tbl <- table(typo_caterpillars)
  df <- data.frame(word = names(tbl), times = as.integer(tbl))

  dist_mat <- stringdist::stringdistmatrix(df$word, df$word, method = "dl")
  rownames(dist_mat) <- df$word
  colnames(dist_mat) <- df$word

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

  typo_df = data.frame(row_ind = as.integer(under_thresh_mat$row),
                       col_ind = as.integer(under_thresh_mat$col))

  for (i in 1:nrow(under_thresh_mat)) {
    typo_df$row_string[[i]] = df$word[under_thresh_mat$row[[i]]]
    typo_df$col_string[[i]] = df$word[under_thresh_mat$col[[i]]]
    typo_df$row_occ[[i]] = under_thresh_mat$row2[[i]]
    typo_df$col_occ[[i]] = under_thresh_mat$col2[[i]]
  }
  typo_df
}

rep(clean_caterpillars$species,clean_caterpillars$count) -> clean_caterpillars_rep

append(clean_caterpillars_rep,messy_caterpillars$CaterpillarSpecies) -> typo_caterpillars

occ_ratio = 10
threshold = 0.3
find_typos(typo_v = typo_caterpillars, threshold = 0.3, occ_ratio = 10)





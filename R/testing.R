# Amdy, Amy, Andy Problem ----
test_v = c("Amy","Andy","Amdy", "Andy", "Amy", "Andy",
           "Ande", "Ame")
test_v2 = c("Andy", "Amdy", "Andy", "Amy", "Amy")


rep(clean_caterpillars$species,clean_caterpillars$count) -> clean_caterpillars_rep

append(clean_caterpillars$species,messy_caterpillars$CaterpillarSpecies) -> typo_caterpillars

typo_df <- as.data.frame(typo_caterpillars) %>% group_by(typo_caterpillars) %>% count


typo_v = typo_caterpillars
threshold = 5
occ_ratio = 2

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
rownames(dist_mat) <- df$word
colnames(dist_mat) <- df$word
# amdy -> andy = 1
# amdy -> amy = 1

dist_mat[lower.tri(dist_mat, diag=T)] <- 10

as.data.frame(which(dist_mat<=threshold, arr.ind = T)) -> under_thresh_mat
under_thresh_mat$row2 <- NA
under_thresh_mat$col2 <- NA
# to optimize later
for (i in 1:nrow(under_thresh_mat)) {
  under_thresh_mat$row2[[i]] <- df$times[under_thresh_mat$row[[i]]]
  under_thresh_mat$col2[[i]] <- df$times[under_thresh_mat$col[[i]]]
}

under_thresh_mat$prom_ratio <- under_thresh_mat$row2 / under_thresh_mat$col2

under_thresh_mat %>% dplyr::filter(prom_ratio > occ_ratio | prom_ratio < 1/occ_ratio) -> under_thresh_mat
# create dataframe showing any multiple matches based on reoccuring
# row index in under_thresh_mat
under_thresh_mat %>% count(row) -> multi_matches
multi_matches[multi_matches$n >= 2,] -> multi_matches
# if there are multiple matches, remove the match with higher prom_ratio
# meaning we take the match with greater ratio of correct:typo
if(nrow(multi_matches) != 0){
  under_thresh_mat %>% filter(row == multi_matches$row) -> t
  under_thresh_mat[-which.max(t$prom_ratio),] -> under_thresh_mat
}
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

# # fix_typos ----
# rep(clean_caterpillars$species,clean_caterpillars$count) -> clean_caterpillars_rep
#
# append(clean_caterpillars_rep,messy_caterpillars$CaterpillarSpecies) -> typo_caterpillars
#
# typo_df <- as.data.frame(typo_caterpillars) %>% group_by(typo_caterpillars) %>% count

# fix_typos(typo_v = typo_caterpillars, thr = 0.3, occ_ratio = 10)
#
# fix_typos(typo_v = typo_df, thr = 0.3, occ_ratio = 10)
#
# find_typos(typo_v = typo_caterpillars, thr = 0.3, occ_ratio = 10)
#
#
# occ_ratio = 4
# thr = 0.7
#
# t = rep(unlist(typo_df[,1]),unlist(typo_df[,2]))
# names(t) <- NULL
#
# fix_typos(typo_v = typo_caterpillars, threshold = 0.3, occ_ratio = 10)
# table(typo_caterpillars) %>% as.data.frame() -> messy
# table(fix_typos(typo_v = typo_caterpillars, threshold = 0.3, occ_ratio = 10)) %>% as.data.frame() -> clean
#
# fix_typos(typo_v = clean_caterpillars, thr = 0.3, occ_ratio = 10)
# fix_typos(typo_v = typo_caterpillars, thr = 3, occ_ratio = 10)
# fix_typos(typo_v = typo_caterpillars, thr = 0.3, occ_ratio = "10")
# fix_typos(typo_v = typo_caterpillars, thr = "0.3", occ_ratio = 10)
#
# as.data.frame(test_v) %>% group_by(test_v) %>% count
#
#
# counties <- c(
#   "Litchfield",
#   "Hartford",
#   "Tolland",
#   "Windham",
#   "Fairfield",
#   "New Haven",
#   "Middlesex",
#   "New London"
# )
#
# counties_typo <- c(
#   "Litchfeld",
#   "Hartferd",
#   "Tolland",
#   "Wind Ham",
#   "Fairfield",
#   "New Heven",
#   "Midlsex",
#   "NwLndn"
# )
#
# counties_v <- append(rep(counties,10),rep(counties_typo,5))
# table(counties_v) %>% as.data.frame() -> messy
# table(fix_typos(typo_v = counties_v, thr = 0.5, occ_ratio = 1.5)) %>% as.data.frame() -> clean
#
# ounties_v <- append(rep(counties,10),counties_typo)
#
# table(ounties_v) %>% as.data.frame() -> messy
# table(fix_typos(typo_v = ounties_v, thr = 0.5, occ_ratio = 9)) %>% as.data.frame() -> clean
#
#
# counties_v -> typo_v
# thr = 0.5
# occ_ratio = 5
#
# fix_typos(typo_v = counties_v, thr = 0.5, occ_ratio = 5) %>% unique
#
# # this is in the data folder
# readr::read_csv("data/aus_cities_typos.csv") -> aus_df
# aus_typos = aus_df$City
# thr = 0.5
# occ_ratio = 10
#
# fix_typos(aus_typos, thr = 0.5, occ_ratio = 10) %>% unique
#
# # i feel like this has potential but there are a ton of weird "corrections" that
# # are super not intuitive
# read_csv("github_typos.csv") -> gh_df
# gh_df %>% select(wrong, correct) -> gh_df2
# gh_typos <- append(gh_df2$wrong,gh_df2$correct)


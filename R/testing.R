# # fix_typos ----
rep(clean_caterpillars$species,clean_caterpillars$count) -> clean_caterpillars_rep

append(clean_caterpillars_rep,messy_caterpillars$CaterpillarSpecies) -> typo_caterpillars

typo_df <- as.data.frame(typo_caterpillars) %>% group_by(typo_caterpillars) %>% count

fix_typos(typo_v = typo_caterpillars, thr = 0.3, occ_ratio = 10)

fix_typos(typo_v = typo_df, thr = 0.3, occ_ratio = 10)

find_typos(typo_v = typo_caterpillars, thr = 0.3, occ_ratio = 10)
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
# test_v = c("Amy","Andy","Amdy", "Andy", "Amy")
# test_v2 = c("Andy", "Amdy", "Andy")
#
# fix_typos(test_v, thr = 0.5, occ_ratio = 0.5)
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

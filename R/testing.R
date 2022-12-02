# # fix_typos ----
# rep(clean_caterpillars$species,clean_caterpillars$count) -> clean_caterpillars_rep
#
# append(clean_caterpillars_rep,messy_caterpillars$CaterpillarSpecies) -> typo_caterpillars
#
# typo_df <- as.data.frame(typo_caterpillars)
#
# typo_v = typo_caterpillars
# occ_ratio = 10
# thr = 0.3
#
# fix_typos(typo_v = typo_v, thr = 0.3, occ_ratio = 10) %>% unique
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
# counties_v <- append(rep(counties,10),counties_typo)
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
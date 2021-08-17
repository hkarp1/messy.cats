# # find typos
#
# # idea from dk:
# # string distance calculation that weights letters that are close in qwerty-space
#
# library(tidyverse)
# data("clean_caterpillars")
# data("messy_caterpillars")
#
#
#
# # testing dataset ----
# rep(clean_caterpillars$species,clean_caterpillars$count) -> clean_caterpillars_rep
#
# append(clean_caterpillars_rep,messy_caterpillars$CaterpillarSpecies) -> typo_caterpillars
#
# typo_df <- as.data.frame(typo_caterpillars)
#
# typo_df %>% group_by(typo_caterpillars) %>% count() %>%
#   arrange(desc(n)) %>% select(typo_caterpillars) %>% as.vector() -> typo_freqs
#
#
#
# typo_df %>% group_by(typo_caterpillars) %>% count() -> counts
#
# mispellings <- counts %>% filter(n <= quantile(counts$n,0.25))
# references <- counts %>% filter(n > quantile(counts$n,0.25))
#
# cat_match(mispellings[["typo_caterpillars"]],references[["typo_caterpillars"]])
#
#
# # hand edited data
# typos = read_csv("data/typos.csv")
#
# # main code ----
# find_typos1.0 <- function(df,messy_column){
#
#   eval(parse(text = paste0("df %>% group_by(",messy_column,") %>% count() -> x")))
#   typos <- x %>% filter(n <= quantile(x$n,0.25))
#   correct <- x %>% filter(n > quantile(x$n,0.25))
#   return(cat_match(typos[[messy_column]],correct[[messy_column]]))
#
# }
#
# find_typos1.0(df = typo_df,messy_column = "typo_caterpillars")
#
# # want to transitively group any pairs with a string distance less than 0.2
# # hopefully create clusters of mispellings
#
#
# find_typos <- function(messy_column){
#   u_m_column = unique(messy_column)
#   as.data.frame(stringdistmatrix(u_m_column,u_m_column,method="jw")) -> x
# }
#
#
# messy_column = typos$species
# u_m_column = unique(messy_column)
# as.data.frame(stringdistmatrix(u_m_column,u_m_column,method="jw")) -> x
#
# row.names(x) = u_m_column
# colnames(x) =  u_m_column
#
# which(x<0.2,arr.ind=TRUE,useNames = F) -> t
#
# f <- function(a,b){
#   stringdist(a,b,method="jw")
# }
#
# l = list(c(1,2),c(3,4))
#
# x[l[[1]][1],l[[1]][2]]
#
#
# # how to decide which is the correctly spelled word in the group:
#
# # thing with lowest average distance between whole cluster?
#
# # make/let user choose
#
#
# # test ----
# find_typos(messy_column = typo_df$typo_caterpillars) -> df
#
#
# messy_column = typo_df$typo_caterpillars
# stringdistmatrix(messy_column,messy_column,method="jw")
#
# ######################
# ### Brainstorming ----
# ######################
#
#
# # Threshold----
# # how to determine what is a typo?
# stringdistmatrix("Pyreferra hesperidago",typos$species,method="jw") -> x
# # so far most things in jw with a dist < 0.15 seem to be typos
#
# # stolen from https://www.joyofdata.de/blog/comparison-of-string-distance-algorithms/
# b <- c(
#   "Cosmo Kramer",
#   "Kosmo Kramer",
#   "Comso Kramer",
#   "Csmo Kramer",
#   "Cosmo X. Kramer",
#   "Kramer, Cosmo",
#   "Jerry Seinfeld",
#   " CKaemmoorrs",
#   "Cosmer Kramo",
#   "Kosmoo Karme",
#   "George Costanza",
#   "Elaine Benes",
#   "Dr. Van Nostren",
#   "remarK omsoC",
#   "Mr. Kramer",
#   "Sir Cosmo Kramer",
#   "C.o.s.m.o. .K.r.a.m.e.r",
#   "CsoKae",
#   "Coso Kraer"
# )
#
# stringdistmatrix("Cosmo Kramer",b,method="jw") -> x2
# # based on this example it looks like typos could even be restricted to 0.20 for now
# # going to try with some shorter words and see how it works out
#
# # poggers
# d <- c(
#   "duck",
#   "fuck",
#   "luck",
#   "dog",
#   "fog",
#   "sog",
#   "sag",
#   "pog",
#   "bat",
#   "cat",
#   "sat",
#   "pat",
#   "batt"
# )
#
# df <- as.data.frame(d)
# df[,2] = df[,1]
#
# stringdistmatrix(df[,2],df[,1],method="jw") -> x3
#
# row.names(x3) = df[,2]
# colnames(x3) = df[,1]
# # the less letters in the strings the worse jw is
#
# stringdistmatrix(df[,2],df[,1],method="cosine") -> x4
#
# row.names(x4) = df[,2]
# colnames(x4) = df[,1]
#
# # probably for any string distance calc the less characters the less precise it will be
# # lets find a slightly shorter one word example to use
#
#
#
# cnts <- c(
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
# cnts_typo <- c(
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
# ct_counties <- append(cnts,cnts_typo)
# ct_counties <- unique(ct_counties)
# ct <- as.data.frame(ct_counties)
#
# stringdistmatrix(ct$ct_counties,ct$ct_counties,method="jw") -> x5
# rownames(x5) = ct$ct_counties
# colnames(x5) = ct$ct_counties
# # for jw with words ~6/7 or more characters, 0.15 will probably be a safe cutoff,
# # 0.2 to be conservative
#
# # need to test on lists with words that are close but not typos but typos would
# # make them closer
#
# l <- c(
#
#
# )
#
#
#
# # Choosing from the Clusters ----
# # how to decide which is the correctly spelled word in the group
# # thing with lowest average distance between whole cluster?
#
# ap = u_m_column[3:4]
# # in a case like this with only 2 strings in a cluster I can't think of a way
# # to decide which is right without looking at like trends in how words are written
# # e.g. soft and sofd, f and d aren't normally next to each other in english
#
# ph = u_m_column[c(9:11,13,14,29,22,25,26)] %>% na.omit()
# stringdistmatrix(ph,ph,method="jw") -> ph.x
# rownames(ph.x) = ph
# colnames(ph.x) = ph
#
# rowSums(ph.x)
# # in this example just summing the rows makes the correct spelling the lowest
#
#
# nr = u_m_column[15:17]
# stringdistmatrix(nr,nr,method="jw") -> nr.x
# rownames(nr.x) = nr
# colnames(nr.x) = nr
#
# rowSums(nr.x)
# # in this example rowsumming doesnt work
#
# # if there are a lot of similar mistakes they will have lower stringdist than the
# # correct spelling
#
# # have user designate the correct spelling in a cluster / if there was a problem
# # with the clustering
#
#

# find typos

# string distance calculation that weights letters that are close in qwerty-space
#
library(tidyverse)

clean_caterpillars
rep(clean_caterpillars$species,clean_caterpillars$count) -> clean_caterpillars_rep

append(clean_caterpillars_rep,messy_caterpillars$CaterpillarSpecies) -> typo_caterpillars

typo_df <- as.data.frame(typo_caterpillars)

typo_df %>% group_by(typo_caterpillars) %>% count() %>%
  arrange(desc(n)) %>% select(typo_caterpillars) %>% as.vector() -> typo_freqs



typo_df %>% group_by(typo_caterpillars) %>% count() -> counts

mispellings <- counts %>% filter(n <= quantile(counts$n,0.25))
references <- counts %>% filter(n > quantile(counts$n,0.25))

cat_match(mispellings[["typo_caterpillars"]],references[["typo_caterpillars"]])


find_typos <- function(df,messy_column){

  eval(parse(text = paste0("df %>% group_by(",messy_column,") %>% count() -> x")))
  typos <- x %>% filter(n <= quantile(x$n,0.25))
  correct <- x %>% filter(n > quantile(x$n,0.25))
  return(cat_match(typos[[messy_column]],correct[[messy_column]]))

}

find_typos(df = typo_df,messy_column = "typo_caterpillars")

`%!in%` <- Negate(`%in%`)
messy_caterpillars %<>% filter(CaterpillarSpecies %!in% c("Acronicta_hasta","Besma_quercivoraria","Heterocampa_gutivitta",
                                                         "Lithophane_hemmena","Lithophane_patefacta","Lomographa_glomeraria",
                                                         "Lomographa_vestaliata","Machimia_tentoriferella","Nadata_gibbosa",
                                                         "Orthosia_hibisci","Phigalia_strigitaria","Schizura_unicornis",
                                                         "Satyrium_calanus","Satyrium_liparops","Tetracis_cachexiata"))
new = messy_caterpillars
df = typo_df
messy_column = "typo_caterpillars"

eval(parse(text = paste0("df %>% group_by(",messy_column,") %>% count() -> x")))
typos <- x %>% filter(n <= quantile(x$n,0.25))
correct <- x %>% filter(n > quantile(x$n,0.25))
cat_match(typos[[messy_column]],correct[[messy_column]],method="lcs")

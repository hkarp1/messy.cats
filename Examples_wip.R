# ABH
# 3/11/21
# cat_match Examples

##############################
## show shapefiles in vignette
##############################

library(dplyr)
library(stringdist)
library(readxl)
library(tidyverse)
library(varhandle)
library(rapportools)
library(sf)
library(rgeos)
library(maptools)
library(raster)
library(qacr)
library(rgdal)
library(messy.cats)




## deer pop ----


deerpop %>%
  filter(Year==2017) %>%
  dplyr::select(DMU, PosthuntPopEst) -> pop17

head(pop17)

dmu <- st_read(dsn = "NDMU Shapefiles",
                  layer = "Deer_Management_Zones_Units_and_Metro_Subunits")
dmu %>%
  dplyr::select(geometry,DEER_MGM_1) -> dmu

plot(dmu)
# data is deer management unit shapefiles and deer population numbers for those
# DMUs, the problem is the names for the management units don't match exactly:
unique(dmu$DEER_MGM_1) %>% head(10)
unique(pop17$DMU) %>% head(10)

cat_match(dmu$DEER_MGM_1,pop17$DMU, p = 0.1, return_dists = T) -> df
# reservations and metro areas don't match well, set a threshold

cat_match(dmu$DEER_MGM_1,pop17$DMU, p = 0.1, return_dists = T, threshold = 0.2) -> df
# now I want to see what the top 3 matches were to check for accuracy

cat_match(dmu$DEER_MGM_1,pop17$DMU, p = 0.1, return_dists = T, threshold = 0.2,
          return_lists=3) -> df
# now that I see the top 3 I want to choose the best one or just disregard the match

cat_match(dmu$DEER_MGM_1,pop17$DMU, p = 0.1, return_dists = T, threshold = 0.2,
          return_lists=3,pick_lists = T) -> df

inner_join(df,dmu, by=c("bad"="DEER_MGM_1")) -> dmu

left_join(dmu,pop17[c(1,2)], by=c("match"="DMU")) -> dmu

head(dmu)

dmu %>% st_as_sf() -> dmu
ggplot()+
  geom_sf(data=dmu, aes(fill=PosthuntPopEst))+
  scale_fill_distiller(palette="RdPu",direction=1)+
  theme(panel.background = element_rect(fill="white"))

## Tree Species ----

# reading in data
trees <- import("veg_data.csv")
trees_clean <- import("veg_biomass_data.csv")
tree.l <- c(unique(trees_clean$Species))

tree.l
unique(trees$Species)

# the method relly depends on the type of strings you are trying to match
# with this tree species data jw gets some very wrong but lcs works

cat_match(trees$Species,tree.l, p=0.1, return_dists = T, pick_lists=F,
          return_lists = 1, threshold = 0.3, method = "jw") -> df_jw

arrange(df_jw,desc(dists)) %>% head(5)

cat_match(trees$Species,tree.l, p=0.1, return_dists = T, pick_lists=F,
          return_lists = NA, threshold = NA, method = "lcs") -> df_lcs

arrange(df_lcs,desc(dists)) %>% head(5)


# Messy mtcars ----
cat_match(rownames(messy_mtcars),rownames(mtcars))

cat_match(rownames(messy_mtcars),rownames(mtcars),return_dists=T)




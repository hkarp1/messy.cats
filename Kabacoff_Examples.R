# ABH
# 3/11/21
# Fx_Evrthng Examples

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


setwd("C:/Users/abhen/OneDrive - wesleyan.edu/FX_VRTHNG")

## deer pop ----

deerpop <- import("Demog.Deer.Harvest_Pop.DMU.2015-2018.Stenglein.csv")

deerpop %>% 
  filter(Year==2017) %>% 
  dplyr::select(DMU, PosthuntPopEst) -> pop17

dmu <- st_read(dsn = "NDMU Shapefiles",
                  layer = "Deer_Management_Zones_Units_and_Metro_Subunits")
dmu %>% 
  dplyr::select(geometry,DEER_MGM_1) -> dmu

extract_min <- function(col) {
  return(col[which.min(col)])
}

fx_vrthng <- function(b_v, g_v, return_dists = FALSE, return_lists = NA, 
                      threshold = NA, method = "jw", q = 1, p = 0, bt = 0,
                      useBytes = FALSE) {
  
  if (is.factor(b_v)) {
    b_v = unfactor(b_v)
  }
  
  if (is.factor(g_v)) {
    g_v = unfactor(g_v)
  }
  
  if (!is.vector(b_v)) {
    stop("Please use a vector for argument b_v")
  } else if (!is.vector(g_v)) {
    stop("Please use a vector for argument g_v")
  } else if (!is.numeric(p)) {
    stop("Argument p must be a number")
  } else if (p > .25) {
    stop("Argument p must be less than or equal to .25")
  } else if (!is.numeric(q)) {
    stop("Argument q must be a number")
  } else if (!is.numeric(bt)) {
    stop("Argument bt must be a number")
  } else if (!is.boolean(return_dists)) {
    stop("Argument unique must be a boolean")
  } else if (!is.boolean(useBytes)) {
    stop("Argument unique must be a boolean")
  } else if (!(method %in% c("osa", "lv", "dl", "hamming", "lcs", "qgram",
                             "cosine", "jaccard", "jw","soundex"))) {
    stop("Please use only a method available to the stringdist function in the
         stringdist package")
  } else if (!is.na(return_lists) & !is.numeric(return_lists)) {
    stop("Argument return_lists must be either NA or numeric")
  } else if (!is.na(threshold) & !is.numeric(threshold)) {
    stop("Argument threshold must be either NA or numeric")
  }
  
  
  u_g_v = unique(g_v)
  u_b_v = unique(b_v)
  
  x <- as.data.frame(stringdistmatrix(u_g_v, u_b_v, method = method,
                                      p = p, useNames = TRUE, useBytes = useBytes))
  new_var <- c()
  
  
  
  if (!is.na(threshold)) {
    if (!is.na(return_lists)) {
      acc_dists = c()
      new_var = c()
      
      for (i in 1:ncol(x)) {
        x[i] %>% arrange(x[i]) %>% slice(1:return_lists) -> t
        t$matches = row.names(t)
        colnames(t)[1] = "dists"
        if (min(unlist(t$dists)) <= threshold) {
          t %>% slice(1) -> t
          new_var[[i]] = unlist(t$matches)
        } else {
          new_var[[i]] = unlist(t$matches)
        }
        
        if (return_dists == TRUE) {
          acc_dists[[i]] = round(unlist(t$dists), 4)
        }
      }
      
    } else {
      for (i in 1:ncol(x)) {
        min <- min(x[[i]])
        min.plc <- which.min(x[[i]])
        if (min <= threshold) {
          new_var <- new_var %>% append(u_g_v[min.plc])
        } else {
          new_var <- new_var %>% append(NA)
        }
      }
    }
    
  } else {
    if (!is.na(return_lists)) {
      acc_dists = c()
      new_var = c()
      
      for (i in 1:ncol(x)) {
        x[i] %>% arrange(x[i]) %>% slice(1:return_lists) -> t
        t$matches = row.names(t)
        colnames(t)[1] = "dists"
        new_var[[i]] = unlist(t$matches)
        if (return_dists == TRUE) {
          acc_dists[[i]] = round(unlist(t$dists), 4)
        }
      }
      
    } else {
      for (i in 1:ncol(x)) {
        min <- which.min(x[[i]])
        new_var <- new_var %>% append(u_g_v[min])
      }
    }
  }
  
  df = data.frame(y = u_b_v)
  df$x = new_var
  
  if (return_dists == TRUE & is.na(return_lists)) {
    df$dists = round(sapply(x, extract_min), 4)
  } else if (return_dists == TRUE & !is.na(return_lists)) {
    df$dists = acc_dists
  }
  
  colnames(df)[1:2] = c("bad", "match") 
  
  changing = df[lapply(df$match, length) > 1, ]
  df = df[!lapply(df$match, length) > 1, ]
  
  
  for (i in 1:nrow(changing)) {
    bad = changing[i, "bad"]
    matches = unlist(changing[i, "match"])
    dists = unlist(changing[i, "dists"])
    for (p in 1:return_lists) {
      print(paste0(matches[[p]], " has a distance of ", dists[[p]], " from ", bad))
      print(paste0("Enter ", p, " to keep this match"))
    }
    print("Enter 0 to force a non-match (make the string match to NA)")
    which = as.numeric(readline(prompt = "Which one would you like to keep?"))
    if (which == 0) {
      changing[i, "match"] = NA
      changing[i, "dists"] = NA
    } else {
      changing[i, "match"] = changing[i, "match"][[1]][which]
      changing[i, "dists"] = changing[i, "dists"][[1]][which]
    }
  }
  
  
  df = rbind(df, changing)
  
  return(df)
  
}

fx_vrthng(dmu$DEER_MGM_1,pop17$DMU, p = 0.1, return_dists = T,
          return_lists = 3, threshold = .3) -> df


inner_join(df,dmu, by=c("bad"="DEER_MGM_1")) -> dmu

dmu$match = unlist(dmu$match)

left_join(dmu,pop17[c(1,2)], by=c("match"="DMU")) -> dmu

st_as_sf(dmu) -> dmu

ggplot()+
  geom_sf(data=dmu, aes(fill=PosthuntPopEst))+
  scale_fill_distiller(palette="RdPu",direction=1)+
  theme(panel.background = element_rect(fill="white"))

## Tree Species ----
## need to get this to work

trees <- import("veg_data.csv")
trees_clean <- import("veg_biomass_data.csv")
tree.l <- c(unique(trees_clean$Species))

fx_vrthng(trees$Species,tree.l, p=0.1, return_dists = T,
          return_lists = 3, threshold = .3) -> df2


## Countries ----
sul05<-import("sul05.csv")
wld<-import("wld.csv")

fx_vrthng(sul05$region,wld$region, p=0.1, return_dists = T,
          return_lists = 3, threshold = .3) -> df3

# df3 %>% 
#   filter(dists>0) -> df3

inner_join(df3,sul05, by=c("bad"="region")) -> sul05_matched

sul05_matched %>% 
  dplyr::select(match,sulfer) %>% 
  dplyr::rename(region=match) -> sul05_matched

ggplot()+
  geom_map(data=wld,aes(map_id=region),map=wld)+
  geom_map(data=sul05, aes(map_id=region,fill=sulfer),map=wld)+
  expand_limits(x=wld$long,y=wld$lat)+
  scale_fill_distiller("Sulfer Emissions (tonnes)", palette="RdPu",direction=1)

ggplot()+
  geom_map(data=wld,aes(map_id=region),map=wld)+
  geom_map(data=sul05_matched, aes(map_id=region,fill=sulfer),map=wld)+
  expand_limits(x=wld$long,y=wld$lat)+
  scale_fill_distiller("Sulfer Emissions (tonnes)", palette="RdPu",direction=1)



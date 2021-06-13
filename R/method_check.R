# # method check
# library(dplyr)
# library(stringdist)
# library(readxl)
# library(tidyverse)
# library(varhandle)
# library(rapportools)
#
#
# # u_g_v = unique(deerpop$DMU)
# # u_b_v = unique(dmu$DEER_MGM_1)
#
# x <- as.data.frame(stringdistmatrix(tolower(u_g_v), tolower(u_b_v),
#                                     method = "jw", p = 0.1,
#                                     useBytes = FALSE,
#                                     weight = c(d=1, i=1, t=1)))
# rownames(x) = u_g_v
# colnames(x) = u_b_v
# new_var <- c()
#
# #
# # jw<-cat_match(dmu$DEER_MGM_1,deerpop$DMU,method="jw",p=0.1,return_dists = T)
# # lcs<-cat_match(dmu$DEER_MGM_1,deerpop$DMU,method="lcs",p=0.1,return_dists = T)
# # cos<-cat_match(dmu$DEER_MGM_1,deerpop$DMU,method="cosine",p=0.1,return_dists = T)


colnames(messy_caterpillars)=c("Caterpillar Species","Avg Weight (g)")
clean_caterpillars




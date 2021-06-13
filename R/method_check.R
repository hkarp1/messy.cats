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

clean_caterpillars[1:29,1] = c("Achatia distincta","Alsophila pometaria","Amphipyra pyramidoides","Crocigrapha normani","Ennomos subsignaria","Eutrapela clemataria","Himella intractata","Hypagyrtis unipunctata","Iridopsis ephyraria","Iridopsis larvaria" ,"Itame pustularia","Lithophane antennata","Lithophane bethunei","Lymantria dispar","Malacasoma disstria","Melanolophia canadaria","Morrisonia confusa","Morrisonia latex","Nematocampa resistaria","Nola triquetrana","Orgyia leucostigma","Orthosia rubescens","Parallelia bistriaris","Phigalea titea","Prochoerodes lineola" ,"Pyreferra hesperidago","Zale phoeocapne","Zale lunata","Zale lunifera")

y = c("Achatia distincta","Acronicta hasta","Alsophila pomataria","Amphipyra pyramadoides","Besma quercivoraria","Heterocampa gutivitta","Himella intracta","Hypagyrtis unipunctata","Iridopsis ephyraria","Itame postularia","Lithophane antennata","Lithophane hemmena","Lithophane patefacta","Lomographa glomeraria","Lomographa vestaliata","Lymantria dispar","Machimia tentoriferella","Malacasoma disstria","Melanolophia canadaria","Morrisonia confusa","Morrisonia latex","Nadata gibbosa","Nematocampa resistaria","Nola triquetrana","Orgyia leucostigma","Orthosia hibisci","Orthosia rubesens","Papilio glaucus","Parallelia bistriarus","Phigalia strigitaria","Phigaliae titea","Prochoerodes linola","Pyrefera hesperidage","Satyrium calanus","Satyrium liparops","Tetracis cachexiata","Zale lunata","Zale lunefera","Schizura unicornis")
messy_caterpillars[1:39,1] = y
messy_caterpillars=messy_caterpillars[1:39,]
saveRDS(messy_caterpillars,"data/messy_caterpillars.rds")

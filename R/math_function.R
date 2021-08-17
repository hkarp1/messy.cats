# library(stringdist)
# library(Rfast)
#
#
# ## Uses heuristic algo to suggest a stringdist metric from among hamming, lv, osa, dl, lcs, jw
# ## Unclear how to incorporate choice of p for jw metric. Test for p = 0, 0.05, 0.1,..., .25?
# select_metric <- function(messy_v, clean_v){
#
#   metric <- c("lv", "osa", "dl", "lcs", "jw" , "jw")
#   metric_certainties <-NULL
#
#   for(k in 1:length(metric)){ ## Normalization still needs to be implemented for hamming, lcs, jw
#     p_0 = as.integer(k == 6)/10
#     dists <- stringdistmatrix(messy_v, clean_v, method = metric[k], p = p_0)
#
#     if(metric[k] == "lv" | metric[k] == "osa" | metric[k] == "dl"){
#       for(i in 1:nrow(dists)){
#         for(j in 1:ncol(dists)){
#           dists[i, j] <- dists[i,j]/max(nchar(messy_v[i]), nchar(clean_v[j]))
#         }
#       }
#     }
#
#     else if(metric[k] == "lcs"){
#       for(i in 1:nrow(dists)){
#         for(j in 1:ncol(dists)){
#           dists[i, j] <- dists[i,j]/(nchar(messy_v[i]) + nchar(clean_v[j]))
#         }
#       }
#     }
#
#     mins <- apply(dists, 1, FUN = min)
#     second_mins <- apply(dists, 1, FUN = nth, k = 2)
#     difs <- second_mins - mins
#     metric_certainties[k] <- mean(difs)
#
#   }
#   return(metric_certainties)
#   ##return(metric[which.max(metric_certainties)])
#
# }
#
# ## Testing
#
# clean_v = c("apple", "pineapple", "baps", "banana", "orange", "a lorange")
# messy_v = c("apoples", "oinealple", "vapps", " vanava", "oprange", "a loraing")
#
# library(stringi)
#
# x <- c()
# y <- c()
#
# table <- matrix(nrow = 10, ncol = 6)
#
#
# for(j in 1:10){
#
#   sim_cert <- c(0, 0, 0, 0, 0, 0)
#
#   for(i in 1:100){
#     x = stri_rand_strings(5, j, pattern = "[A-Za-z0-9]")
#     y = stri_rand_strings(5, j, pattern = "[A-Za-z0-9]")
#
#     sim_cert<- sim_cert + select_metric(x, y)
#   }
#
#   table[j,] <- sim_cert/100
#
# }
#
#
# certainties_5 <- as.data.frame(table)
# colnames(certainties_5) <- c("lv", "osa", "dl", "lcs", "jw p= 0" , "jw p = .1")
# certainties_5["String Length"] <- 1:10
# certainties_5 <- certainties_5[c("String Length", "lv", "osa", "dl", "lcs", "jw p= 0" , "jw p = .1")]
#
# table <- matrix(nrow = 10, ncol = 6)
#
#
# for(j in 1:10){
#
#   sim_cert <- c(0, 0, 0, 0, 0, 0)
#
#   for(i in 1:100){
#     x = stri_rand_strings(10, j, pattern = "[A-Za-z0-9]")
#     y = stri_rand_strings(10, j, pattern = "[A-Za-z0-9]")
#
#     sim_cert<- sim_cert + select_metric(x, y)
#   }
#
#   table[j,] <- sim_cert/100
#
# }
#
#
# certainties_10 <- as.data.frame(table)
# colnames(certainties_10) <- c("lv", "osa", "dl", "lcs", "jw p= 0" , "jw p = .1")
# certainties_10["String Length"] <- 1:10
# certainties_10 <- certainties_10[c("String Length", "lv", "osa", "dl", "lcs", "jw p= 0" , "jw p = .1")]
#
# table <- matrix(nrow = 10, ncol = 6)
#
# for(j in 1:10){
#
#   sim_cert <- c(0, 0, 0, 0, 0, 0)
#
#   for(i in 1:100){
#     x = stri_rand_strings(15, j, pattern = "[A-Za-z0-9]")
#     y = stri_rand_strings(15, j, pattern = "[A-Za-z0-9]")
#
#     sim_cert<- sim_cert + select_metric(x, y)
#   }
#
#   table[j,] <- sim_cert/100
#
# }
#
#
# certainties_15 <- as.data.frame(table)
# colnames(certainties_15) <- c("lv", "osa", "dl", "lcs", "jw p= 0" , "jw p = .1")
# certainties_15["String Length"] <- 1:10
# certainties_15 <- certainties_15[c("String Length", "lv", "osa", "dl", "lcs", "jw p= 0" , "jw p = .1")]
#
# table <- matrix(nrow = 10, ncol = 6)
#
# for(j in 1:10){
#
#   sim_cert <- c(0, 0, 0, 0, 0, 0)
#
#   for(i in 1:100){
#     x = stri_rand_strings(20, j, pattern = "[A-Za-z0-9]")
#     y = stri_rand_strings(20, j, pattern = "[A-Za-z0-9]")
#
#     sim_cert<- sim_cert + select_metric(x, y)
#   }
#
#   table[j,] <- sim_cert/100
#
# }
#
#
# certainties_20 <- as.data.frame(table)
# colnames(certainties_20) <- c("lv", "osa", "dl", "lcs", "jw p= 0" , "jw p = .1")
# certainties_20["String Length"] <- 1:10
# certainties_20 <- certainties_20[c("String Length", "lv", "osa", "dl", "lcs", "jw p= 0" , "jw p = .1")]
#
#
# reg_5 <- NULL
#
# for(i in 2:ncol(certainties_5)){
#   reg[i] <- coefficients(lm(certainties_5[,i] ~ poly(certainties_5[,1], 3, raw=TRUE)))
#
#
#
# }
#

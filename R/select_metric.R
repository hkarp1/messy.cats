# Uses heuristic algo to suggest a stringdist metric from among hamming, lv, osa, dl, lcs, jw
# For each metric, measures certainty via the difference between the best matches for each word and
# the average of all matches for each word
 select_metric <- function(messy_v, clean_v){

   metric <- c("lv", "osa", "dl", "lcs", "jw" , "jw")
   metric_certainties <-NULL

   for(k in 1:length(metric)){
     p_0 = as.integer(k == 6)/10 #sets p_0 to 0.1 for second test of jw
     dists <- stringdist::stringdistmatrix(messy_v, clean_v, method = metric[k], p = p_0)

     if(metric[k] == "lv" | metric[k] == "osa" | metric[k] == "dl"){ #maps lv, osa, and dl values into the range [0,1]
       for(i in 1:nrow(dists)){
         for(j in 1:ncol(dists)){
           dists[i, j] <- dists[i,j]/max(nchar(messy_v[i]), nchar(clean_v[j]))
         }
       }
     }

     else if(metric[k] == "lcs"){ #maps lcs into range [0,1]
       for(i in 1:nrow(dists)){
         for(j in 1:ncol(dists)){
           dists[i, j] <- dists[i,j]/(nchar(messy_v[i]) + nchar(clean_v[j]))
         }
       }
     }

     mins <- apply(dists, 1, FUN = min)
     means <- apply(dists, 1, FUN = mean)
     difs <- means - mins
     metric_certainties[k] <- mean(difs)

   }

   return(metric_certainties)

   if(which.max(metric_certainties) == 6){
     return("jw, p = 0.1")
   }
   else{
     return(metric[which.max(metric_certainties)])
   }

 }


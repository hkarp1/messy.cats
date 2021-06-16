library(stringdist)
library(Rfast)


## Uses heuristic algo to suggest a stringdist metric from among hamming, lv, osa, dl, lcs, jw
## Unclear how to incorporate choice of p for jw metric. Test for p = 0, 0.05, 0.1,..., .25?
select_metric <- function(messy_v, clean_v){

  metrics <- c("hamming", "lv", "osa", "dl", "lcs", "jw")
  metric_certainties <-NULL

  for(k in 1:length(metrics)){ ## Normalization still needs to be implemented for hamming, lcs, jw
    dists <- stringdistmatrix(messy_v, clean_v, method = metric[k])

    if(metric[k] == "lv" | metric[k] == "osa" | metric[k] == "dl"){
      for(i in 1:nrow(dists)){
        for(j in 1:ncol(dists)){
          dists[i, j] <- dists[i,j]/max(nchar(messy_v[i]), nchar(clean_v[j]))
        }
      }
    }

    mins <- apply(dists, 1, FUN = min)
    next_mins <- apply(dists, 1, FUN = nth, k = 2)
    difs <- next_mins - mins
    metric_certainties[k] <- mean(difs)

  }

  return(metrics[match(min(measures), measures)])

}







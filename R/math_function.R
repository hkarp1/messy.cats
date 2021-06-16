library(stringdist)

t = c("apple", "apple tree", "apple pie", "red apple",
      "tie", "tied", "time")

m = stringdistmatrix(t,t,method = c("osa"))
m = cbind(m, unlist(lapply(t, nchar)))
acc = c()

for (i in 1:(ncol(m)-1)) {
  v = m[-i, c(i, ncol(m))]
  v[,2] = ifelse(nchar(t[i]) > v[,2], nchar(t[i]), v[,2])
  matching_chars = v[,2] - v[,1]
  non_match_pct = (v[,2] - matching_chars)/v[,2]
  avg_non_match = mean(non_match_pct)
  acc = append(acc, avg_non_match)
}
mean(acc)
## need a way to decide which word is longer









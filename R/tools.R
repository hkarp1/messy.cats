create_full <- function(df){
  names(df) -> vec #vector of column names

  df[,stringr::str_detect(vec,"first")] -> first #find the first column
  df[,stringr::str_detect(vec,"last")] -> last #find the last column

  df$full = paste(first,last,sep = " ") # combine into full column
  return(df)
}

extract_min <- function(col) {
  return(col[which.min(col)])
}

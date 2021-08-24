#' @title cat_join
#' @description cat_join() joins two dataframes using the closest match
#' between two specified columns with misspellings or slight format differences.
#' The closest match can be found using a variety of different string distance measurement options.
#' @param messy_df The dataframe to be joined using a messy categorical variable.
#' @param clean_df The dataframe to be joined with a clean categorical variable to be used
#' as a reference for the messy column.
#' @param by A vector that specifies the columns to match and join by. If the column
#' names are the same input: "column_name". If the columns have different names
#' input: c("messy_column" = "clean_column")
#' @param threshold The maximum distance that will form a match. If this argument
#' is specified, any element in the messy vector that has no match closer than
#' the threshold distance will be replaced with NA. Default: NA
#' @param method The type of string distance calculation to use. Possible methods
#'  are : osa, lv, dl, hamming, lcs, qgram, cosine, jaccard, jw, and soundex.
#'   See package stringdist for more information. Default: 'jw'
#' @param q Size of the q-gram used in string distance calculation. Default: 1
#' @param p Only used with method "jw", the Jaro-Winkler penatly size. Default: 0
#' @param bt Only used with method "jw" with p > 0, Winkler's boost threshold. Default: 0
#' @param useBytes Whether or not to perform byte-wise comparison. Default: FALSE
#' @param weight Only used with methods "osa" or "dl", a vector representing the
#' penalty for deletion, insertion, substitution, and transposition,
#' in that order. Default: c(d = 1, i = 1, t = 1)
#' @param join Choose a join function from the dplyr package to use in joining the datasets. Default: 'left'
#' @return Returns a dataframe consisting of the two inputted dataframes joined by their designated columns.
#' @details When dealing with messy categorical string data, string distance
#' matching can be an easy and efficient cleaning tool. A variety of string
#' distance calculation algorithms have been developed for different types of data,
#' and these algorithms can be used to detect and remedy problems with categorical
#' string data.
#'
#' By providing a correctly spelled and specified vector of categories to be compared
#' against a vector of messy strings, a cleaned vector of categories can be generated
#' by finding the correctly specificed string most similar to a messy string. This
#' method works particularly well for messy user-inputted data that often suffers
#' from transposition or misspelling errors.
#'
#' `cat_join()` joins the messy and clean datasets using the closest matching elements from
#' designated columns. The columns from the datasets are inputted into `cat_replace()` as the
#' messy and clean vectors, and the datasets are joined using a user inputted dplyr join verb.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  messy_trees = data.frame()
#'  messy_trees[1:9,1] = c("red oak", "williw", "hemluck", "white elm", "fir tree", "birch tree", "pone", "dagwood", "mople")
#'  messy_trees[1:9,2] = c(34,12,43,32,65,23,12,45,35)
#'  clean_trees=data.frame()
#'  clean_trees[1:9,1] = c("oak", "willow", "hemlock", "elm", "fir", "birch", "pine", "dogwood", "maple")
#'  clean_trees[1:9,2] = "y"
#'  cat_join(messy_trees,clean_trees,by="V1",method="jaccard")
#'  }
#' }
#' @rdname cat_join
#' @export

cat_join <- function(messy_df, clean_df, by, threshold = NA, method = "jw",
                     q = 1, p = 0, bt = 0,
                     useBytes = FALSE, weight=c(d=1, i=1, t=1),
                     join="left"){
  # error handling ----
  if (!is.data.frame(messy_df)) {
    stop("Please use a dataframe for argument messy_df")
  } else if (!is.data.frame(clean_df)) {
    stop("Please use a dataframe for argument clean_df")
  } else if (!is.numeric(p)) {
    stop("Argument p must be a number")
  } else if (p > .25) {
    stop("Argument p must be less than or equal to .25")
  } else if (!is.numeric(q)) {
    stop("Argument q must be a number")
  } else if (!is.numeric(bt)) {
    stop("Argument bt must be a number")
  } else if (!rapportools::is.boolean(useBytes)) {
    stop("Argument unique must be a boolean")
  } else if (!(method %in% c("osa", "lv", "dl", "hamming", "lcs", "qgram",
                             "cosine", "jaccard", "jw","soundex"))) {
    stop("Please use only a method available to the stringdist function in the
         stringdist package")
  } else if (!(join %in% c("inner","left","right","full"))) {
    stop("Please use only a join type available to the mutate-joins function in the
         dplyr package: left, right, inner, or full.")
  } else if (!is.numeric(threshold) & !is.na(threshold)) {
    stop("Argument threshold must be numeric")
  } else if (!is.vector(weight) | length(weight) != 3) {
    stop("Argument weight must be a vector of length 3")
  }
  # main code ----
  if (length(by)==1){
    messy_df[[by]] <- cat_replace(messy_v = messy_df[[by]], clean_v = clean_df[[by]], q=q,p=p,bt=bt,
                          useBytes = useBytes,weight = weight, threshold = threshold,method = method)

    return(eval(parse(text=paste0("dplyr::", join,"_join(messy_df,clean_df,by=by)"))))

  } else if (length(by)==2){
    messy_df[[by[1]]] <- cat_replace(messy_v = messy_df[[by[1]]], clean_v = clean_df[[by[2]]], q=q,p=p,bt=bt,
                           useBytes = useBytes,weight = weight, threshold = threshold,method = method)

    colnames(messy_df)[colnames(messy_df) == by[1]] = by[2]
    return(eval(parse(text = paste0(join,"_join(messy_df,clean_df,by='", by[[2]], "')")))
)
  }
}

# # Testing
# join = "right"
#
# x = mtcars[,1:4]
# x$car = rownames(x)
#
# y = mtcars[,c(1,5:11)]
# y$car = rownames(y)
# by="car"
#
# eval(parse(text=paste0(join,"_join(y,x,by=by)")))
#
# # Example 1
# cat_join(y,x,by="car",method="jw",join="left")
#
# # Example 2
# messy_trees = data.frame()
# messy_trees[1:9,1] = c("red oak", "williw", "hemluck", "white elm", "fir tree", "birch tree", "pone", "dagwood", "mople")
# messy_trees[1:9,2] = c(34,12,43,32,65,23,12,45,35)
# clean_trees=data.frame()
# clean_trees[1:9,1] = c("oak", "willow", "hemlock", "elm", "fir", "birch", "pine", "dogwood", "maple")
# clean_trees[1:9,2] = "y"
#
# cat_join(messy_trees,clean_trees,by="V1",method="jaccard",join="inner",threshold=0.7)


% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cat_match_interactive.R
\name{cat_match_interactive}
\alias{cat_match_interactive}
\title{cat_match_interactive}
\usage{
cat_match_interactive(
  messy_v,
  clean_v,
  return_dists = TRUE,
  return_lists = NA,
  pick_lists = FALSE,
  threshold = NA,
  method = "auto",
  q = 1,
  p = 0,
  bt = 0,
  useBytes = FALSE,
  weight = c(d = 1, i = 1, t = 1)
)
}
\arguments{
\item{messy_v}{The messy string vector that will be restructured. This can come in the form
of a column of a dataframe or a lone vector.}

\item{clean_v}{The clean string vector that will be referenced to perform the restructing.
Again, this argument can be a dataframe column or vector.}

\item{return_dists}{If set to TRUE the distance between the matched strings will
be returned as a third column in the output dataframe, Default: TRUE}

\item{return_lists}{Return list of top X matches, Default: NA}

\item{pick_lists}{Set to TRUE to manually choose matches, Default: FALSE}

\item{threshold}{The maximum distance that will form a match. If this argument
is specified, any element in the messy vector that has no match closer than
the threshold distance will be replaced with NA, Default: NA}

\item{method}{The type of string distance calculation to use. Possible methods
are : osa, lv, dl, hamming, lcs, qgram, cosine, jaccard, jw, and soundex. A method
can be automatically selected by setting method to auto.
See package stringdist for more information, Default: 'auto'}

\item{q}{Size of the q-gram used in string distance calculation. Default: 1}

\item{p}{Only used with method "jw", the Jaro-Winkler penatly size. Default: 0}

\item{bt}{Only used with method "jw" with p > 0, Winkler's boost threshold. Default: 0}

\item{useBytes}{Whether or not to perform byte-wise comparison. Default: FALSE}

\item{weight}{Only used with methods "osa" or "dl", a vector representing the
penalty for deletion, insertion, substitution, and transposition,
in that order. Default: c(d = 1, i = 1, t = 1)}
}
\value{
Returns a dataframe with each unique value in the bad vector and it's
closest match in the good vector. If return_dists is TRUE the distances between
the matches are added as a column.
}
\description{
\code{cat_match_interactive()} matches the contents of a messy vector with
the closest match in a clean vector. The closest match can be found
using a variety of different string distance measurement options.
}
\details{
When dealing with messy categorical string data, string distance
matching can be an easy and efficient cleaning tool. A variety of string
distance calculation algorithms have been developed for different types of data,
and these algorithms can be used to detect and remedy problems with categorical
string data.

By providing a correctly spelled and specified vector of categories to be compared
against a vector of messy strings, a cleaned vector of categories can be generated
by finding the correctly specificed string most similar to a messy string. This
method works particularly well for messy user-inputted data that often suffers
from transposition or misspelling errors.

\code{cat_match_interactive()} is meant as an exploratory tool to discover how the elements
of two vectors will match using string distance measures, and has added functionality
to solve issues by hand and create a dataframe that can be used to create custom
matches between the clean and messy vectors.
}
\examples{
if(interactive()){
 messy_trees = c("red oak", "williw", "hemluck", "white elm",
 "fir tree", "birch tree", "pone", "dagwood", "mople")
 clean_trees = c("oak", "willow", "hemlock", "elm", "fir", "birch", "pine", "dogwood", "maple")
 matched_trees = cat_match_interactive(messy_trees, clean_trees)
 }
}
\seealso{
\code{\link[varhandle]{unfactor}}
\code{\link[rapportools]{is.boolean}}, \code{\link[rapportools]{min}}
\code{\link[stringdist]{stringdist}}
\code{\link[dplyr]{arrange}}, \code{\link[dplyr]{slice}}
}

#' Adjusted Rand Index (ARI)
#'
#' Description of the adjusted Rand Index function.
#'
#' In information theory, the Rand Index (also called the Rand Measure) is a measure of the similarity between
#' two data clusterings or classifications. If N is the set of elements and X and Y are the parition of N into n subsets, then
#' the Rand Index is composed of four subsets: (a) the number of pairs of elements in N that are in the same subset in in X and the same
#' subset in Y; (b) the number of pairs of elements in N that are in different subsets in X and differents subsets in Y; (c)
#' the number of pairs of elements in N that are in the same subset in X but different subsets in Y; and (d) the number of
#' pairs of elements in N that are in different subsets in X but the same subset in Y.
#' The adjusted Rand Index istyhe corrected-for-chance version of the Rand Index, which establishes a baseline by using the expected
#' similarity of all pairwise comparisions between clusterings specified by a random model. The ARI can yield negative results if the
#' index is less than the expected index.
#'
#' @param a a vector of classifications; this must be a vector of characters, integers, numerics, or a factor, but not a list.
#' @param b a vector of classifications
#'
#' @return res, a scalar with the adjusted Rand Index (ARI).
#'
#' @export

adj_RI <- function(a,b){
  res <- sort_pairs(a,b)

  N <- length(a)

  stot <- sum(choose(res$nij,2),na.rm=TRUE)
  srow <- sum(choose(res$ni.,2),na.rm=TRUE)
  scol <- sum(choose(res$n.j,2),na.rm=TRUE)

  expected_index <- (srow*scol)/(choose(N,2))
  max_index <- (srow+scol)/2

  if (expected_index == max_index & stot!=0){
    res <- 1
  } else {
    res <- (stot-expected_index)/(max_index-expected_index)
  }
  res
}

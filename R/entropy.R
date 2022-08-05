#' Entropy
#'
#' Description of the entropy function.
#'
#' A function to compute the empirical entropy for two vectors of classifications and the joint entropy
#'
#' The function returns a list of four different objects:
#' uv the joint entropy
#' u the conditional entropy of partition a
#' v the conditional entropy of partition b
#' sortPairs the output from the sort_pairs function ((see \code{\link{sort_pairs}} for more details))
#'
#' @param a a vector of classifications; this must be a vector of characters, integers, numerics, or a factor, but not a list.
#' @param b a vector of classifications
#'
#' @return a list of four objects, including the two conditional entropies, the joint entropy, and the output
#' of sort_pairs
#'
entropy <- function(a,b){
  res <- sort_pairs(a,b)
  N <- length (a)
  h.uv <- -sum(res$nij * log(res$nij))/N + log(N)
  h.u <- -sum(res$ni. * log(res$ni.))/N + log(N)
  h.v <- -sum(res$n.j * log(res$n.j))/N + log(N)

  res <- list(uv=h.uv, u=h.u, v=h.v, sortPairs=res)
  res
}

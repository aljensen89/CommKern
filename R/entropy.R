#' Entropy
#'
#' Description of the entropy function.
#'
#' A function to compute the empirical entropy for two vectors of classifications and the joint entropy
#'
#' @param a a vector of classifications; this must be a vector of characters,
#' integers, numerics, or a factor, but not a list.
#' @param b a vector of classifications
#'
#' @return a list of four objects:
#' \itemize{
#'   \item{\code{uv}}{ the joint entropy}
#'   \item{\code{u}}{ the conditional entropy of partition \code{a}}
#'   \item{\code{v}}{ the conditional entropy of partition \code{b}}
#'   \item{\code{sort_pairs}}{ the output from the sort_pairs function}
#' }
#'
#' @seealso \code{\link{sort_pairs}}
#'
entropy <- function(a, b) {
    res <- sort_pairs(a, b)
    N <- length(a)
    h.uv <- -sum(res$nij * log(res$nij))/N + log(N)
    h.u <- -sum(res$ni. * log(res$ni.))/N + log(N)
    h.v <- -sum(res$n.j * log(res$n.j))/N + log(N)

    res <- list(uv = h.uv, u = h.u, v = h.v, sort_pairs = res)
    res
}

#' Adjusted Rand Index (ARI)
#'
#' Description of the adjusted Rand Index function.
#'
#' In information theory, the Rand Index (also called the Rand Measure) is a
#' measure of the similarity between two data clusterings or classifications. If
#' N is the set of elements and X and Y are the partition of N into n subsets,
#' then the Rand Index is composed of four subsets: (a) the number of pairs of
#' elements in N that are in the same subset in in X and the same subset in Y;
#' (b) the number of pairs of elements in N that are in different subsets in X
#' and different subsets in Y; (c) the number of pairs of elements in N that
#' are in the same subset in X but different subsets in Y; and (d) the number of
#' pairs of elements in N that are in different subsets in X but the same subset
#' in Y. The adjusted Rand Index is the corrected-for-chance version of the Rand
#' Index, which establishes a baseline by using the expected similarity of all
#' pairwise comparisons between clusterings specified by a random model. The ARI
#' can yield negative results if the index is less than the expected index.
#'
#' @param a a vector of classifications; this must be a vector of characters,
#' integers, numerics, or a factor, but not a list.
#' @param b a vector of classifications
#' 
#' @seealso \code{\link{NMI}}, \code{\link{purity}}
#'
#' @return a scalar with the adjusted Rand Index (ARI)
#' 
#' @examples
#' set.seed(7)
#' x <- sample(x = rep(1:3, 4), 12)
#' 
#' set.seed(18)
#' y <- sample(x = rep(1:3, 4), 12)
#' 
#' adj_RI(x,y)
#'
#' @export
adj_RI <- function(a, b) {
    res <- sort_pairs(a, b)

    N <- length(a)

    stot <- sum(choose(res$nij, 2), na.rm = TRUE)
    srow <- sum(choose(res$ni., 2), na.rm = TRUE)
    scol <- sum(choose(res$n.j, 2), na.rm = TRUE)

    expected_index <- (srow * scol)/(choose(N, 2))
    max_index <- (srow + scol)/2

    if (expected_index == max_index & stot != 0) {
        res <- 1
    } else {
        res <- (stot - expected_index)/(max_index - expected_index)
    }
    res
}

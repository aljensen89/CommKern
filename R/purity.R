#' Purity
#'
#' Description of the purity function.
#'
#' In information theory, purity is an external evaluation criterion of cluster
#' quality. It is the percent of the total number of objects (data points) that
#' were classified in the range of [0,1]. Because we lack a ground truth
#' partition, a harmonic mean is calculated, where we consider partition a to be
#' the ground truth and then consider partition b to be the ground truth.
#'
#' @param a a vector of classifications; this must be a vector of characters,
#' integers, numerics, or a factor, but not a list.
#' @param b a vector of classifications
#'
#' @seealso \code{\link{adj_RI}}, \code{\link{NMI}}
#'
#' @return a scalar with the harmonic mean of the purity
#'
#' @examples
#' set.seed(7)
#' x <- sample(x = rep(1:3, 4), 12)
#'
#' set.seed(18)
#' y <- sample(x = rep(1:3, 4), 12)
#'
#' purity(x,y)
#'
#' @export

purity <- function(a, b) {
    purity_ab <- sum(apply(table(a, b), 2, max))/length(b)
    purity_ba <- sum(apply(table(b, a), 2, max))/length(a)

    harmonic_mean <- 2 * purity_ab * purity_ba/(purity_ab + purity_ba)
    harmonic_mean
}

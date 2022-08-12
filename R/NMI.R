#' Normalized mutual information (NMI)
#'
#' Description of the normalized mutual information function.
#'
#' In information theory, the mutual information (MI) of two random variables is
#' a measure of the mutual dependence between two variables, or the
#' quantification of the 'amount of information' obtained about one random
#' variable by observing the other random variable. The normalization of the MI
#' score scales the results between 0 (no mutual information) and 1 (perfect
#' correlation). The five options for the variant - max, min, square root, sum,
#' and joint - all relate to the denominator of the NMI = MI / D. 
#'
#' @param a a vector of classifications; this must be a vector of characters,
#' integers, numerics, or a factor, but not a list.
#' @param b a vector of classifications
#' @param variant a string in ('max', 'min', 'sqrt', 'sum', 'joint') that
#' calculates different variants of the NMI. The default use is 'max'.
#'
#' @seealso \code{\link{adj_RI}}, \code{\link{purity}}
#'
#' @return a scalar with the normalized mutual information (NMI).
#'
#' @examples
#' x <- c(1, 3, 1, 2, 3, 3, 3, 2, 1, 2, 1, 2)
#' y <- c(1, 1, 2, 3, 2, 1, 3, 1, 2, 3, 3, 2)
#'
#' NMI(x, y, variant = 'max')
#' NMI(x, y, variant = 'min')
#' NMI(x, y, variant = 'sqrt')
#' NMI(x, y, variant = 'sum')
#' NMI(x, y, variant = 'joint')
#'
#' x <- c("A", "A", "A", "A", "B", "C", "A", "B", "B", "C")
#' y <- c("B", "A", "A", "A", "C", "C", "B", "C", "D", "D")
#' NMI(x, y, variant = 'max')
#' NMI(x, y, variant = 'min')
#' NMI(x, y, variant = 'sqrt')
#' NMI(x, y, variant = 'sum')
#' NMI(x, y, variant = 'joint')
#'
#' @export

NMI <- function(a, b, variant = c("max", "min", "sqrt", "sum", "joint")) {
    variant <- match.arg(variant)
    H <- entropy(a, b)
    MI <- -H$uv + H$u + H$v

    D <- switch(
                variant,
                max = max(H$u, H$v),
                min = min(H$u, H$v),
                sqrt = sqrt(H$u * H$v),
                sum = 0.5 * (H$u + H$v),
                joint = H$uv
               )
    res <- MI / D
    res
}

#' Trace
#'
#' Return the trace of a square matrix
#'
#' @param x a square matrix
#' @param ... arguments passed to \code{\link{sum}}
#'
#' @return The trace, the sum of the diagonal elements, of the square matrix
#' \code{x}
#'
tr <- function(x, ...) {
    stopifnot(is.matrix(x))
    stopifnot(nrow(x) == ncol(x))
    sum(diag(x), ...)
}


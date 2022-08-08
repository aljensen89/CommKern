#' Bounds of grid search function
#'
#' Description of the bounds of grid search function.
#'
#' This ancillary function finds the upper and lower bounds of the grid search
#' implemented in the kernel score test.
#'
#' The function returns an m x m matrix (where m is the number of networks) to
#' be used as input for the kernel function.
#'
#' @param dist_mat a square distance matrix
#'
#' @return a square matrix of the same dimensions of the input matrix, comprised
#' of the sum square differences.
#'
up_low <- function(dist_mat) {
  stopifnot(is.matrix(dist_mat))
  stopifnot(nrow(dist_mat) == ncol(dist_mat))
  sq_diff <- diag(nrow(dist_mat))
  for (i in 1:nrow(dist_mat)) {
    for (j in 1:nrow(dist_mat)) {
      sq_diff[i, j] <- sum((dist_mat[i, ] - dist_mat[j, ])^2)
    }
  }
  sq_diff
}



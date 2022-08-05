#' Bounds of grid search function
#'
#' Description of the bounds of grid search function.
#'
#' This ancillary function finds the upper and lower bounds of the grid search implemented in the
#' kernel score test.
#'
#' The function returns an m x m matrix (where m is the number of networks) to be used as input for
#' the kernel function.
#'
#' @param dist_mat a square distance matrix
#'
#' @return a square matrix of the same dimensions of the input matrix, comprised of the sum
#' square differences.
#'
up_low <- function(dist_mat){
  p <- nrow(dist_mat)
  q <- ncol(dist_mat)
  sq_diff <- diag(p)
  for (i in 1:p){
    for (j in 1:p){
      sum <- 0
      for (k in 1:q){
        sum <- sum+((dist_mat[i,k]-dist_mat[j,k])**2)
      }
      sq_diff[i,j] <- sum
    }
  }
  return(sq_diff)
}

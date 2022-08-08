#' Distance-based kernel
#'
#' Description of the distance-based kernel function
#'
#' This is an ancillary function that is passed into the score function, used
#' for calculating the distance-based kernel.
#'
#' The function returns an m x m matrix (where m is the number of networks) to
#' be used as input for the kernel function.
#'
#' @param mat a distance-based matrix
#' @param rho a bandwidth/scaling parameter whose optimal value is solved for
#' within the larger score function
#'
#' @return the value of the kernel
#'
#' @export

kernel <- function(mat, rho) {
    K <- exp(-(mat^2)/rho)
    return(K)
}

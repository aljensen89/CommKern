#' Compute multimodal modularity matrix
#'
#' Description of the compute multimodal modularity matrix function.
#'
#' Calculates the multimodal version of the modularity matrix, which is detailed in the accompanying
#' manuscript as the following:
#' \deqn{\sum_{i \neq j} M_{ij} \delta(C_i,C_j) - \alpha \sum_{i \neq j} S_{ij} \delta(C_i,C_j).}{sum_{i <> j} M_{ij} delta(C_i,C_j) - alpha sum_{i <> j} S_{ij} delta(C_i,C_j).}
#' This function incorporates both the modularity matrix calculated from the \code{\link{compute_modularity_matrix}}
#' function and adds the additional component of a guidance matrix. The alpha parameter controls
#' the extent to which the guidance matrix influences the modularity, where alpha=0 means the
#' function reverts to the typical modularity calculation and alpha > 0 allows for some influence
#' of the guidance matrix. The guidance matrix will not penalize the modularity if two nodes are not
#' connected within it; it will only decrease the modularity if the two nodes have guidance information.
#' The function takes in a network object, the mod_matrix output from
#' \code{\link{compute_modularity_matrix}}, a vector of communities, and a parameter
#' alpha and returns the multimodal modularity matrix.
#'
#' @param net a network object in list form (see the \code{\link{matrix_to_df}} function for more details)
#' @param mod_matrix the modularity matrix output from the
#' \code{\link{compute_modularity_matrix}} function
#' @param communities the vector of node assignments to communities
#' @param alpha a double parameter balancing the use of the guidance matrix in modularity calculation
#'
#' @seealso \code{\link{matrix_to_df}}, \code{\link{compute_modularity_matrix}}
#'
#' @return multimodal modularity matrix
#'
compute_multimodal_mod <- function(mod_matrix, net, communities, alpha) {
    mat <- mod_matrix + (alpha * net$str_matrix)
    diag(mat) <- 0
    rtn <- 0
    for (i in unique(communities)) {
        idx <- which(communities == i)
        rtn <- rtn + sum(mat[idx, idx])  # We're adding instead of subtracting, and will negate later
    }
    -rtn  # Negate the sum, since we're adding instead of subtracting
}


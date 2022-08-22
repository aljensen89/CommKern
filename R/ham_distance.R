#' Hamiltonian distance matrix creation
#'
#' Description of the Hamiltonian distance matrix creation function.
#'
#' This function creates a distance matrix using the Hamiltonian output values
#' from a community detection algorithm that implements a Hamiltonian value,
#' such as the hierarchical multimodal spinglass algorithm. To ensure a
#' positive, semi-definite matrix (as required for the kernel function), the
#' absolute difference between Hamiltonian values is calculated.
#'
#' The function returns an m x m matrix (where m is the number of networks) to
#' be used as input for the kernel function.
#'
#' @param hamil_df a data frame containing two columns, one for network ID and
#' another containing Hamiltonian values
#'
#' @seealso \code{\link{hms}}
#'
#' @return the Hamiltonian distance matrix to be used as input for the kernel function
#'
#' @examples
#' hamil_df <- data.frame(id  = seq(1:8),
#'                        ham = c(-160.5375, -167.8426, -121.7128, -155.7245,
#'                                -113.9834, -112.5262, -117.9724, -171.374))
#'
#' ham_distance(hamil_df)
#'
#' @export

ham_distance <- function(hamil_df) {
  hamil_expand <- expand.grid(hamil_df[, 1], hamil_df[, 1])
    colnames(hamil_expand) <- c("id_a", "id_b")
    hamil_expand$hamil_diff <- NA

    for (i in 1:nrow(hamil_expand)) {
        hamil_expand$hamil_diff[i] <- sqrt(abs(hamil_df[hamil_df$id == hamil_expand$id_a[i],
            ][, 2] - hamil_df[hamil_df$id == hamil_expand$id_b[i], ][, 2])^2)
    }

    hamil_dist <- reshape2::dcast(data = hamil_expand, formula = id_a ~ id_b, value.var = "hamil_diff")
    hamil_dist <- as.matrix(hamil_dist[, -1])
    return(hamil_dist)
}

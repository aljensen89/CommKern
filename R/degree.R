#' Node degree calculation
#'
#' Description of the node degree calculation function.
#'
#' This is an ancillary function that calculates the functional and structural degree of each network
#' node using the functional and structural adjacency matrices, respectively.
#'
#' @param adj_matrix_func the adjacency matrix for functional connectivity
#' @param adj_matrix_str the adjacency matrix for structural connectivity
#' @param vertex_df a data frame of node (or vertex) information
#'
#' @return a data frame to be incorporated into the network object
#'
degree <- function(adj_matrix_func, adj_matrix_str, vertex_df) {
    qf <- nrow(adj_matrix_func)
    for (i in 1:qf) {
        vertex_df[i, 3] <- colSums(adj_matrix_func)[i]
        vertex_df[i, 4] <- colSums(adj_matrix_str)[i]
    }
    return(vertex_df)
}

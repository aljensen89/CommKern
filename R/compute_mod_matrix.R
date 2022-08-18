#' Compute modularity matrix
#'
#' Description of the compute modularity matrix function.
#'
#' Calculates the modularity matrix, which is the difference between the
#' observed adjacency matrix and the expected adjacency matrix (from a null
#' model). This is only computed for the main component of network information,
#' not accounting for the guidance. For neuroimaging application, this function
#' would be computing the modularity matrix for the functional connectivity
#' aspect of the network object. The function takes in a network object and
#' returns the modularity matrix.
#'
#' @param net a \code{spinglass_net} object (see \code{\link{matrix_to_df}} for
#' more details)
#'
#' @seealso \code{\link{matrix_to_df}}
#'
#' @return mod_matrix
#'
compute_modularity_matrix <- function(net) {
    m <- 0.5 * sum(net$func_matrix)
    d <- outer(net$vertexes$func_degree, net$vertexes$func_degree)/(2 * m)
    net$func_matrix - d
}


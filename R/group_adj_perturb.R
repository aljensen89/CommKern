#' Group adjacency matrices
#'
#' Description of the simulated group adjacency matrices function.
#'
#' This function takes the output from the \code{\link{group_network_perturb}}
#' function, which is a list of data frames summarizing each simulated network,
#' and creates an array of adjacency matrices. These adjacency matrices can then
#' be used as input to any community detection algorithm (such as the
#' hierarchical multimodal spinglass algorithm, \code{\link{hms}}).
#'
#' @param group_network_list the output from
#' \code{\link{group_network_perturb}}, which is a list of data frames detailing
#' nodes, community assignments of each node, and edge weights between each dyad
#' of nodes
#' @param n_nets the number of networks simulated
#' @param n_nodes the number of nodes in each simulated network (will be the
#' same across all networks)
#'
#' @seealso \code{\link{group_network_perturb}}, \code{\link{hms}}
#'
#' @return an array of adjacency matrices of dimension (n_nets x n_nodes x n_nodes)
#'
#' @examples
#'
#' # Example 1
#' sim_nofuzzy <-
#'   group_network_perturb(
#'     n_nodes = 45,
#'     n_comm = 3,
#'     n_nets = 3,
#'     perturb_prop = 0.1,
#'     wcr = c(8, 8),
#'     bcr = c(1.5, 8)
#'   )
#'
#' nofuzzy_adj <-
#'   group_adj_perturb(sim_nofuzzy, n_nets = 3, n_nodes = 45)
#'
#' if (require(pheatmap)) {
#'   pheatmap::pheatmap(
#'     nofuzzy_adj[1,,],
#'     treeheight_row = FALSE,
#'     treeheight_col = FALSE
#'   )
#' }
#'
#' # Example 2
#' sim_fuzzy <-
#'   group_network_perturb(
#'     n_nodes = 45,
#'     n_comm = 3,
#'     n_nets = 3,
#'     perturb_prop = 0.1,
#'     wcr = c(8, 8),
#'     bcr = c(1.5, 8),
#'     bfcr = c(3, 8),
#'     fuzzy_comms = c('comm_b','comm_c')
#'   )
#'
#' fuzzy_adj <-
#'   group_adj_perturb(sim_fuzzy, n_nets = 3, n_nodes = 45)
#'
#' if (require(pheatmap)) {
#'   pheatmap::pheatmap(
#'     fuzzy_adj[2,,],
#'     treeheight_row = FALSE,
#'     treeheight_col = FALSE
#'   )
#' }
#'
#' @export
group_adj_perturb <- function(group_network_list, n_nets, n_nodes) {
    adj_array <- array(NA, c(n_nets, n_nodes, n_nodes))
    for (j in 1:n_nets) {
        wgt_col_name <- colnames(group_network_list[[j]])[6]
        node_names <- union(group_network_list[[j]]$Node_A,
                            group_network_list[[j]]$Node_B)

        mat <- matrix(0, nrow = n_nodes, ncol = n_nodes,
                      dimnames = list(node_names, node_names))

        mat[as.matrix(group_network_list[[j]][, c(1, 2)])] <-
          group_network_list[[j]][, wgt_col_name]

        mat[as.matrix(group_network_list[[j]][, c(2, 1)])] <-
          group_network_list[[j]][, wgt_col_name]

        adj_array[j, , ] <- mat
    }
    return(adj_array)
}

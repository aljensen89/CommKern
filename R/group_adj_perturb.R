#' Group adjacency matrices
#' 
#' Description of the simulated group adjacency matrices function.  
#' 
#' This function takes the output from the \code{\list{group_network_perturb}} function, which is a list of
#' dataframes summarizing each simulated network, and creates an array of adjacency matrices. These adjacency
#' matrices can then be used as input to any community detection algorithm (such as \code{list\{hier_mult_spin}}).
#' 
#' The function returns an array of adjacency matrices of dimension (n_nets x n_nodes x n_nodes)
#' 
#' @param group_network_pertub the output from \code{\list{group_network_perturb}}, which is a list of dataframes
#' detailing nodes, community assignments of each node, and edge weights between each dyad of nodes
#' @param n_nets the number of networks simulated
#' @param n_nodes the number of nodes in each simulated network (will be the same across all networks)
#' 
#' @return adj_array, an array of adjacency matrices of dimension (n_nets x n_nodes x n_nodes)
#' 
#' @export
#' 
group_adj_perturb <- function(group_network_perturb,n_nets,n_nodes){
  adj_array <- array(NA,c(n_nets,n_nodes,n_nodes))
  for (j in 1:n_nets){
    wgt_col_name <- colnames(group_network_perturb[[j]])[6]
    node_names <- union(group_network_perturb[[j]]$Node_A,group_network_perturb[[j]]$Node_B)
    mat <- matrix(0,nrow=n_nodes,ncol=n_nodes,dimnames=list(node_names,node_names))
    mat[as.matrix(group_network_perturb[[j]][,c(1,2)])] <- group_network_perturb[[j]][,wgt_col_name]
    mat[as.matrix(group_network_perturb[[j]][,c(2,1)])] <- group_network_perturb[[j]][,wgt_col_name]
    
    adj_array[j,,] <- mat
  }
  return(adj_array)
}
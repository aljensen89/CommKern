#' Simulated network dataframe
#' 
#' Description of the simulated network dataframe function.  
#' 
#' This is an ancillary function that creates a list of dataframes, of which each dataframe describes
#' the community assignment for each node in the network. These dataframes are used as a starting
#' point for the edge weights to be added between nodes (see \code{\link{group_network_perturb}} and
#' \code{\link{get_weights}} for more information).
#' 
#'The function returns a list of dataframes detailing the nodes, node dyads, and community assignments.
#' 
#' @param n_nodes the number of nodes in each simulated network (will be the same across all networks)
#' @param n_comm the number of communities to be simulated in each network (will be the same across all networks)
#' @param n_nets the number of networks to simulate
#' @param perturb_prop the proportion of network nodes to randomly alter their community assignment within each network
#' 
#' @return net_list, a list of network dataframes containing nodes, their community assignment, and node dyads
#'  
simnet_df_perturb <- function(n_nodes,n_comm,n_nets,perturb_prop){
  
  # Create a master dataframe that has IDs for all nodes and 
  # assigns random communities to nodes
  master_node_df <- tidyr::tibble(
    Node = paste0("node_", str_pad(1:n_nodes, width = nchar(n_nodes), pad = "0")),
    Comm = sample(x = paste0("comm_", letters[1:n_comm]),
                   size = n_nodes,
                   replace = T))
  
  net_list <- list()
  perturb_num <- ceiling(n_nodes*perturb_prop)
  
  for (i in 1:n_nets){
    rows <- sample(1:n_nodes,perturb_num)
    new_comms <- sample(x = paste0("comm_", letters[1:n_comm]),
                         size = perturb_num, replace = T)
    
    ind_node_df <- master_node_df
    
    for (j in 1:length(rows)){
      ind_node_df[rows[j],][2] <- new_comms[j]
    }
    
    # Create a dataframe with all possible combinations of nodes except 'self-relationships'
    ind_dataframe <- expand.grid(Node_A = ind_node_df$Node,
                                 Node_B = ind_node_df$Node,
                                 stringsAsFactors = F) %>%
      dplyr::left_join(select(ind_node_df, Node_A = Node, Node_A_Comm = Comm),
                       by = "Node_A") %>% 
      dplyr::left_join(select(ind_node_df, Node_B = Node, Node_B_Comm = Comm),
                       by = "Node_B") %>% 
      dplyr::filter(Node_A != Node_B)
    
    # Limit the dataframe to one row per dyad for an undirected network
    ind_dataframe <- ind_dataframe %>% 
      dplyr::arrange(Node_A, Node_B) %>% 
      dplyr::mutate(dyad = if_else(Node_A < Node_B, paste0(Node_A, "_", Node_B),
                                   paste0(Node_B, "_", Node_A))) %>% 
      dplyr::distinct(dyad, .keep_all = T)
    
    net_list[[i]] <- ind_dataframe
  }
  
  return(net_list)
}

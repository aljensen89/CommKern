#' Simulated group networks
#' 
#' Description of the simulated group networks function.  
#' 
#' This function creates a list of simulated networks, of which each network is in a dataframe format,
#' which describes describes the community assignment for each node in the network, and simulates the edge weights
#' based on whether the node dyad is: (a) withiin the same community; (b) between different communities, or (c)
#' between different communities, but designated as 'fuzzy' in their distinction from one another.
#' 
#' The function returns a list of dataframes detailing the nodes, node dyads, community assignments,
#' and edge weights for all dyads in each simulated network
#' 
#' @param n_nodes the number of nodes in each simulated network (will be the same across all networks)
#' @param n_comm the number of communities to be simulated in each network (will be the same across all networks)
#' @param n_nets the number of networks to simulate
#' @param perturb_prop the proportion of network nodes to randomly alter their community assignment within each network
#' @param wcr within community edge weights, sampled from a beta distribution; for example, c(8,8) will ask for the within
#' community edge weights to be sampled from a Beta(8,8) distribution
#' @param bcr between community edge weights, sampled from a beta distribution; for example, c(1,8) will ask for the between
#' community edge weights to be sampled from a Beta(1,8) distribution
#' @param bfcr fuzzy community edge weights, sampled from a beta distribution; for example, c(4,8) will ask for the
#' fuzzy coommunity edge weights to be sampled from a Beta (4,8) distribution
#' @param fuzzy_comms the communities for which their distinction is 'fuzzy,' or not as distinct; fuzzy communities tend to have
#' higher between community edge weights; for example, c("comm_a","comm_c") will create a fuzzy distinction between
#' communities a and c
#' 
#' @return net_perturb_list, a list of network dataframes containing nodes, their community assignment, node dyads, and edge weights
#' 
#' @export
#' 
group_network_perturb <- function(n_nodes,n_comm,n_nets,perturb_prop,wcr,bcr,bfcr=NA,close_comms=NA){
  
  # Creating the group-perturbed network list
  net_perturb_list <- simnet_df_perturb(n_nodes,n_comm,n_nets,perturb_prop)
  
  # Creating and adding the weights for each network to the network list
  for (i in 1:n_nets){
    newcolname <- paste0("S",i)
    net_perturb_list[[i]][[newcolname]] <- get_weights(net_perturb_list[[i]],wcr=wcr,bcr=bcr)
  }
  return(net_perturb_list)
}
#' Potts Model Function
#' 
#' Description of the Potts model function.  
#' 
#' From the pottsmodel_2 test file, translating the PottsModel function
#' 
#' @param network a network (potentially need more than one variable for this?)
#' @param qvalue an unsigned integer
#' @param m an integer
#' 
#' @return TBD
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export
potts_model <- function(network, qvalue, m){
  
  net <- network
  q <- qvalue #Number of communities
  operation_mode <- m
  k_max <- 0
  Qa <- rep(NA, q+1) #Needed in calculating modularity
  weights <- rep(NA, q+1) #Weights for each spin state needed in Monte Carlo process
  color_field <- rep(NA, q+1) #Bookkeeping of occupation numbers of spin states or the number of links in the community
  neighbors <- rep(NA, q+1)
  
  num_of_nodes <- length(net$vertexes$node_id)
  num_of_links <- nrow(net$func_edges)
  
  #n_cur=iter.First(net->node_list); ##the first node in the network list is the current node
  
  ##These lists are needed to keep track of spin states for parallel update mode
  new_spins <- rep(0,length(network$vertexes$node_id))
  previous_spins <- rep(0,length(network$vertexes$node_id))
  
  for (i in 1:length(network$vertexes$node_id)){
    if (k_max < network$vertexes$func_degree[i]){
      k_max <- network$vertexes$func_degree[i]
    }
  }
}
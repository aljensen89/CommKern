#' Heat bath lookup negative
#' 
#' Description of the negative heat bath function
#' 
#' From the pottsmodel_2 text file, translating the HeatBathLookup_Neg
#' function. The description in the C++ code is the following: the same function as 
#' before, but rather than parallel update, it picks the nocdes to update randomly
#' 
#' @param gamma double
#' @param lambda double
#' @param t double
#' @param max_sweeps integer
#' 
#' @return changes/num_of_nodes/sweep
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

heatbath_neg <- function(gamma,lambda,t,max_sweeps) {
  #The new_spin contains the spin to which we will update
  #The spin_opt is the optional spin we will consider
  #The old_spin is the spin of the node we are currently changing
  
  new_spin <- NA
  spin_opt <- NA
  old_spin <- NA
  sweep <- 0
  changes <- 0
  problem_count <- 0
  
  exp_old_spin <- 0 #The expectation value for the old spin
  exp_spin <- 0 #The expectation value for the other spin
  v <- 0 #The node we will be investigating
  
  #The variables required for the calculations
  delta_pos_out <- 0
  delta_pos_in <- 0
  delta_neg_out <- 0
  delta_neg_in <- 0
  k_v_pos_out <- 0
  k_v_pos_in <- 0
  k_v_neg_out <- 0
  k_v_neg_in <-0
  
  w <- 0 #Weight of the edge
  beta <- (1/t) #Weight for the probabilities
  r <- 0 #Random number used for assigning new spin
  
  maxweight <- 0
  sum_weights <- 0 #For normalizing the probabilities
  
  m_pt <- m_p
  m_nt <- m_n
  
  if (m_pt<0.001) {
    m_pt <- 1
  }
  
  if(m_nt<0.001) {
    m_nt <- 1
  }
  
  while(sweep<max_sweeps) {
    sweep <- sweep+1
    
    #Loop over all nodes in network
    for(n in 1:num_of_nodes){
      #Look for a random node
      v <- sample(1:(num_of_nodes-1),1)
      node <- network$vertexes$node_id[network$vertexes$node_id==v]
      
      #Initialize the neighbors and weights
      for(i in 1:q){
        neighbours[i] <- 0
        weights[i] <- 0
      }
      
      #Loop over all links (=neighbors)
      l_iter <- network$func_edges %>% 
        filter(network$func_edges$func_start_node==network$vertexes$node_id[node] | 
                 network$func_edges$func_end_node==network$vertexes$node_id[node])
      
      for (j in 1:nrow(l_iter)){
        w <- l_iter$func_weight[j]
        
        #If node is the starting node for the current link, then n_cur becomes l_cur's ending node
        #otherwise it becomes l_cur's starting node
        if(node==l_iter$func_start_node[j]){
          n_cur <- l_iter$func_end_node[j]
        }else {
          n_cur <- l_iter$func_start_node[j]
        }
        
        #Add the link to the correct cluster
        neighbours[network$vertexes$community[network$vertexes$node_id==n_cur]] <- w + neighbours[network$vertexes$community[network$vertexes$node_id==n_cur]]
      }
      
      #We now have the weight of the (in and out) neighbors in each cluster available to us
      
    }
  }
  
}
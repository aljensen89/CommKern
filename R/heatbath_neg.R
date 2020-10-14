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
  delta_pos_out <- 0 #Positive outdegree for node, equation (1) from Traag and Bruggeman
  delta_pos_in <- 0 #Positive indegree for node, equation (1) from Traag and Bruggeman
  delta_neg_out <- 0 #Negative outdegree for node, equation (1) from Traag and Bruggeman
  delta_neg_in <- 0 #Negative indegree for node, equation (1) from Traag and Bruggeman
  k_v_pos_out <- 0 #gamma*(delta_pos_out/m_pt)
  k_v_pos_in <- 0 #gamma*(delta_pos_in/m_pt)
  k_v_neg_out <- 0 #lambda*(delta_neg_out/m_nt)
  k_v_neg_in <-0 #lambda*(delta_neg_in/m_nt)
  
  w <- 0 #Weight of the edge
  beta <- (1/t) #Weight for the probabilities
  r <- 0 #Random number used for assigning new spin
  
  maxweight <- 0
  sum_weights <- 0 #For normalizing the probabilities
  
  m_pt <- m_p #All positive indegree weights in the network, used for normalization
  m_nt <- m_n #All negative indegree weights in the network, used for normalization
  
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
      v <- sample(1:num_of_nodes,1)
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
        #Like A_{ij} in equation (10) from Traag and Bruggeman
        neighbours[network$vertexes$community[network$vertexes$node_id==n_cur]] <- w + neighbours[network$vertexes$community[network$vertexes$node_id==n_cur]]
      }
      
      #We now have the weight of the (in and out) neighbors in each cluster available to us
      old_spin <- spin[v]
      
      #Look for the optimal spin
      #Set the appropriate variables
      delta_pos_out <- degree_pos_out[v]
      delta_pos_in <- degree_pos_in[v]
      delta_neg_out <- degree_neg_out[v]
      delta_neg_in <- degree_neg_in[v]
      
      k_v_pos_out <- gamma*(delta_pos_out/m_pt)
      k_v_pos_in <- gamma*(delta_pos_in/m_pt)
      k_v_neg_out <- lambda*(delta_neg_out/m_nt)
      k_v_neg_in <- lambda*(delta_neg_in/m_nt)
      
      #The expectation value for the old spin
      if(directed_net==TRUE){
        exp_old_spin <- (k_v_pos_out*(degree_community_pos_in[old_spin]-delta_pos_in)-
                         k_v_neg_out*(degree_community_neg_in[old_spin]-delta_neg_in)+
                         k_v_pos_in*(degree_community_pos_out[old_spin]-delta_pos_out)-
                         k_v_neg_in*(degree_community_neg_out[old_spin]-delta_neg_out))
      } else {
        #Approximately equal to [m_{rs}^{+/-}], which is the expected number of arcs from community r to s
        exp_old_spin <- (k_v_pos_out*(degree_community_pos_in[old_spin]-delta_pos_in)-
                         k_v_neg_out*(degree_community_neg_in[old_spin]-delta_neg_in))
      }
      
      #Calculating the probabilities for each transition to another community
      weights[old_spin] <- 0
      
      for(spin_opt in 1:q) { #All possible new spins
        if(spin_opt!=old_spin){ #Except the old one!
          if(directed_net==TRUE){
            exp_spin <- ((k_v_pos_out*degree_community_pos_in[spin_opt])-
                         (k_v_neg_out*degree_community_neg_in[spin_opt])+
                         (k_v_pos_in*degree_community_pos_out[spin_opt])-
                         (k_v_neg_in*degree_community_neg_out[spin_opt]))
          } else {
            #Approximately equal to [m_{rs}^{+/-}], which is the expected number of arcs from community r to s
            exp_spin <- ((k_v_pos_out*degree_community_pos_in[spin_opt])-
                         (k_v_neg_out*degree_community_neg_in[spin_opt]))
          }
          #Difference in (observed-expected) between spin_opt and old_spin
          weights[spin_opt] <- ((neighbours[spin_opt]-exp_spin)-
                               (neighbours[old_spin]-exp_old_spin))
          
          if(weights[spin_opt]>maxweight) {
            maxweight <- weights[spin_opt]
          }
        }
      }
      
      #Calculate expected probability across all possible community designations
      sum_weights <- 0
      for (spin_opt in 1:q) { #All possible new spins
        weights[spin_opt] <- weights[spin_opt]-maxweight #Subtract maxweight for numerical stability (otherwise overflow)
        weights[spin_opt] <- exp(beta*weights[spin_opt]) #Probability statement from Traag and Bruggeman
        sum_weights <- sum_weights + weights[spin_opt]
      }
      
      #Choose a new spin dependent on the calculated probabilities
      r <- sample(0:sum_weights,1)
      new_spin <- 1
      found <- FALSE
      while(found==FALSE & new_spin<=q) {
        if(r<=weights[new_spin]){
          spin_opt <- new_spin #We have found a new spin
          found=TRUE
        } else {
          r <- r-weights[new_spin] #Perhaps the next spin is the one we want
        }
        new_spin <- new_spin+1
      }
      
      #Some weird thing happened: we haven't found a new spin when that shouldn't be the case
      #Numerical problems?
      if(found==FALSE) {
        problem_count <- problem_count+1
      }
      
      #If there wasn't a problem, we should have found our new spin
      new_spin <- spin_opt
      
      #The new spin is available to us, so change all appropriate counters
      if(new_spin!=old_spin){ #Did we really change something?
        changes <- changes+1
        spin[v] <- new_spin
      }
      
      #The new spin increases by one and the old spin decreases by one
      csize[new_spin] <- csize[new_spin]+1
      csize[old_spin] <- csize[old_spin]-1
      
      #Change the sums of degree for the old spin
      degree_community_pos_in[old_spin] <- degree_community_pos_in[old_spin]-delta_pos_in
      degree_community_neg_in[old_spin] <- degree_community_neg_in[old_spin]-delta_neg_in
      degree_community_pos_out[old_spin] <- degree_community_pos_out[old_spin]-delta_pos_out
      degree_community_neg_out[old_spin] <- degree_community_neg_out[old_spin]-delta_neg_out
      
      #And for the new spin
      degree_community_pos_in[new_spin] <- degree_community_pos_in[new_spin]+delta_pos_in
      degree_community_neg_in[new_spin] <- degree_community_neg_in[new_spin]+delta_neg_in
      degree_community_pos_out[new_spin] <- degree_community_pos_out[new_spin]+delta_pos_out
      degree_community_neg_out[new_spin] <- degree_community_neg_out[new_spin]+delta_neg_out
    }
  }
  
  acceptance <- changes/num_of_nodes/sweep
  return(acceptance)
}

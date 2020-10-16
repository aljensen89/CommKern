heatbath_multimodal <- function(gamma,alpha,temp,max_sweeps){
  sweep <- 0
  rn <- 0
  changes <- 1
  
  current_communities <- network$vertexes$community
  current_hamiltonian <- compute_multimodal_mod(modularity_matrix,guidance_matrix,
                                                     current_communities,alpha)
  
  while(sweep<max_sweeps){
    sweep <- sweep+1
    rn <- -1
    
    new_communities <- current_communities
    new_hamiltonian <- current_hamiltonian
    
    #Look for a random node
    while(rn<0 | rn>num_of_nodes){
      rn <- sample(1:num_of_nodes,1)
    }
    
    node <- network$vertexes$node_id[rn]
    deg <- network$vertexes$func_degree[node]
    
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
      }else{
        n_cur <- l_iter$func_start_node[j]
      }
    }
    
    #Search optimal spin
    old_spin <- network$vertexes$community[network$vertexes$node_id==node]
    spin_opt <- old_spin
    
    for(spin in 1:q){ #all possible new spins
      if(spin!=old_spin){ #except the old one
        new_communities[node] <- spin
        new_hamiltonian <- compute_multimodal_mod(modularity_matrix,guidance_matrix,
                                                       new_communities,alpha)
        
        if (new_hamiltonian<current_hamiltonian){
          current_communities <- new_communities
          changes <- changes+1
        }
        
        #Otherwise, move to it with some probability
        probOfMoving <- exp(-(new_hamiltonian-current_hamiltonian)/temp)
        
        if(runif(1,min=0,max=1)<probOfMoving){
            current_communities <- new_communities
          }
          current_hamiltonian <- new_hamiltonian
          changes <- changes+1
        }
      }
  }
  acceptance <- changes/num_of_nodes/sweep
  return(acceptance)
}
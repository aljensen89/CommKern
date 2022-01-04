heatbath_multimodal <- function(alpha,temp,max_sweeps){
  sweep <- 0
  rn <- 0
  changes <- 1
  
  current_communities <- net$vertexes$community
  current_hamiltonian <- compute_multimodal_mod(mod_matrix,net,current_communities,alpha)
  
  while(sweep<max_sweeps){
    sweep <- sweep+1
    rn <- -1
    
    new_communities <- current_communities
    new_hamiltonian <- current_hamiltonian
    
    #Look for a random node
    while(rn<0 | rn>num_of_nodes){
      rn <- sample(1:num_of_nodes,1)
    }
    
    node <- net$vertexes$node_id[rn]
    
    #Search optimal spin
    old_spin <- net$vertexes$community[net$vertexes$node_id==node]
    spin_opt <- old_spin
    
    for(spin in 1:q){ #all possible new spins
      if(spin!=old_spin){ #except the old one
        new_communities[rn] <- spin ##Fix how this is found for lower layers!
        new_hamiltonian <- compute_multimodal_mod(mod_matrix,net,new_communities,alpha)
        
        if (new_hamiltonian<current_hamiltonian){
          current_communities <- new_communities
          current_hamiltonian <- new_hamiltonian
          changes <- changes+1
        } else{
        #Otherwise, move to it with some probability
        probOfMoving <- exp(-(new_hamiltonian-current_hamiltonian)/temp)
        
        if(runif(1,min=0,max=1)<probOfMoving){
          current_communities <- new_communities
          current_hamiltonian <- new_hamiltonian
          changes <- changes+1
        }
        }
      }
    }
  }
  best_communities <<- current_communities
  best_hamiltonian <<- current_hamiltonian
  
  acceptance <- changes/(max_sweeps*q) #Proportion of changes that occurred divided by total possible changes
  return(acceptance)
}

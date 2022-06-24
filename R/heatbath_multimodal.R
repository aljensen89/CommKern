#' Multimodal heatbath algorithm
#' 
#' Description of the multimodal heatbath algorithm function.  
#' 
#' This is one of the two workhorse functions for the algorithm. The heatbath algorithm selects a
#' network node at random, calculates the multimodal modularity for the current configuration, and
#' then switches its community assignment to each possible community. If the modularity of this
#' iterated configuration is less than the current configuration, the new configuration is accepted
#' and the algorithm moves on to the next randomly chosen node. If this is not the case, the node is
#' moved to the new community assignment with some probability, which is a function of the current
#' modularity value, the iterated value, and the system's temperature.
#' Once the algorithm finishes with the randomly chosen node, this counts as a sweep. A new sweep
#' occurs, with the same steps taken as above, until the sweep number maxes out (usually set to 50
#' to balance computation time with robustness).
#' 
#' The function returns the acceptance of the heatbath algorithm for the given temperature.
#' 
#' @param net a \code{hms_network} object
#' @param alpha a double parameter balancing the use of the guidance matrix in modularity calculation
#' @param temp a double parameter found using the find_start_temp() function
#' @param max_sweeps an integer parameter of the maximum number of sweeps allowed at each temperature
#' 
#' @return acceptance value of the algorithm for the given temperature
#'   
#' @export
heatbath_multimodal <- function(net, alpha, temp, max_sweeps) {
  UseMethod("heatbath_multimodal")
}

#' @export
heatbath_multimodal.hms_network <- function(net, alpha, temp, max_sweeps) {
  sweep <- 0
  rn <- 0
  changes <- 1
  
  current_communities <- net$vertexes$community
  current_hamiltonian <- compute_multimodal_mod(net, mod_matrix, current_communities, alpha)
  
  while(sweep<max_sweeps){
    sweep <- sweep+1
    rn <- -1
    
    new_communities <- current_communities
    new_hamiltonian <- current_hamiltonian
    
    #Look for a random node
    while(rn<0 | rn>num_of_nodes){
      rn <- sample(1:num_of_nodes,1)
    }
    
    node <- net$network$vertexes$node_id[rn]
    
    #Search optimal spin
    old_spin <- net$network$vertexes$community[net$network$vertexes$node_id==node]
    spin_opt <- old_spin
    
    for(spin in seq(1, net$q, by = 1)){ #all possible new spins
      if(spin!=old_spin){ #except the old one
        new_communities[rn] <- spin
        new_hamiltonian <- compute_multimodal_mod(net, mod_matrix, new_communities, alpha)
        
        if (new_hamiltonian<current_hamiltonian){
          current_communities <- new_communities
          current_hamiltonian <- new_hamiltonian
          changes <- changes+1
        } else{
        #Otherwise, move to it with some probability
        probOfMoving <- exp(-(new_hamiltonian-current_hamiltonian)/temp)
        
        if(stats::runif(1,min=0,max=1)<probOfMoving){
          current_communities <- new_communities
          current_hamiltonian <- new_hamiltonian
          changes <- changes+1
        }
        }
      }
    }
  }
  net$best_communities <- current_communities
  net$best_hamiltonian <- current_hamiltonian
  net$acceptance <- changes/(max_sweeps*net$q) #Proportion of changes that occurred divided by total possible changes
  return(net)
}


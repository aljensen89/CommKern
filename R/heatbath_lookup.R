#' Heat bath lookup
#' 
#' Description of the heat bath lookup function.
#' 
#' From the pottsmodel_2 text file, translating the HeatBathLookup function. This is the
#' function that generally is used for optimization, as the parallel update has its flaws
#' due to the cyclic attractors
#' 
#' @param gamma double
#' @param prob double
#' @param kT double
#' @param max_sweeps integer
#' 
#' @return changes
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

heatbath_lookup <- function(gamma,prob,kT,max_sweeps) {
  new_spin <- 0.0 
  spin_opt <- 0.0 
  old_spin <- 0.0 
  
  sweep <- 0.0 
  max_q <- 0.0 
  rn < -0.0 
  
  w <- 0.0 
  degree <- 0.0 
  norm <- 0.0 
  r <- 0.0 
  beta <- 0.0 
  minweight <- 0.0 
  prefac <- 0.0 
  norm <- 0
  
  found <- FALSE
  sweep <- 0.0
  changes <- 1.0
  problemcount <- 0
  
  #num_of_nodes <- length(network$vertexes$node_id)
  
  while(sweep<max_sweep){
    sweep <- sweep+1
    
    #Loop over all nodes in network
    for (n in 0:num_of_nodes){
      rn <- -1
      
      while(rn<0 | rn>num_of_nodes){
        rn <- sample(1:num_of_nodes,1)
      }
      
      node <- network$vertexes$node_id[rn]
      
      #Initialize the neighbors and the weights
      for(i in 0:q){
        neighbors[i] <- 0
        weights[i] <- 0
      }
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
          
        neighbours[network$vertexes$community[network$vertexes$node_id==n_cur]] <- w + neighbours[network$vertexes$community[network$vertexes$node_id==n_cur]]
      }
      
      #Search optimal spin
      old_spin <- network$vertexes$community[network$vertexes$node_id==node]
        
      if (operation_mode==0){
        prefac <- 1.0
        delta <- 1.0
      }else if (operation_mode==1){ #Newman modularity
        prefac <- 1.0
        prob <- deg/total_degree_sum #Is total_degree_sum initialized somewhere else?
        delta <- deg
      }
      
      spin_opt <- old_spin
      beta <- (1/kT)*prefac
      minweight <- 0
      weights[old_spin] <- 0
      
      for(spin in 1:q){ #all possible new spins
        if(spin!=old_spin){ #except the old one
          h <- color_field[spin]-(color_field[old_spin]-delta)
          weights[spin] <- neighbors[old_spin]-neighbors[spin]+(gamma*prob*h)
          
          if (weights[spin]<minweight){
            minweight <- weights[spin]
          }
        }
      }
      
      for(spin in 1:q){ #all possible new spins
        weights[spin] <- weights[spin]-minweight
        weights[spin] <- exp(-beta*weights[spin])
        norm <- norm+weights[spin]
      }
        
      #Choose a new spin
      r <- runif(0,norm)
      new_spin <- 1
      found <- FALSE
        
      while(found==FALSE & new_spin<=q){
        if(r<=weights[new_spin]){
          spin_opt <- new_spin
          found <- TRUE
        }else{
          r <- r-weights[new_spin]
        }
        new_spin <- new_spin+1
      }
        
      if(found == FALSE){
        problemcount <- problemcount+1
      }
        
      #Now set the new spin
      if(new_spin!=old_spin){
        changes <- changes+1
        node<-Set_ClusterIndex(new_spin)
          
        color_field[old_spin] <- color_field[old_spin]-delta
        color_field[new_spin] <- color_field[new_spin]+delta
          
        #Q matrix update - iteration over all neighbors
        for (j in 1:nrow(l_iter)){
          w2 <- l_iter$func_weight[j]
            
          #If node is the starting node for the current link, then n_cur becomes l_cur's ending node
          #otherwise it becomes l_cur's starting node
          if(node==l_iter$func_start_node[j]){
            n_cur2 <- l_iter$func_end_node[j]
          }else{
            n_cur2 <- l_iter$func_start_node[j]
          }
          Qmatrix[old_spin,network$vertexes$community[network$vertexes$node_id==n_cur2]] <- Qmatrix[old_spin,network$vertexes$community[network$vertexes$node_id==n_cur2]]-w2
          Qmatrix[new_spin,network$vertexes$community[network$vertexes$node_id==n_cur2]] <- Qmatrix[new_spin,network$vertexes$community[network$vertexes$node_id==n_cur2]]+w2
          Qmatrix[network$vertexes$community[network$vertexes$node_id==n_cur2],old_spin] <- Qmatrix[network$vertexes$community[network$vertexes$node_id==n_cur2],old_spin]-w2
          Qmatrix[network$vertexes$community[network$vertexes$node_id==n_cur2],new_spin] <- Qmatrix[network$vertexes$community[network$vertexes$node_id==n_cur2],new_spin]+w2
            
          Qa[old_spin] <- Qa[old_spin]-w2
          Qa[new_spin] <- Qa[new_spin]+w2
        }
       }
      }
    }
    max_q <- 0
      
    for(i in 1:q){
      if(color_field[i]>max(q)){
        max_q <- color_field[i]+0.5
      }
    }
 acceptance <- changes/num_of_nodes/sweep
 return(acceptance)
}
    
      
      
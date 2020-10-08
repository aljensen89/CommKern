#' Heat bath lookup zero temp
#' 
#' Description of the heat bath parallel function at zero temp.  
#' 
#' From the pottsmodel_2 text file, translating the HeathBathLookupZeroTemp
#' function. The description in the C++ code is the following: the same function as 
#' before, but rather than parallel update, it picks the nocdes to update randomly
#' 
#' @param gamma double
#' @param prob double
#' @param max_sweeps integer
#' 
#' @return changes
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

heatbath_zerotemp <- function(gamma,prob,max_sweeps) {
  new_spin <- 0
  spin_opt <- 0
  old_spin <- 0
  spin <- 0
  r <- 0.0
  h <- 0.0
  delta <- 0.0
  deltaE <- 0.0
  deltaEmin <- 0.0
  w <- 0.0
  deg <- 0.0
  sweep <- 0
  changes <- 0
  #num_of_nodes <- length(network$vertexes$node_id)
  
  while(sweep < max_sweeps){
    sweep <- sweep+1
    
    #over all nodes in the network
    for(n in 1:num_of_nodes){
      r <- -1
      
      while(r < 0 | r > (num_of_nodes-1)){
        r <- sample(0:(num_of_nodes-1),1)
        
        node <- network$vertexes$node_id[r]
        n_cur <- NA
        
        #Count how many neighbors of each spin are present
        #First set everything to zero
        for(i in 0:q){
          neighbors[i] <- 0
          deg <- network$vertexes$func_degree[node]
          
          #Loop over all links (=neighbors)
          l_iter <- network$func_edges %>% 
            filter(network$func_edges$func_start_node==network$vertexes$node_id[node] | 
                     network$func_edges$func_end_node==network$vertexes$node_id[node])
          
          for (j in 1:nrow(l_iter)){
            w <- l_iter$func_weight[i]
            
            #If node is the starting node for the current link, then n_cur becomes l_cur's ending node
            #otherwise it becomes l_cur's starting node
            if(node==l_iter$func_start_node[i]){
              n_cur <- l_iter$func_end_node[i]
            }else{
                n_cur <- l_iter$func_start_node[i]
                }
            
            neighbours[network$vertexes$community[network$vertexes$node_id==n_cur]] <- w + neighbours[network$vertexes$community[network$vertexes$node_id==n_cur]]
          }
          
          #Search optimal spin
          old_spin <- network$vertexes$community[network$vertexes$node_id==node]
          
          if (operation_mode==0){
            delta <- 1.0
          }
          else if (operation_mode==1){ #Newman modularity
            prob <- deg/total_degree_sum #Is total_degree_sum initialized somewhere else?
            delta <- deg
          }
        }
        
        spin_opt <- old_spin
        for(spin in 1:q){ #all possible spins
          if(spin!=old_spin){
            h <- color_field[spin]+delta-color_field[old_spin]
            deltaE <- (neighbours[old_spin]-neighbours[spin])+(gamma*prob*h)
            if(deltaE<deltaEmin){
              spin_opt <- spin
              deltaEmin <- deltaE
            }
          }
        } #for spin
        
        #Now update the spins
        new_spin <- spin_opt
        
        if(new_spin!=old_spin){ #did we really change something?
          changes <- changes+1
          network$vertexes$community[network$vertexes$node_id==node] <- new_spin
          
          color_field[old_spin] <- color_field-1 
          color_field[new_spin] <- color_field+1 
          
          #Q matrix update - iteration over all neighbors
          l_iter2 <- network$func_edges %>% 
            filter(network$func_edges$func_start_node==network$vertexes$node_id[node] | 
                     network$func_edges$func_end_node==network$vertexes$node_id[node])
          
          for (j in 1:nrow(l_iter2)){
            w2 <- l_iter2$func_weight[i]
            
            #If node is the starting node for the current link, then n_cur becomes l_cur's ending node
            #otherwise it becomes l_cur's starting node
            if(node==l_iter$func_start_node[i]){
              n_cur2 <- l_iter2$func_end_node[i]
            }else{
              n_cur2 <- l_iter2$func_start_node[i]
            }
            
            Qmatrix[old_spin,Get_ClusterIndex(n_cur2)] <- Qmatrix[old_spin,Get_ClusterIndex(n_cur2)]-w2
            Qmatrix[new_spin,Get_ClusterIndex(n_cur2)] <- Qmatrix[new_spin,Get_ClusterIndex(n_cur2)]+w2
            Qmatrix[Get_ClusterIndex(n_cur2),old_spin] <- Qmatrix[Get_ClusterIndex(n_cur2),old_spin]-w2
            Qmatrix[Get_ClusterIndex(n_cur2),new_spin] <- Qmatrix[Get_ClusterIndex(n_cur2),new_spin]+w2
            
            Qa[old_spin] <- Qa[old_spin]-w2
            Qa[new_spin] <- Qa[new_spin]+w2
            
          } #while l_iter2
        }
      } #for n
    } #while markov
    
    acceptance <- changes/num_of_nodes/sweep
    return(acceptance)
  }
}

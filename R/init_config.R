#' Initial Configuration Assignment
#' 
#' Description of the initial configuration assignment function.  
#' 
#' From the pottsmodel_2 text file, translating the assign_initial_conf function. The
#' description in the C++ code is the following: assinging an initial random configuration
#' of spins to nodes if calledwith a negative argument or the spin used as argument when
#' called with a positive one.
#' 
#' @param spin an integer
#' 
#' @return the initial configuration of the model
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

init_config <- function(spin) {
  
  #Need to initialize color_field elsewhere in code
  ##color_field<-rep(0,q)
  s <- 0
  sum_weight <- 0
  av_k_squared <- 0
  av_k <- 0
  total_degree_sum <- 0
  
  for (i in 1:length(network$vertexes$node_id)){
    #If the spin is somehow <0, randomly assign s to an integer, otherwise assign as normal
    if(spin<0){s <- sample(1:q,1)
    }else{ 
      s <- spin
    }
    
    sum_weight <- network$vertexes$func_degree[i]
    
    network$vertexes$community[i] <- s
    
    #l_iter <- network$func_edges %>% 
    #  filter(network$func_edges$func_start_node==network$vertexes$node_id[i] | 
    #           network$func_edges$func_end_node==network$vertexes$node_id[i])
    
    av_k_squared <- av_k_squared + (sum_weight)^2
    av_k <- av_k + sum_weight
    
    #In case we want all links to contribute equally - parameter gamma is fixed
    if(operation_mode==0){
      color_field[i] <- color_field[i]+1
    } else{
        color_field[i] <- color_field[i]+sum_weight
    }
    
    #Or in the case we want to use a weight of each link that is proportional to k_i*k_j
    total_degree_sum <- total_degree_sum + sum_weight
    
  }
  
  av_k_squared <- av_k_squared / length(network$vertexes$node_id)
  av_k <- av_k / length(network$vertexes$node_id)
  
  return(network)
}

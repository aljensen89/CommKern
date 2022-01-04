#' Initial configuration assignment
#' 
#' Description of the initial configuration assignment function.  
#' 
#' Assigning an initial random configuration of spins to nodes if called with a negative 
#' argument or the spin used as argument when called with a positive one.
#' The function returns the initial random configuration of nodes to communities.
#' 
#' @param spin an integer indicating the maximum number of spins, or communities, that can be used
#' 
#' @return net list object with the communities portion of the list initially assigned
#'   
#' @export

init_config <- function(spin) {
  s <- 0
  sum_weight <- 0
  
  for (i in 1:length(net$vertexes$node_id)){
    #If the spin is somehow <0, randomly assign s to an integer, otherwise assign as normal
    if(spin<0){
      s <- sample(1:q,1)
    } else{ 
      s <- spin
    }
    
    sum_weight <- net$vertexes$func_degree[i]
    
    net$vertexes$community[i] <- s
  }
  return(net)
}

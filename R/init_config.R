#' Initial configuration assignment
#' 
#' Description of the initial configuration assignment function.  
#' 
#' From the pottsmodel_2 text file, translating the assign_initial_conf function. The
#' description in the C++ code is the following: assigning an initial random configuration
#' of spins to nodes if called with a negative argument or the spin used as argument when
#' called with a positive one.
#' The function returns the initial random configuration of nodes to communities.
#' 
#' @param spin integer
#' 
#' @return the initial configuration of the model
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
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

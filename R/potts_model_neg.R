#' Potts Model N Function
#' 
#' Description of the PottsNodelN function.  
#' 
#' From the pottsmodel_2 test file, translating the PottsModel function
#' 
#' @param network a network (potentially need more than one variable for this?)
#' @param num_communities an unsigned integer
#' @param directed a boolean
#' 
#' @return TBD
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export
potts_model <- function(network, num_communities, directed){
  
 net <- network
 q <- num_communities
 is_directed <- directed
 is_init <- FALSE
 num_nodes <- length(net$vertexes$node_id)
 
}
#' Potts Model Function
#' 
#' Description of the Potts model function.  
#' 
#' From the pottsmodel_2 test file, translating the PottsModel function
#' 
#' @param network a network (potentially need more than one variable for this?)
#' @param qvalue an unsigned integer
#' @param m an integer
#' 
#' @return TBD
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export
Potts_Model <- function(network, qvalue, m){
  
  #initialize iter, n_cur, i_ptr, net, q, operation_mode
  
  k_max=0
  Qa <- rep(NA, q+1)
  weights <- rep(NA, q+1)
  color_field <- rep(NA, q+1)
  neighbors <- rep(NA, q+1)
  
  num_of_nodes <- length(node_list)
  num_of_links <- length(link_list)
  
  n_cur <- iter[i] #also calling the node list here; using an iter.First call
  #Documentation says these lists are needed to keep track of spin states for parallel update mode
  
  new_spins <- vector()
  previous_spins <- vector()
  
  while(!iter[q+1]) {
    if(k_max < degree(n_cur)) {
      k_max <- degree(n_cur)
    }
    i_ptr <- 0
    #new_spins <- Push(i_ptr)
    
    i_ptr <- 0
    #previous_spins <- Push(i_ptr)
    
    n_cur <- iter[i+1]
  }
}

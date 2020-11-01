multimodal_spinglass <- function(functional_matrix,structural_matrix,network,spins,
                                 alpha,coolfact,gamma){
  
  ##Initializing variables for the function
  net <- network
  changes <- 1
  q <- spins
  num_of_nodes <- length(net$vertexes$node_id)
  best_communities <- rep(NA,num_of_nodes)
  best_hamiltonian <- NA
  mod_matrix <- compute_modularity_matrix(functional_matrix,net)
  
  ##Checks on the function input values
  if (spins < 2 | spins > length(net$vertexes$node_id)) {
    stop("Invalid number of spins")
  }
  if (coolfact < 0 | coolfact >= 1) {
    stop("Invalid temperature cooling factor")
  }
  if (gamma < 0) {
    stop("Invalid gamma value")
  }
  if(alpha < 0) {
    stop("Invalid alpha value")
  }
# if(starttemp < 0){
#    stop("Invalid starttemp value")
#  }
  
  ##Finding the inital temperature for the heatbath_multimodal function
  #initial_temp <- find_start_temp(gamma,alpha,starttemp)
  initial_temp <- 10000
  
  ##Initial random configuration of the nodes to spin states/communities
  net <- init_config(-1)
  
  temp <- initial_temp
    
  while(changes > 0 & temp > 1e-8){
    acc <- heatbath_multimodal(gamma,alpha,temp,100)
    if(acc < (1-(1/spins)*0.01)){
      changes <- 0
    } else{
      changes <- 1
    }
   temp <- temp*coolfact
  }
  
  net$vertexes$community <- best_communities
  return(net)
}
  
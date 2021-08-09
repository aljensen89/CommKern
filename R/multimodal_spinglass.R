multimodal_spinglass <- function(network,spins,alpha,coolfact,gamma){
  
  #Initializing variables
  net <- network
  changes <- 1
  q <- spins
  num_of_nodes <- length(net$vertexes$node_id)
  best_communities <- rep(NA,num_of_nodes)
  best_hamiltonian <- NA
  mod_matrix <- compute_modularity_matrix(net)
  
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

  start.time<-Sys.time()
  
  ##Initial random configuration of the nodes to spin states/communities
  #net <- init_config(-1)
  
  ##Finding the inital temperature for the heatbath_multimodal function
  initial_temp <- find_start_temp(gamma,alpha,1)
  #initial_temp <- 10000
  
  temp <- initial_temp
    
  while(changes > 0 & temp > 1e-6){
    acc <- heatbath_multimodal(gamma,alpha,temp,50)
    if(acc < (1-(1/spins))*0.05){
      changes <- 0
    } else{
      changes <- 1
    }
   temp <- temp*coolfact
   net$vertexes$community <- best_communities
  }
  
  #net$vertexes$community <- best_communities
  
  end.time<-Sys.time()
  
  time.taken<-end.time-start.time
  
  return(net)
}
  
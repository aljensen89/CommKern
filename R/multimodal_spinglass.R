multimodal_spinglass <- function(functional_matrix,structural_matrix,network,spins,
                                 alpha,starttemp,stoptemp,coolfact,gamma){
  
  ##Initializing variables for the function
  changes <- 0
  mod_matrix <- compute_modularity_matrix(functional_matrix,network)
  
  ##Checks on the function input values
  if (spins < 2 | spins > length(network$vertexes$node_id)) {
    stop("Invalid number of spins")
  }
  if (coolfact < 0 | coolfact >= 1) {
    stop("Invalid temperature cooling factor")
  }
  if (gamma < 0) {
    stop("Invalid gamma value")
  }
  if (starttemp/stoptemp < 1) {
    stop("The starttemp value should be larger in absolute value than the stoptemp")
  }
  
  if(alpha < 0) {
    stop("Invalid alpha value")
  }
  
  ##Finding the inital temperature for the heatbath_multimodal function
  initial_temp <- FindStartTemp(gamma,alpha,starttemp)
  
  ##Initial random configuration of the nodes to spin states/communities
  network <- init_config(-1)
  
  temp <- initial_temp
  while(temp > stoptemp){
    acc <- heatbath_multimodal(gamma,alpha,temp,max_sweeps)
    if(acc < (1-(1/spins)*0.01)){
      changes <- 0
    } else{
      changes <- 1
    }
   temp <- temp*coolfact
  }
}
  
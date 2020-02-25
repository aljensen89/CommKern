#' Initialize Lookup
#' 
#' Description of the initialize lookup function.  
#' 
#' From the pottsmodel_2 text file, translating the initialize_lookup function
#' 
#' @param kT double
#' @param gamma double
#' 
#' @return ???
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

init_lookup <- function(kT, gamma){
  
  #The lookup table contains all entries of exp{-beta*(-neighbors+gamma*h)} as needed in HeatBath algorithm
  beta <- 1.0/kT
  
  for (w in 0:(k_max+num_of_nodes)){
    neg_lookup[w] <- exp((-beta)*(-w))
  }
  delta_ij[1] <- 1.0
  
  for (w in (num_of_nodes-k_max):(num_of_nodes+k_max)){
    #This appears to be empty in the C++ code
  }
  
  for (n in 1:num_of_nodes){
    gamma_term[n] <- exp(-n/(kT*gamma))
  }
  gamma_term[1] <- 1.0
}
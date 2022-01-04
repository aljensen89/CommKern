#' Starting temperature
#' 
#' Description of the starting temperature function.  
#' 
#' Within the spinglass algorithm, we would like to start from a temperature
#' with at least 95 of all proposed spin changes accepted in 50 sweeps over the network.
#' The function returns the temperature found.
#' 
#' @param alpha a double parameter balancing the use of the guidance matrix in modularity calculation
#' @param ts the starting temperature for the search, set to 1 within the algorithm 
#' 
#' @return kT the starting temperature that meets the criteria specified above
#'
#' @export

find_start_temp <- function(alpha,ts) {
  kT <- ts
  acceptance <- 0
  
  net<<-init_config(-1) #calling the init_config() function
  
  while(acceptance < (1-(1/q))*0.95){ #want 95% acceptance
    kT <- kT*1.1
    acceptance <- heatbath_multimodal(alpha,kT,50)
  }
  kT <- kT*1.1
  return(kT)
}

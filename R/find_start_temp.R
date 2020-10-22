#' Starting temperature
#' 
#' Description of the starting temperature function.  
#' 
#' From the pottsmodel_2 text file, translating the FindStartTemp function. The
#' description in the C++ code is the following: we would like to start from a temperature
#' with at least 95 of all proposed spin changes accepted in 50 sweeps over the network.
#' The function returns the temperature found.
#' 
#' @param gamma double
#' @param prob double
#' @param ts double
#' 
#' @return kT
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

find_start_temp <- function(gamma,alpha,ts) {
  kT <- ts
  
  init_config(-1) #calling the init_config() function
  
  while(acceptance<(1.0-((1.0/q)*0/95))){ #want 95% acceptance
    kT <- kT*1.1
    heatbath_multimodal(gamma,alpha,kT,50) #calling the heatbath_lookup() function
  }
  kT <- kT*1.1 #just to be sure (of what??)
  return(kT)
}

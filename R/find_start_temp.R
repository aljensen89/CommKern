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
#' @export

find_start_temp <- function(gamma,alpha,ts) {
  kT <- ts
  acceptance <- 0
  
  net<<-init_config(-1) #calling the init_config() function
  
  while(acceptance < (1-(1/q))*0.95){ #want 95% acceptance
    kT <- kT*1.1
    acceptance <- heatbath_multimodal(gamma,alpha,kT,50)
  }
  kT <- kT*1.1
  return(kT)
}

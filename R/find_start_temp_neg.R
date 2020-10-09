#' Starting temperature for negative negative
#' 
#' Description of the starting temperature negative function.  
#' 
#' From the pottsmodel_2 text file, translating the FindStartTemp function. The
#' description in the C++ code is the following: we would like to start from a temperature
#' with at least 95 of all proposed spin changes accepted in 50 sweeps over the network.
#' The function returns the temperature found.
#' 
#' @param gamma double
#' @param lambda double
#' @param ts double
#' 
#' @return kT
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

find_start_temp_neg <- function(gamma,lambda,ts) {
  kT <- ts
  
  #Assign random initial condition
  network<-init_config_neg(TRUE)
  
  #The factor 1-(1/q) is important since even at infinite temperature, 1-(1/q) of all spins do
  #change their state as a randomly chosen new state is with probability (1/q) the old state
  acceptance <- 0
  while (acceptance<((1-(1/q))*0.95)){ #Want 95% acceptance
    kt <- kT*1.1
    acceptance <- HeatBathLookup_Neg(gamma,lambda,kT,50)
  }
  kT <- kt*1.1 #Just to be sure
  return(kT)
}

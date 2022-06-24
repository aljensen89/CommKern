#' Starting temperature
#' 
#' Description of the starting temperature function.  
#' 
#' Within the spinglass algorithm, we would like to start from a temperature
#' with at least 95 of all proposed spin changes accepted in 50 sweeps over the network.
#' The function returns the temperature found.
#' 
#' @param net a \code{hms_network} object
#' @param alpha a double parameter balancing the use of the guidance matrix in modularity calculation
#' @param ts the starting temperature for the search, set to 1 within the algorithm 
#' 
#' @return kT the starting temperature that meets the criteria specified above
#'
#' @export
find_start_temp <- function(net, alpha, ts) {
  UseMethod("find_start_temp")
}

#' @export
find_start_temp.hms_network <- function(net, alpha, ts) {
  kT <- ts
  net$acceptance <- 0
  
  while(net$acceptance < (1-(1/net$q))*0.95){ #want 95% acceptance
    kT <- kT * 1.1
    net <- heatbath_multimodal(net = net, alpha = alpha, temp = kT, max_sweeps = 50)
  }

  kT <- kT * 1.1
  return(kT)
}

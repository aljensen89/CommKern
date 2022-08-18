#' Starting temperature
#' 
#' Description of the starting temperature function.  
#' 
#' Within the spinglass algorithm, we would like to start from a temperature
#' with at least 95 of all proposed spin changes accepted in 50 sweeps over the network.
#' The function returns the temperature found.
#' 
#' @param net a \code{spinglass_net} object
#' @param mod_matrix mod_matrix
#' @param spins spins
#' @param alpha a double parameter balancing the use of the guidance matrix in modularity calculation
#' @param ts the starting temperature for the search, set to 1 within the algorithm 
#' 
#' @return the starting temperature that meets the criteria specified above
#'
find_start_temp <- function(net, mod_matrix, spins, alpha, ts) {
    kT <- ts
    acceptance <- 0

    while (acceptance < (1 - (1/spins)) * 0.95) {
        # want 95% acceptance
        kT <- kT * 1.1
        acceptance <- heatbath_multimodal(net = net, mod_matrix = mod_matrix, spins = spins,
            alpha = alpha, temp = kT, max_sweeps = 50)$acceptance
    }

    kT <- kT * 1.1

    kT
}


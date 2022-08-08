#' Multimodal heatbath algorithm
#' 
#' Description of the multimodal heatbath algorithm function.  
#' 
#' This is one of the two workhorse functions for the algorithm. The heatbath
#' algorithm selects a network node at random, calculates the multimodal
#' modularity for the current configuration, and then switches its community
#' assignment to each possible community. If the modularity of this iterated
#' configuration is less than the current configuration, the new configuration
#' is accepted and the algorithm moves on to the next randomly chosen node. If
#' this is not the case, the node is moved to the new community assignment with
#' some probability, which is a function of the current modularity value, the
#' iterated value, and the system's temperature. Once the algorithm finishes
#' with the randomly chosen node, this counts as a sweep. A new sweep occurs,
#' with the same steps taken as above, until the sweep number maxes out (usually
#' set to 50 to balance computation time with robustness).
#' 
#' @param net a \code{hms_network} object
#' @param mod_matrix mod_matrix
#' @param spins spins
#' @param alpha a double parameter balancing the use of the guidance matrix in
#' modularity calculation
#' @param temp a double parameter found using the find_start_temp() function
#' @param max_sweeps an integer parameter of the maximum number of sweeps
#' allowed at each temperature
#' 
#' @return acceptance value of the algorithm for the given temperature
#'   
#' @export
heatbath_multimodal <- function(net, mod_matrix, spins, alpha, temp, max_sweeps) {
    UseMethod("heatbath_multimodal")
}

#' @export
heatbath_multimodal <- function(net, mod_matrix, spins, alpha, temp, max_sweeps) {
    sweep <- 0
    rn <- 0
    changes <- 1

    current_communities <- net$vertexes$community
    current_hamiltonian <- compute_multimodal_mod(mod_matrix = mod_matrix, net = net,
        communities = current_communities, alpha = alpha)

    while (sweep < max_sweeps) {
        sweep <- sweep + 1
        rn <- -1

        new_communities <- current_communities
        new_hamiltonian <- current_hamiltonian

        # Look for a random node
        num_of_nodes <- length(net$vertexes$node_id)
        while (rn < 0 | rn > num_of_nodes) {
            rn <- sample(1:num_of_nodes, 1)
        }

        node <- net$vertexes$node_id[rn]

        # Search optimal spin
        old_spin <- net$vertexes$community[net$vertexes$node_id == node]

        for (spin in 1:spins) {
            # all possible new spins except the old one
            if (spin != old_spin) {
                new_communities[rn] <- spin
                new_hamiltonian <- compute_multimodal_mod(mod_matrix = mod_matrix,
                  net = net, communities = new_communities, alpha = alpha)

                if (new_hamiltonian < current_hamiltonian) {
                  current_communities <- new_communities
                  current_hamiltonian <- new_hamiltonian
                  changes <- changes + 1
                } else {
                  # Otherwise, move to it with some probability
                  probOfMoving <- exp(-(new_hamiltonian - current_hamiltonian)/temp)

                  if (stats::runif(1, min = 0, max = 1) < probOfMoving) {
                    current_communities <- new_communities
                    current_hamiltonian <- new_hamiltonian
                    changes <- changes + 1
                  }
                }
            }
        }
    }

    acceptance <- changes/(max_sweeps * spins)  #Proportion of changes that occurred divided by total possible changes
    return(list(acceptance = acceptance, best_communities = current_communities,
        best_hamiltonian = current_hamiltonian))
}

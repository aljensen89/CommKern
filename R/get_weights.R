#' Simulated network edge weights
#'
#' Description of the simulated network edge weights function.
#'
#' This is an ancillary function that creates a vector of edge weights sampled
#' from Beta distributions. Within and between community edge weights are each
#' sampled from a distinct Beta distribution. If 'fuzzy' communities wish to be
#' created, a third Beta distribution is specified and the communities for which
#' their distinction is 'fuzzy' also needs to be specified. This vector of edge
#' weights is then passed to \code{\link{group_network_perturb}} to create the
#' final simulated network object.
#'
#' @param network_df a data frame containing information about network nodes,
#' their community assignment, and all node dyads, coming from
#' \code{\link{simnet_df_perturb}}
#' @param wcr within community edge weights, sampled from a beta distribution;
#' for example, c(8,8) will ask for the within community edge weights to be
#' sampled from a Beta(8,8) distribution
#' @param bcr between community edge weights, sampled from a beta distribution;
#' for example, c(1,8) will ask for the between community edge weights to be
#' sampled from a Beta(1,8) distribution
#' @param bfcr fuzzy community edge weights, sampled from a beta distribution;
#' for example, c(4,8) will ask for the fuzzy community edge weights to be
#' sampled from a Beta (4,8) distribution
#' @param fuzzy_comms the communities for which their distinction is 'fuzzy,' or
#' not as distinct; fuzzy communities tend to have higher between community edge
#' weights; for example, c('comm_a','comm_c') will create a fuzzy distinction
#' between communities a and c
#'
#' @return a vector of edge weights associated with the node dyads from the
#' network data frame
#'
get_weights <- function(network_df, wcr, bcr, bfcr = NA, fuzzy_comms = NA) {

    # Create empty vector for all weights
    weights = rep(NA_real_, time = nrow(network_df))

    # Go through all dyads and sample from respective distribution, dependent
    # on the community membership of both nodes
    for (i in seq_along(weights)) {
        node_A_comm <- network_df[i, "Node_A_Comm"]
        node_B_comm <- network_df[i, "Node_B_Comm"]
        # Both nodes in same community
        if (node_A_comm == node_B_comm) {
            weights[i] <- stats::rbeta(n = 1, shape1 = wcr[1], shape2 = wcr[2])
        }
        # Nodes in different community
        if (node_A_comm != node_B_comm) {
            weights[i] <- stats::rbeta(n = 1, shape1 = bcr[1], shape2 = bcr[2])
        }

        # If some communities have a 'fuzzier' relationship with each other,
        # use specified distributions for this kind of relationship
        if (all(!is.na(bfcr)) & all(!is.na(fuzzy_comms)) & node_A_comm != node_B_comm &
            node_A_comm %in% fuzzy_comms & node_B_comm %in% fuzzy_comms) {
            weights[i] <- stats::rbeta(n = 1, shape1 = bfcr[1], shape2 = bfcr[2])
        }
    }
    return(weights)
}

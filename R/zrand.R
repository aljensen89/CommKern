#' Rand z-score
#' 
#' Description of the Rand z-score function.  
#' 
#' This is an ancillary function that calculates the Rand z-score between two
#' partitions, which is used in the consensus similarity function
#' 
#' @param part1 a partition of nodes to communities or clusters
#' @param part2 a partition of nodes to communities or clusters
#' 
#' @return the Rand z-score between two partitions
#' 
zrand <- function(part1, part2) {
    if (length(part1) == 1) {
        part1 <- t(part1)
    }

    if (length(part2) == 1) {
        part2 <- t(part2)
    }
    if (anyNA(part1) | anyNA(part2)) {
        stop("NAs are not supported")
     }

    if (length(part1) != length(part2)) {
        stop("Partitions must be of the same length")
    }

    # Generate contingency table and calculate row/column marginals
    res <- sort_pairs(part1, part2)
    N <- length(part1)
    stot <- sum(choose(res$nij, 2), na.rm = TRUE)
    srow <- sum(choose(res$ni., 2), na.rm = TRUE)
    scol <- sum(choose(res$n.j, 2), na.rm = TRUE)

    # Calculating the Rand and adjusted Rand indices Rand index
    SR <- 1 + (sum(res$nij^2) - (sum(res$ni.^2) + sum(res$n.j^2))/2)/choose(N, 2)

    ## Adjusted Rand index
    expectedIndex <- (srow * scol)/(choose(N, 2))
    maximumIndex <- (srow + scol)/2
    if (expectedIndex == maximumIndex & stot != 0) {
        SAR <- 1
    } else {
        SAR <- (stot - expectedIndex)/(maximumIndex - expectedIndex)
    }

    # The adjusted coefficient is calculated by subtracting the expected value
    # and rescale the result by the difference between the maximum allowed
    # value and the mean value

    # Calculate the variance and z-score of Rand
    C1 <- 4 * sum(res$n.j^3) - 8 * (N + 1) * srow + N * (N^2 - 3 * N - 2)
    C2 <- 4 * sum(res$n.j^3) - 8 * (N + 1) * scol + N * (N^2 - 3 * N - 2)

    ## Calculate the variance of the Rand coefficient
    M <- N * (N - 1)/2

    var <- M/16 - (4 * srow - 2 * M)^2 * (4 * scol - 2 * M)^2/(256 * M^2) + C1 *
        C2/(16 * N * (N - 1) * (N - 2)) + ((4 * srow - 2 * M)^2 - 4 * C1 - 4 * M) *
        ((4 * scol - 2 * M)^2 - 4 * C2 - 4 * M)/(64 * N * (N - 1) * (N - 2) * (N -
        3))

    ## Calculate the z-score of the Rand coefficient
    zRand <- (stot - expectedIndex)/sqrt(var)

    return(zRand)
}

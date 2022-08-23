#' Count pairs
#'
#' Description of the count pairs function.
#'
#' A function to count pairs of integers or factors and identify the pair counts
#'
#' @param a a vector of classifications
#' @param b a vector of classifications
#' @param order a vector of permutations (coming from the order() function in base R)
#'
#' @return a list of five different vectors:
#' \itemize{
#'   \item{\code{pair_nb}}{: a vector containing counts of nodes within all possible
#'   classification pairs from partitions a and b}
#'   \item{\code{pair_a}}{: a vector of the same length as pair_nb, specifying the order
#'   of classifications in pair_nb from partition a}
#'   \item{\code{pair_b}}{: a vector of the same length as pair_nb, specifying the order
#'   of classifications in pair_nb from partition b}
#'   \item{\code{a_nb}}{: a vector containing counts of nodes within each class for
#'   partition a}
#'   \item{\code{b_nb}}{: a vector containing counts of nodes within each class for
#'   partition b}
#' }
#'
count_pairs <- function(a, b, order) {
    n <- length(a)

    # Count per classification
    count1 <- rep(0, n)
    for (i in 1:n) {
        count1[a[i] + 1] <- count1[a[i] + 1] + 1
    }
    count2 <- rep(0, n)
    for (i in 1:n) {
        count2[b[i] + 1] <- count2[b[i] + 1] + 1
    }

    # Count per pairs
    count <- 1
    class1_cur <- a[order[1] + 1]
    class2_cur <- b[order[1] + 1]

    for (j in 1:n) {
        if (class1_cur != a[order[j] + 1] | class2_cur != b[order[j] + 1]) {
            count <- count + 1
            class1_cur <- a[order[j] + 1]
            class2_cur <- b[order[j] + 1]
        }
    }

    # Create output integer vector for pairs
    name_a <- rep(0, count)
    name_b <- rep(0, count)
    number_pair <- rep(0, count)

    current_position <- 1
    name_a[1] <- a[order[1] + 1]
    name_b[1] <- b[order[1] + 1]
    number_pair[1] <- 1

    # Count pairs
    for (k in 2:n) {
        if (name_a[current_position] == a[order[k] + 1] & name_b[current_position] ==
            b[order[k] + 1]) {
            number_pair[current_position] <- number_pair[current_position] + 1
        } else {
            current_position <- current_position + 1
            name_a[current_position] <- a[order[k] + 1]
            name_b[current_position] <- b[order[k] + 1]
            number_pair[current_position] <- 1
        }
    }

    # Output as list
    ListOut <- list(pair_nb = number_pair, pair_a = name_a, pair_b = name_b, a_nb = count1[count1 >
        0], b_nb = count2[count2 > 0])
    return(ListOut)
}

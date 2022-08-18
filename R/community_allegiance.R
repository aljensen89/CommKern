#' Community Allegiance
#' 
#' Description of the community allegiance function.  
#' 
#' This function calculates the community allegiance of each node in a network.
#' For node i, the stability of its allegiance to community A is calculated as
#' the number of times where node i belongs to community A, divided by the total
#' number of runs. This measure is bounded in [0,1], where higher values of
#' stability indicate that a node belong to a single community across a greater
#' number of runs.
#' 
#' The function returns a square matrix whose values are bounded in [0,1],
#' where higher values in the off diagonal indicate that the two nodes belong
#' to the same community over a higher proportion of partitions. 
#' 
#' @param comm_matrix a matrix whose first column is the node label/id and all
#' subsequent columns are different partitions
#' 
#' @return a matrix whose values are bounded in [0,1], where higher values 
#' in the off diagonal indicate that the two nodes belong to the same 
#' community over a higher proportion of runs.
#' 
#' @examples
#' 
#' set.seed(7)
#' x <- sample(x = rep(1:3, 4), 12)
#' 
#' set.seed(18)
#' y <- sample(x = rep(1:3, 4), 12)
#' 
#' set.seed(3)
#' z <- sample(x = rep(1:3, 4), 12)
#' 
#' xyz_comms <- data.frame(id=seq(1:length(x)),x_comm=x,y_comm=y,z_comm=z)
#' xyz_alleg <- community_allegiance(xyz_comms)
#' 
#' xyz_melt <- reshape2::melt(xyz_alleg)
#'
#' ggplot2::ggplot(data = xyz_melt) +
#'  ggplot2::theme_minimal() +
#'  ggplot2::aes(x = as.factor(Var1), y = as.factor(Var2), fill = value) +
#'  ggplot2::geom_tile() +
#'  ggplot2::xlab('Node') + ggplot2::ylab('Node') +
#'  ggplot2::ggtitle('Community Allegiance Example') +
#'  ggplot2::scale_fill_gradient2(
#'    low  = 'navy',
#'    high = 'goldenrod1',
#'    mid  = 'darkturquoise', 
#'    midpoint = 0.5,
#'    limit = c(0, 1),
#'    space = 'Lab', 
#'    name='')
#' 
#' @export
#' 
community_allegiance <- function(comm_matrix) {
    alleg_matrix <- matrix(data = 0, nrow = nrow(comm_matrix), ncol = nrow(comm_matrix))

    for (k in 2:ncol(comm_matrix)) {
        for (i in 1:nrow(alleg_matrix)) {
            for (j in 1:ncol(alleg_matrix)) {
                if (comm_matrix[comm_matrix[, 1] == i, k] == comm_matrix[comm_matrix[,
                  1] == j, k]) {
                  alleg_matrix[i, j] <- alleg_matrix[i, j] + 1
                } else {
                  alleg_matrix[i, j] <- alleg_matrix[i, j] + 0
                }
            }
        }
    }
    alleg_matrix_norm <- alleg_matrix/(ncol(comm_matrix) - 1)
    return(alleg_matrix_norm)
}

#' Extrinsic evaluation distance matrix creation
#'
#' Description of the extrinsic evaluation distance matrix creation function.
#'
#' This function creates a distance matrix using the community output values
#' from any community detection algorithm, such as the hierarchical multimodal
#' spinglass algorithm. Because extrinsic evaluation metrics for clustering
#' algorithms use the underlying idea of similarity, distance is calculated as
#' (1-similarity). The use of distance ensures that the distance matrix will be
#' positive and semi-definite, a requirement for its use in the kernel function.
#'
#' The function returns an m x m matrix (where m is the number of partitions) to be used as input for
#' the kernel function.
#'
#' @param comm_df a dataframe whose columns are different partitions. All partitions must have the same
#' set of nodes in order for this function to work and this dataframe should exclude a node ID column
#' for ease of computation.
#' @param variant a string in ('NMI', 'Adj_RI', 'purity') that calculates different external cluster
#' evaluation metrics.
#' 
#' @seealso \code{\link{adj_RI}}, \code{\link{NMI}}, and \code{\link{purity}}
#'
#' @return the extrinsic evaluation distance matrix to be used as input for the kernel function
#' 
#' @examples
#' set.seed(7)
#' x <- sample(x = rep(1:3, 4), 12)
#' 
#' set.seed(18)
#' y <- sample(x = rep(1:3, 4), 12)
#' 
#' set.seed(3)
#' z <- sample(x = rep(1:3, 4), 12)
#' 
#' xyz_comms <- data.frame(x_comm=x,y_comm=y,z_comm=z)
#' ext_distance(xyz_comms,variant="NMI")
#' ext_distance(xyz_comms,variant="adj_RI")
#' ext_distance(xyz_comms,variant="purity")
#'
#' @export

ext_distance <- function(comm_df,variant=c("NMI","adj_RI","purity")){
  variant <- match.arg(variant)

  comm_expand <- expand.grid(1:ncol(comm_df),1:ncol(comm_df))
  colnames(comm_expand) <- c("id_a","id_b")
  comm_expand$ext_sim <- NA

  for (i in 1:nrow(comm_expand)){
    pair_compare <- switch(variant,NMI=NMI(comm_df[,comm_expand$id_a[i]],comm_df[,comm_expand$id_b[i]],variant="max"),
                           adj_RI=adj_RI(comm_df[,comm_expand$id_a[i]],comm_df[,comm_expand$id_b[i]]),
                           purity=purity(comm_df[,comm_expand$id_a[i]],comm_df[,comm_expand$id_b[i]]))
    comm_expand$ext_sim[i] <- 1-pair_compare
  }

  comm_dist <- as.matrix(reshape2::dcast(data=comm_expand,formula=id_a~id_b,value.var='ext_sim'))[,-1]
  return(comm_dist)
}

#' Hamiltonian distance matrix creation
#'
#' Description of the Hamiltonian distance matrix creation function.
#'
#' This function creates a distance matrix using the Hamiltonian output values from a community detection algorithm that
#' implements a Hamiltonian value, such as the hierarchical multimodal spinglass algorithm. To ensure a positive,
#' semidefinite matrix (as required for the kernel function), the absolute difference between Hamiltonian values is calculated.
#'
#' The function returns an m x m matrix (where m is the number of networks) to be used as input for
#' the kernel function.
#'
#' @param hamil_df a dataframe containing a column for network ID and another column containing Hamiltonian values
#'
#' @return hamil_dist, the Hamiltonian distance matrix to be used as input for the kernel function
#'
#' @export

ham_distance <- function(hamil_df){
  hamil_expand <- tidyr::expand_grid(hamil_df$id,hamil_df$id)
  colnames(hamil_expand) <- c("id_a","id_b")
  hamil_expand$hamil_diff <- NA

  for (i in 1:nrow(hamil_expand)){
    hamil_expand$hamil_diff[i] <- sqrt(abs(hamil_df$hamil[hamil_expand$id_a[i]]-hamil_df$hamil[hamil_expand$id_b[i]])^2)
  }

  hamil_dist <- as.matrix(reshape2::dcast(data=hamil_expand,formula=id_a~id_b,value.var='hamil_diff'))[,-1]
  return(hamil_dist)
}

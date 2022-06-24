#' Compute modularity matrix
#' 
#' Description of the compute modularity matrix function.  
#' 
#' Calculates the modularity matrix, which is the the difference between the observed adjacency
#' matrix and the expected adjacency matrix (from a null model). This is only computed for the
#' main componenet of network information, not accounting for the guidance. For neuroimaging
#' application, this function would be computing the modularity matrix for the functional
#' connectivity aspect of the network object.
#' The function takes in a network object and returns the modularity matrix.
#' 
#' @param net a \code{spinglass_net} object (see \code{\link{matrix_to_df}} for more details)
#' 
#' @return mod_matrix
#'   
#' @export
compute_modularity_matrix <- function(net) {
  UseMethod("compute_modularity_matrix")
}

#' @export
compute_modularity_matrix.spinglass_net <- function(net) {
  m <- 0.5*sum(net$func_matrix)
  
  mod_matrix <- matrix(0,nrow=nrow(net$func_matrix),ncol=ncol(net$func_matrix))
  
  for(i in 1:nrow(net$func_matrix)){
    d_i <- net$vertexes$func_degree[i]
    for(j in 1:ncol(net$func_matrix)){
      A_ij <- net$func_matrix[i,j]
      d_j <- net$vertexes$func_degree[j]
      
      #Compute the null model
      null_probability <- (d_i*d_j)/(2*m)
      
      #Compute the modularity
      M_ij <- A_ij - null_probability
      mod_matrix[i,j] <- M_ij
    }
  }
  return(mod_matrix)
}

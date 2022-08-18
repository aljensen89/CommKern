library(CommKern)

# Verifying output of compute_modularity_matrix
mod_matrix_alt <- function(net){
  m <- 0.5*sum(net$func_matrix)
  mod_matrix <- matrix(0,nrow=nrow(net$func_matrix),ncol=ncol(net$func_matrix))

  for (i in 1:nrow(net$func_matrix)){
    d_i <- net$vertexes$func_degree[i]
    for (j in 1:ncol(net$func_matrix)){
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

stopifnot(all(mod_matrix_alt(SBM_net)==CommKern:::compute_modularity_matrix(SBM_net)))

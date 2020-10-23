compute_modularity_matrix <- function(functional_matrix,net){
  m <- 0.5*sum(functional_matrix)
  
  mod_matrix <- matrix(0,nrow=nrow(functional_matrix),ncol=ncol(functional_matrix))
  
  for(i in 1:nrow(functional_matrix)){
    d_i <- net$vertexes$func_degree[i]
    for(j in 1:ncol(functional_matrix)){
      A_ij <- functional_matrix[i,j]
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

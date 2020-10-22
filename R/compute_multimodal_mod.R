#-\sum_{i \neg j} M_{ij} \delta(C_i,C_j) - \alpha \sum_{i \neg j} S_{ij} \delta(C_i,C_j)
compute_multimodal_mod <- function(mod_matrix,structural_matrix,communities,alpha){
  sum <- 0
  
  for(i in 1:nrow(mod_matrix)){
    for(j in 1:ncol(mod_matrix)){
      if(i==j){
        next
      }
      #Only contribute to the value if delta(C_i, C_j) == 1
      if(communities[i] != communities[j]){
        next
      }
      M_ij <- mod_matrix[i,j]
      S_ij <- structural_matrix[i,j]
      
      sum <- sum+M_ij+(alpha*S_ij) #We're adding instead of subtracting, and will negate later
    }
  }
  return(-1*sum) #Negate the sum, since we're adding instead of subtracting 
}

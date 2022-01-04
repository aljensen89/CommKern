#' Compute multimodal modularity matrix
#' 
#' Description of the compute multimodal modularity matrix function.  
#' 
#' Calculates the multimodal version of the modularity matrix, which is detailed in the accompanying
#' manuscript as the following:
#' \sum_{i \neg j} M_{ij} \delta(C_i,C_j) - \alpha \sum_{i \neg j} S_{ij} \delta(C_i,C_j).
#' This function incorporates both the modularity matrix calculated from the compute_mod_matrix()
#' function and adds the additional component of a guidance matrix. The alpha parameter controls
#' the extent to which the guidance matrix influences the modularity, where alpha=0 means the
#' function reverts to the typical modularity calculation and alpha > 0 allows for some influence
#' of the guidance matrix. The guidance matrix will not penalize the modularity if two nodes are not
#' connected within it; it will only decrease the modularity if the two nodes have guidance information.
#' The function takes in a network object, the mod_matrix output from compute_mod_matrix(),
#' a vector of communities, and a parameter alpha and returns the multimodal modularity matrix.
#' 
#' @param net list
#' @param mod_matrix matrix
#' @param communities vector
#' @param alpha double
#' 
#' @return multimodal modularity matrix
#'   
#' @export

compute_multimodal_mod <- function(mod_matrix,net,communities,alpha){
  sum <- 0
  
  for(i in 1:nrow(mod_matrix)){
    for(j in 1:ncol(mod_matrix)){
      if(i==j){
        next
      }

      if(communities[i] != communities[j]){
        next
      }
      M_ij <- mod_matrix[i,j]
      S_ij <- net$str_matrix[i,j]
      
      sum <- sum+(M_ij+(alpha*S_ij)) #We're adding instead of subtracting, and will negate later
    }
  }
  return(-1*sum) #Negate the sum, since we're adding instead of subtracting 
}

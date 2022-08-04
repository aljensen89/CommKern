#' Consensus similarity
#' 
#' Description of the consensus similarity function.  
#' 
#' This function identifies a single representative partition from a set of partitions that is the most
#' similar to the all others. Here, similarity is taken to be the z-score of the Rand coefficient.
#' 
#'The function returns the consensus partition, which is determined by the maximum average pairwise similarity.
#' 
#' @param comm_matrix a matrix whose columns are different partition and whose rows are nodes within a network
#' 
#' @return consensus_part, the consensus partition determined by the maximum average pairwise similarity
#' 
#' @export
#' 
consensus_similarity <- function(comm_matrix){
  t_comm_matrix <- comm_matrix
  
  npart <- nrow(t_comm_matrix)
  
  #Initialize variables
  pairwise_sim <- matrix(NA,nrow=npart,ncol=npart)
  
  #Calculate the pairwise similarities
  for (i in 1:(npart-1)){
    for (j in i:npart){
      pairwise_sim[i,j] <- zrand(t_comm_matrix[i,],t_comm_matrix[j,])
    }
  }
  
  pairwise_sim <- as.matrix(Matrix::forceSymmetric(pairwise_sim,uplo="U"))
  
  #Average pairwise similarity
  average_pairwise_sim <- rowSums(pairwise_sim)/(npart-1)
  
  #Extract partition most similar to others
  consensus_similarity <- max(average_pairwise_sim)
  consensus_part <- t_comm_matrix[which.max(average_pairwise_sim),]
  
  return(consensus_part)
}
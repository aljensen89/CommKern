#' Community allegiance
#' 
#' Description of the community allegiance function.  
#' 
#' This function calculates the community allegiance of each node in a network. For node i, the stability of its 
#' allegiance to community A is calculated as the number of times where node i belongs to community A, divided by
#' the total number of runs. This measure is bounded in [0,1], where higher values of stability indicate
#' that a node belong to a single community across a greater number of runs.
#' 
#' The function returns the consensus partition, which is determined by the maximum average pairwise similarity.
#' 
#' @param comm_matrix a matrix whose first column is the node label/id and all subsequent columns are different partitions
#' 
#' @return alleg_matrix_norm, a matrix whose values are bounded in [0,1], where higher values indicate that a node belongs to a
#' single community over a higher proporition of runs
#' 
#' @export
#' 
community_allegiance <- function(comm_matrix){
  alleg_matrix <- matrix(data=0,nrow=nrow(comm_matrix),ncol=nrow(comm_matrix))
  
  for (k in 2:ncol(comm_matrix)){
    for (i in 1:nrow(alleg_matrix)){
      for (j in 1:ncol(alleg_matrix)){
        if (comm_matrix[comm_matrix$node_id==i,k]==comm_matrix[comm_matrix$node_id==j,k]){
          alleg_matrix[i,j] <- alleg_matrix[i,j] + 1
        } else{
          alleg_matrix[i,j] <- alleg_matrix[i,j] + 0
        }
      }
    }
  }
  alleg_matrix_norm <- alleg_matrix/(ncol(comm_matrix)-1)
  return(alleg_matrix_norm)
}
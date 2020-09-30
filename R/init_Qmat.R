#' Initialize Q Matrix
#' 
#' Description of the initialize Q matrix function.  
#' 
#' From the pottsmodel_2 text file, translating the initialize_Qmatrix function. From the
#' documentation in the C++ code: Q denotes the modularity of the network. This function
#' calculates it initially. In the event of a spin changing its state, it only needs
#' updating. Note that Qmatrix and Qa are only counting! The normalization by num_of_links
#' is done later.
#' 
#' @param network list
#' 
#' @return ???
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

init_Qmat <- function(network){
  #Need to create Qmatrix and Qa somewhere else in the code
  #Dimensions determined by q - the max number of communities specified a priori
  ##Qmatrix<-matrix(0L,ncol=q,nrow=q)
  ##Qa<-rep(0,q)

  #Go over all links and make corresponding entries in Q matrix
  #An edge connecting state i with state j will get and entry in Q_ij and Q_ji
  
  for (k in 1:nrow(network$func_edges)){
    i <- network$vertexes$community[network$vertexes$node_id==network$func_edges[k,1]]
    j <- network$vertexes$community[network$vertexes$node_id==network$func_edges[k,2]]
    
    Qmatrix[i,j] <- Qmatrix[i,j]+network$func_edges[k,3]
    Qmatrix[j,i] <- Qmatrix[j,i]+network$func_edges[k,3]
  }
  
  for (i in 1:q){
    Qa[i] <- rowSums(Qmatrix)[i]
  }
  calculate_Q() #This is a function!
}
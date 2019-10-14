#' Initialize Q Matrix
#' 
#' Description of the initialize Q matrix function.  
#' 
#' From the pottsmodel_2 text file, translating the initialize_Qmatrix function. From the
#' documentation in the C++ code: Q denotes the modularity of the network. This function
#' calculates it initially. In the event of a sping changing its state, it only needs
#' updating. Note that Qmatrix and Qa are only counting! The normalization by num_of_links
#' is done later.
#' 
#' @return ???
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

init_Qmat <- function(){
  #DLList_Iter<NLink*> l_iter
  #NLink *l_cur
  
  num_of_links <- length(link_list)
  
  for (i in 0:q){
    Qa[i] <- 0.0
    for (j in i:q){
      Qmatrix[i,j] <- 0.0
      Qmatrix[j,i] <- 0.0
    }
  }
  
  l_cur <- l_iter[i] #also calling the links list here; using an l_iter.First call
  
  while (!l_iter[q]){
    i <- l_cur #also calling a Get_ClusterIndex function from Get_Start
    j <- l_cur #also galling a Get_ClusterIndex function from Get_End
    
    Qmatrix[i,j] <- Qmatrix[i,j]+l_cur #also calling a Get_Weight function here
    Qmatrix[j,i] <- Qmatrix[j,i]+l_cur #also calling a Get_Weight function here
    
    l_cur <- l_iter[i+1]
  }
  
  for (i in 0:q){
    for (j in 0:q){
      Qa[i] <- Qa+Qmatrix[i,j]
    }
  }
  calculate_Q
}
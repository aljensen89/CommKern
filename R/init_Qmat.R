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
#' @return ???
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

init_Qmat <- function(){
  #DLList_Iter<NLink*> l_iter; ##Creating an iteration using the NLink constructor, function holding edge-based info
  #NLink *l_cur; ##The current link being examined
  
  num_of_links <- length(link_list) #num_of_links=net->link_list->Size();
  
  #Initialize with zeros
  for (i in 0:q){
    Qa[i] <- 0.0
    for (j in i:q){
      Qmatrix[i,j] <- 0.0
      Qmatrix[j,i] <- 0.0
    }
  }
  
  #Go over all links and make corresponding entries in Q matrix
  #An edge connecting state i with state j will get and entry in Q_ij and Q_ji
  
  #l_cur=l_iter.First(net->link_list); ##From links of n_cur, first link in list is assigned as l_cur
  
  while (!iter.End()){ #Figure out how to code this into R, until the end of the iteration...
    i <- Get_ClusterIndex(Get_Start(l_cur)) #grab cluster index for the starting node in l_cur's list
    j <- Get_ClusterIndex(Get_End(l_cur)) #grab cluster index for the ending node in l_cur's list
    
    Qmatrix[i,j] <- Qmatrix[i,j]+Get_Weight(l_cur) 
    Qmatrix[j,i] <- Qmatrix[j,i]+Get_Weight(l_cur) 
    
    #l_cur=l_iter.Next();
  }
  
  for (i in 0:q){
    for (j in 0:q){
      Qa[i] <- Qa+Qmatrix[i,j]
    }
  }
  calculate_Q() #This is a function!
}
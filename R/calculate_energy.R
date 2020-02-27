#' Calculate energy
#' 
#' Description of the calculate energy function.  
#' 
#' From the pottsmodel_2 text file, translating the calculate_energy function. The
#' documentation from the C++ code is the following: this function calculates the energy
#' for the standard Hamiltonian given a particular value of gamma and the current spin
#' states.
#' 
#' @param gamma
#' 
#' @return e
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

calculate_energy <- function(gamma){
  e <- 0
  
  #DLList_Iter<NLink*> l_iter; ##Creating an iteration using the NLink constructor, function holding edge-based info
  #NLink *l_cur; ##The current link being examined
  
  #l_cur=l_iter.First(net->link_list); ##From the network's link list, grab first one
  
  while (!iter.End()){ #Figure out how to code this into R, until the end of the iteration...
    #If the cluster index for the nodes connected by the current link are the same, then the energy goes down by one
    if(Get_ClusterIndex(Get_Start(l_cur))==Get_ClusterIndex(Get_End(l_cur))) {
      e <- e-1
      }
    #l_cur=l_iter.Next(); ##Move to the next link in the node's link list, making it the current link
  }
  
  #And the penalty term contributes according to cluster sizes
  for(i in 1:q){
    e <- e+(gamma*0.5*color_field[i]*(color_field[i]-1))
  }
  
  energy <- e #Where is "energy" being called in the C++ script?
  return(e)
}
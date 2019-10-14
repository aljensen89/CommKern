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
  e <- 0.0
  
  #DLList_Iter<NLink*> l_iter
  #NLink *l_cur
  
  l_cur <- l_iter[i] #also calling the links list here; using an l_iter.First call
  
  while(!l_iter[q+1]){
    #if(L=l_cur->Get_Start()->Get_ClusterIndex()==l_cur->Get_End()->Get_ClusterIndex())
    #  {
    #  e <- e-1
    #  }
      l_cur <- l_iter[i+1]
  }
  
  for(i in 1:q){
    e <- e+(gamma*0.5*color_field[i]*(color_field[i]-1))
  }
  
  energy <- e
  e
}
#' Heat bath lookup zero temp
#' 
#' Description of the heat bath parallel function at zero temp.  
#' 
#' From the pottsmodel_2 text file, translating the HeathBathLookupZeroTemp
#' function. The description in the C++ code is the following: the same function as 
#' before, but rather than parallel update, it picks the nocdes to update randomly
#' 
#' @param gamma double
#' @param prob double
#' @parama max_sweeps integer
#' 
#' @return changes
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

heatbath_zerotemp <- function(gamma,prob,max_sweeps) {
  #DLList_Iter<NNode*> iter, net_iter
  #DLList_Iter<NLink*> l_iter
  #DLList_Iter<unsigned int*> i_iter, i_iter2
  #NNode *node, *n_cur
  #NLink *l_cur
  
  new_spin <- 0.0 #unsigned integer
  spin_opt <- 0.0 #unsigned integer
  old_spin <- 0.0 #unsigned integer
  spin <- 0.0 #unsigned integer
  r <- 0.0 #long
  
  #unsigned long changes
  
  h<- 0.0 #double
  delta <- 0.0 #double
  deltaE <- 0.0 #double
  deltaEmin <- 0.0 #double
  w <- 0.0 #double
  degree <- 0.0 #double
  
  sweep <- 0
  changes <- 0
  
  while(sweep<max_sweeps){
    sweep <- sweep+1
    
    #over all nodes in the network
  }
}

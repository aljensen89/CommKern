#' Heat bath parallel lookup zero temp
#' 
#' Description of the heat bath parallel lookup function at zero temp.  
#' 
#' From the pottsmodel_2 text file, translating the HeathBathParallelLookupZeroTemp
#' function. The description in the C++ code is the following: this function does a
#' parallel update at zero T. Hence, it is really fast on easy problems. Max sweeps is
#' the maximum number of sweeps it should perform, if it does not converge earlier.
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

heatbath_parallel_zerotemp <- function(gamma,prob,max_sweeps) {
  #DLList_Iter<NNode*> iter, net_iter
  #DLList_Iter<NLink*> l_iter
  #DLList_Iter<unsigned int*> i_iter, i_iter2
  #NNode *node, *n_cur
  #NLink *l_cur
  #unsigned in *SPIN, *P_SPIN, 
  
  new_spin <- 0.0 #unsigned integer
  spin_opt <- 0.0 #unsigned integer
  old_spin <- 0.0 #unsigned integer
  spin <- 0.0 #unsigned integer
  sweep <- 0.0 #unsigned integer
  
  #unsigned long changes
  
  h<- 0.0 #double
  delta <- 0.0 #double
  deltaE <- 0.0 #double
  deltaEmin <- 0.0 #double
  w <- 0.0 #double
  degree <- 0.0 #double
  
  cyclic <- FALSE #boolean
  
  changes <- 1
  
  while(sweep<max_sweeps & changes){ #what does the changes part mean?
    cyclic <- TRUE
    sweep <- sweep+1
    changes <- 0
    
    #Loop over all nodes
    node <- node_list[1] #also calling net_iter from .First call
    SPIN <- new_spins[1] #also calling i_iter from .First call
    
    while(!net_iter[length(net_iter)]){
      #How many neighbors of each type? Set them all to zero
      for(i in 0:q){
        neighbours[i] <- 0.0
        degree <- Get_Weight[i]
        #Loop over all link (=neighbours)
        #l_cur=l_iter.First(node->Get_Links())
        
        while(!l_iter[length(l_iter)]){
          
        }
      }
    }
  }
}

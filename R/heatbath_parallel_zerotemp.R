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
          w <- Get_Weight(l_cur)
          ifelse(node=Get_Start(l_cur),n_cur<-Get_End(l_cur),n_cur<-Get_Start(l_cur))
          
          neighbours[Get_ClusterIndex(n_cur)] <- w+neighbours
          
          #l_cur=l_iter.Next()
        }
        #Search optimal spin
        old_spin <- Get_ClusterIndex(node)
        
        if (operation_mode==0){
          delta <- 1.0
        }
        else if (operation_mode==1){ #Newman modularity
          prob <- degree/total_degree_sum
          delta <- degree
        }
      }
      
      spin_opt <- old_spin
      for(spin in 1:q){
        if(spin!=old_spin){
          h <- color_field[spin]+delta-color_field[old_spin]
          deltaE <- as.numeric(neightbours[old_spin]-neighbours[spin])+(gamma*prob*h)
          if(deltaE<deltaEmin){
            spin_opt <- spin
            deltaEmin <- deltaE
          }
        }
      }
      
      #*SPIN=spin_opt
      node <- net_iter[i+1] #uses the net_iter.Next() call
      SPIN <- i_iter[i+1] #uses the i_iter.Next() call
    }
    
    #----------Now set all spins to new values----------
    #node=net_iter.First(net->node_list) 
    #SPIN=i_iter.First(new_spins)
    #P_SPIN=i_iter2.First(previous_spins)
    
    while(!net_iter[length(net_iter)]){
      old_spin <- Get_ClusterIndex(node)
      #new_spin=*SPIN
      
      if(new_spin!=old_spin){
        changes <- changes+1
        node <- Set_ClusterIndex(new_spin) #But in C++ this is using a ->
        
        #In parallel update, there occur cyclic attractors of size two
        #This makes the program run forever
        
        if(new_spin!=P_SPIN){ #actually *P_SPIN
          cyclic <- FALSE
          #*P_SPIN=old_spin
          color_field[old_spin] <- color_field-1 #-- argument in C++
          color_field[new_spin] <- color_field+1 #++ argument in C++
          
          #Q matrix update
          #l_cur=l_iter.First(node->Get_Links())
          while(!l_iter[length(l_iter)]){
            w <- Get_Weight(l_cur)
            if(node==Get_Start(l_cur)){
              n_cur <- Get_End(l_cur)
            }
            else {
              n_cur <- Get_Start(l_cur)
            }
            Qmatrix[old_spin,Get_ClusterIndex(n_cur)] <- Qmatrix[old_spin,Get_ClusterIndex(n_cur)]-w
            Qmatrix[new_spin,Get_ClusterIndex(n_cur)] <- Qmatrix[new_spin,Get_ClusterIndex(n_cur)]+w
            Qmatrix[Get_ClusterIndex(n_cur),old_spin] <- Qmatrix[Get_ClusterIndex(n_cur),old_spin]-w
            Qmatrix[Get_ClusterIndex(n_cur),new_spin] <- Qmatrix[Get_ClusterIndex(n_cur),new_spin]+w
            
            Qa[old_spin] <- Qa[old_spin]-w
            Qa[new_spin] <- Qa[new_spin]+w
            
            #l_cur=l_iter.Next()
          }
        }
        
        #node=net_iter.Next()
        #SPIN=i_iter.Next()
        #P_SPIN=i_ier2.Next()
      }
    }
    
    #In case of a cyclic attractor, we want to interrupt
    if(cyclic){
      print("Cyclic attractor!")
      acceptance <- 0.0
    }
    
    else {
      acceptance <- as.numeric(changes)/as.numeric(num_of_nodes)
      changes
    }
  }
}

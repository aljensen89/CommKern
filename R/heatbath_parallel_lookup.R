#' Heat bath parallel lookup
#' 
#' Description of the heat bath parallel lookup function.
#' 
#' From the pottsmodel_2 text file, translating the HeatBathParallelLookup function. This
#' function performs a parallel update at temperature T
#' 
#' @param gamma double
#' @param prob double
#' @param kT double
#' @param max_sweeps integer
#' 
#' @return changes
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

heatbath_parallel_lookup <- function(gamma,prob,kT, max_sweeps) {
  #DLList_Iter<NNode*> iter, net_iter
  #DLList_Iter<NLink*> l_iter
  #DLList_Iter<unsigned int*> i_iter, i_iter2
  #NNode *node, *n_cur
  #NLink *l_cur
  
  new_spin <- 0.0 #unsigned integer
  spin_opt <- 0.0 #unsigned integer
  old_spin <- 0.0 #unsigned integer
  
  #unsigned int *SPIN, *P_SPIN
  
  sweep <- 0.0 #unsigned integer
  max_q <- 0.0 #long
  
  #unsigned long changes, problemcount
  
  h <- 0.0 #double
  delta <- 0.0 #double
  norm <- 0.0 #double
  r <- 0.0 #double
  beta <- 0.0 #double
  minweight <- 0.0 #double
  prefac <- 0.0 #double
  w <- 0.0 #double
  degree <- 0.0 #double
  
  cyclic <- FALSE #boolean
  found <- FALSE #boolean
  
  #unsigned long num_of_nodes
  
  sweep <- 0.0
  changes <- 1.0
  
  num_of_nodes <- Size(node_list) #uses call num_of_nodes=net->node_list->Size()
  
  while(sweep<max_sweep & changes){ #what is the second component of this statement?
    cyclic <- TRUE
    sweep <- sweep+1
    changes <- 0
    
    #Loop over all nodes
    node <- node_list[1] #also calling net_iter from .First call
    SPIN <- new_spins[1] #also calling i_iter from .First call
    
    while(!net_iter[length(net_iter)]){
      
      #Initialize neighbors and weights
      problemcount <- 0
      for(i in 1:q){
        neighbors[i] <- 0
        weights[i] <- 0
      }
      norm <- 0
      degree<-Get_Weight(node)
      
      #Loop over all link (=neighbors)
      #l_cur=l_iter.First(node->Get_Links())
      
      while(!l_iter[length(l_iter)]){
        w <- Get_Weight(l_cur)
        
        if(node==Get_Start(l_cur)){
          n_cur<-Get_End(l_cur)
        }
        else{
          n_cur<-Get_Start(l_cur)
        }
        
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
    beta <- 1/kT*prefac
    minweight <- 0
    weights[old_spin] <- 0
    
    #Loop over all possible new spins
    for(spin in 1:q){
      if(spin!=old_spin){ #only if different from old_spin
        h <-color_field[spin]+delta-color_field[old_spin]
        weights[spin] <- (neighbours[old_spin]-neighbours[spin])+(gamma*prob*h)
        if(weights[spin]<minweight){
          minweight <- weights[spin]
        }
      }
    }
      #Loop over all possible spins
      for (spin in 1:q){
        weights[spin] <- weights[spin]-minweight #subtract minweight
        weights[spin] <- exp{-beta*weights[spin]}
        norm <- norm+weights[spin]
      }
      
      #Now choose a new spin
      r <- runif(0,norm)
      new_spin <- 1
      found <- FALSE
      
      while(!found & new_spin<=q){
        if(r<=weights[new_spin]){
          spin_opt <- new_spin
          found <- TRUE
          break
        }
        else{
          r <- r-weights[new_spin]
        }
        new_spin <- new_spin+1
      }
      if(!found){
        problemcount <- problemcount+1
      }
      #Put new spin on list
      #*SPIN=spin_opt
      #node=net_iter.Next()
      #SPIN=i_iter.Next()
  }
  
  #Now update all spins
  #node=net_iter.First(net->node_list)
  #SPIN=i_iter.First(new_spins)
  #P_SPIN=i_iter2.First(previous_spins)
  
  while(!net_iter[length(net_iter)]){
    old_spin <- Get_ClusterIndex(node)
    #new_spin=*SPIN
    
    if(new_spin!=old_spin){
      changes <- changes+1
      node<-Set_ClusterIndex(new_spin)
      
      #if(new_spin!=*P_SPIN) cyclic=FALSE
      #*P_SPIN=old_spin
      
      color_field[old_spin] <- color_field[old_spin]-delta
      color_filed[new_spin] <- color_filed[new_spin]+delta
      
      #Q matrix update - iteration over all neighbors
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
    #P_SPIN=i_iter2.Next()
  }
  max_q <- 0
  
  for(i in 1:q){
    if(color_field[i]>max(q)){
      max_q <- color_field[i]
    }
    
    if (cyclic & changes) {
      acceptance <- changes/num_of_nodes
      return(0)
    }
    else{}
      acceptance <- changes/num_of_nodes
      return(changes)
  }
}

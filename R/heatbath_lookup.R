#' Heat bath lookup
#' 
#' Description of the heat bath lookup function.
#' 
#' From the pottsmodel_2 text file, translating the HeatBathLookup function. This is the
#' function that generally is used for optimization, as the parallel update has its flaws
#' due to the cyclic attractors
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

heatbath_lookup <- function(gamma,prob,kT,max_sweeps) {
  #DLList_Iter<NNode*> iter, net_iter
  #DLList_Iter<NLink*> l_iter
  #DLList_Iter<unsigned int*> i_iter, i_iter2
  #NNode *node, *n_cur
  #NLink *l_cur
  
  new_spin <- 0.0 #unsigned integer
  spin_opt <- 0.0 #unsigned integer
  old_spin <- 0.0 #unsigned integer
  
  sweep <- 0.0 #unsigned integer
  max_q <- 0.0 #long
  rn < -0.0 #long
  
  #unsigned long changes, problemcount
  
  w <- 0.0 #double
  degree <- 0.0 #double
  norm <- 0.0 #double
  r <- 0.0 #double
  beta <- 0.0 #double
  minweight <- 0.0 #double
  prefac <- 0.0 #double
  
  found <- FALSE #boolean
  
 num_of_nodes <- 0.0 #long int
  
  sweep <- 0.0
  changes <- 1.0
  
  num_of_nodes <- Size(node_list) #uses call num_of_nodes=net->node_list->Size()
  
  while(sweep<max_sweep){
    sweep <- sweep+1
    
    #Loop over all nodes in network
    for (n in 0:(num_of_nodes-1)){
      rn <- rn-1
      
      while(rn<0 | rn>(num_of-nodes-1)){
        rn <- sample(0:(num_of_nodes-1),1)
        
        node<-Get(rn) #uses the node=net->node_list->Get(rn)
        
        #Initialize the neighbors and the weights
        problemcount <- 0
        for(i in 0:q){
          neighbors[i] <- 0
          weights[i] <- 0
        }
        norm <- 0
        degree <- Get_Weight(node)
        
        #Loop over all links (=neighbors)
        #l_cur=l_iter.First(node->Get_Links())
        
        while(!net_iter[length(net_iter)]){
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
        
        #Look for optimal spin
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
      
      for(spin in 1:q){ #all possible new spins
        if(spin!=old_spin){ #except the old one
          h <- color_field[spin]-(color_field[old_spin]-delta)
          weights[spin] <- neighbors[old_spin]-neighbors[spin]+(gamma*prob*h)
          
          if (weights[spin]<minweight){
            minweight <- weights[spin]
          }
        }
        
        for(spin in 1:q){ #all possible new spins
          weights[spin] <- weights[spin]-minweight
          weights[spin] <- exp{-beta*weights[spin]}
          norm <- norm+weights[spin]
        }
        
        #Choose a new spin
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
        
        #Now set the new spin
        if(new_spin!=old_spin){
          changes <- changes+1
          node<-Set_ClusterIndex(new_spin)
          
          color_field[old_spin] <- color_field[old_spin]-delta
          color_field[new_spin] <- color_field[new_spin]+delta
          
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
      }
    }
      max_q <- 0
      
      for(i in 1:q){
        if(color_field[i]>max(q)){
          max_q <- color_field[i]+0.5
        }
        
        acceptance <- changes/num_of_nodes/sweep
        return(acceptance)
      }
  }
}
    
      
      
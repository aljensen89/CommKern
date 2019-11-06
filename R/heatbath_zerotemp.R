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
#' @param max_sweeps integer
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
    for(n in 0:length(NNode)){
      r <- -1
      
      while(r<0 | r>(length(NNode)-1)){
        r<-sample(0:(length(NNode)-1),1)
        node<-node_list(r) #Uses the call node=net->node_list->Get(r)
        
        #Count how many neighbors of each spin are present first of all zero
        for(i in 0:q){
          neighbors[i]<-0
          degree<-Get_Weight(node)
          
          #Loop over all links (=neighbors)
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
        
        #Now update the spins
        #new_spin=spin_opt
        if(new_spin!=old_spin){
          changes<-changes+1
          node<-Set_ClusterIndex(node)
          
          color_field[old_spin] <- color_field-1 #-- argument in C++
          color_field[new_spin] <- color_field+1 #++ argument in C++
          
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
    
    acceptance<-changes/num_of_nodes/sweep
    acceptance
  }
}

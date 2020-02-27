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
  #DLList_Iter<NNode*> iter, net_iter ##Creating two iterations using the NNode constructor, function holding node-based info
  #DLList_Iter<NLink*> l_iter ##Creating an iteration using the NLink constructor, function holding edge-based info
  #DLList_Iter<unsigned int*> i_iter, i_iter2 ##Creating two iterations, not using a constructor
  #NNode *node, *n_cur ##The current node being examined (x2)
  #NLink *l_cur ##The current link being examined
  
  new_spin <- 0 #unsigned integer
  spin_opt <- 0 #unsigned integer
  old_spin <- 0 #unsigned integer
  spin <- 0 #unsigned integer
  r <- 0.0 #long
  
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
    for(n in 0:num_of_nodes){ #Need to get total number of nodes
      r <- -1
      
      while(r<0 | r>(num_of_nodes-1)){
        r<-sample(0:(num_of_nodes-1),1)
        #node=net->node_list->Get(r); ##from the node list of the network, grab the rth one and make it node
        
        #Count how many neighbors of each spin are present first of all zero
        for(i in 0:q){
          neighbors[i]<-0
          degree<-Get_Weight(node)
          
          #Loop over all links (=neighbors)
          #l_cur=l_iter.First(node->Get_Links()) ##For the node, get all links and grab first one
          
          while(!l_iter.End()){
            w <- Get_Weight(l_cur)
            
            #If node is the starting node for the current link, then n_cur becomes l_cur's ending node
            #otherwise it becomes l_cur's starting node
            ifelse(node==Get_Start(l_cur), n_cur<-Get_End(l_cur), n_cur<-Get_Start(l_cur))
            
            neighbours[Get_ClusterIndex(n_cur)] <- w+neighbours
            #l_cur=l_iter.Next() ##Move on to the next link in the iterator
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
        for(spin in 1:q){ #all possible spins
          if(spin!=old_spin){
            h <- color_field[spin]+delta-color_field[old_spin]
            deltaE <- as.numeric(neightbours[old_spin]-neighbours[spin])+(gamma*prob*h)
            if(deltaE<deltaEmin){
              spin_opt <- spin
              deltaEmin <- deltaE
            }
          }
        } #for spin
        
        #Now update the spins
        new_spin=spin_opt
        
        if(new_spin!=old_spin){ #did we really change something?
          changes<-changes+1
          node <- Set_ClusterIndex(new_spin) #But in C++ this is using a -> ASK PETER ABOUT THIS
          
          color_field[old_spin] <- color_field-1 #-- argument in C++
          color_field[new_spin] <- color_field+1 #++ argument in C++
          
          #Q matrix update - iteration over all neighbors
          #l_cur=l_iter.First(node->Get_Links()) ##For node, look at all its links and grab first one
          while(!l_iter.End()){ #Until the end of the link iterator...
            w <- Get_Weight(l_cur)
            
            #If node is the starting node for the current link, then n_cur becomes l_cur's ending node
            #otherwise it becomes l_cur's starting node
            ifelse(node==Get_Start(l_cur), n_cur <- Get_End(l_cur), n_cur <- Get_Start(l_cur))
            
            Qmatrix[old_spin,Get_ClusterIndex(n_cur)] <- Qmatrix[old_spin,Get_ClusterIndex(n_cur)]-w
            Qmatrix[new_spin,Get_ClusterIndex(n_cur)] <- Qmatrix[new_spin,Get_ClusterIndex(n_cur)]+w
            Qmatrix[Get_ClusterIndex(n_cur),old_spin] <- Qmatrix[Get_ClusterIndex(n_cur),old_spin]-w
            Qmatrix[Get_ClusterIndex(n_cur),new_spin] <- Qmatrix[Get_ClusterIndex(n_cur),new_spin]+w
            
            Qa[old_spin] <- Qa[old_spin]-w
            Qa[new_spin] <- Qa[new_spin]+w
            
            #l_cur=l_iter.Next() #Next link in the iterator
          } #while l_iter
        }
      } #for n
    } #while markov
    
    acceptance<-changes/num_of_nodes/sweep
    return(acceptance)
  }
}

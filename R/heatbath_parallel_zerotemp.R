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
#' @param max_sweeps integer
#' 
#' @return changes
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

heatbath_parallel_zerotemp <- function(gamma,prob,max_sweeps) {
  #DLList_Iter<NNode*> iter, net_iter ##Creating two iterations using the NNode constructor, function holding node-based info
  #DLList_Iter<NLink*> l_iter ##Creating an iteration using the NLink constructor, function holding edge-based info
  #DLList_Iter<unsigned int*> i_iter, i_iter2 ##Creating two iterations, not using a constructor
  #NNode *node, *n_cur ##The current node being examined (x2)
  #NLink *l_cur ##The current link being examined
  #unsigned int *SPIN, *P_SPIN ##Values at memory locations for pointers SPIN and P_SPIN
  
  new_spin <- 0 #unsigned integer
  spin_opt <- 0 #unsigned integer
  old_spin <- 0 #unsigned integer
  spin <- 0 #unsigned integer
  sweep <- 0 #unsigned integer
  
  changes <- 0.0 #unsigned long
  
  h<- 0.0 #double
  delta <- 0.0 #double
  deltaE <- 0.0 #double
  deltaEmin <- 0.0 #double
  w <- 0.0 #double
  degree <- 0.0 #double
  
  cyclic <- FALSE #boolean
  
  changes <- 1
  
  while(sweep<max_sweeps & changes==1){ #in C++ code, only want to run when sweep<max_sweeps and changes is non-zero
    cyclic <- TRUE
    sweep <- sweep+1
    changes <- 0
    
    #Loop over all nodes
    #node=net_iter.First(net->node_list); ##From network's node list, grab first one
    #SPIN=i_iter.First(new_spins); ##Grab first one from new spins??
    
    while(!net_iter.End()){ #until you get to the end of the network/node iterator...
      #How many neighbors of each type? Set them all to zero
      for(i in 0:q){
        neighbours[i] <- 0.0 #Where is neighbours being created?
        degree <- Get_Weight[node] #degree=node->Get_Weight();
        #Loop over all link (=neighbours)
        #l_cur=l_iter.First(node->Get_Links()) ##for the node in question, find all links and grab first one
        
        while(!l_iter.End()){ #until you get to the end of the link iterator...
          w <- Get_Weight(l_cur) #Get the weight of the current link
          
          #If node is the starting node for the current link, then n_cur becomes l_cur's ending node
          #otherwise it becomes l_cur's starting node
          ifelse(node=Get_Start(l_cur),n_cur<-Get_End(l_cur),n_cur<-Get_Start(l_cur)) 
          
          neighbours[Get_ClusterIndex(n_cur)] <- w+neighbours[Get_ClusterIndex(n_cur)]
          
          #l_cur=l_iter.Next() ##Move to the next link in the list, making it the current link
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
      #deltaEmin=0.0 ##Not needed as we already have this initialized to be zero at beginning of code
      for(spin in 1:q){ #all possible spin states
        if(spin!=old_spin){
          h <- color_field[spin]+delta-color_field[old_spin]
          deltaE <- as.numeric(neightbours[old_spin]-neighbours[spin])+(gamma*prob*h)
          if(deltaE<deltaEmin){
            spin_opt <- spin
            deltaEmin <- deltaE
          }
        }
      } #for spin
      
      #*SPIN=spin_opt
      #node=net_iter.Next(); ##Move to next node in the network iterator
      #SPIN=i_iter.Next(); ##Move to next (??)
    } #while !net_iter.End()
    
    #----------Now set all spins to new values----------
    #node=net_iter.First(net->node_list) ##From the network's node list, grab first one and set as node 
    #SPIN=i_iter.First(new_spins) ##From new spins list, grab first one and make it SPIN
    #P_SPIN=i_iter2.First(previous_spins) ##From previous spins list, grab first one and make it P_SPIN
    
    while(!net_iter.End()){
      old_spin <- Get_ClusterIndex(node)
      #new_spin=*SPIN
      
      if(new_spin!=old_spin){ #Do we really have a change?
        changes <- changes+1
        #node -> Set_ClusterIndex(new_spin) #At memory address, set the cluster index for current node to new_spin
        
        #In parallel update, there occur cyclic attractors of size two
        #This makes the program run forever
        
        if(new_spin!=P_SPIN){ #actually *P_SPIN
          cyclic <- FALSE
          #*P_SPIN=old_spin
          color_field[old_spin] <- color_field-1 #-- argument in C++
          color_field[new_spin] <- color_field+1 #++ argument in C++
          
          #Q matrix update, iteration over all neighbours
          #l_cur=l_iter.First(node->Get_Links()) ##For node, look at all its links and grab first one
          while(!l_iter.End()){
            w <- Get_Weight(l_cur)
            
            #If the node is the starting node for the current link, then n_cur becomes the ending node;
            #otherwise, n_cur becomes the starting node for the link
            ifelse(node==Get_Start(l_cur), n_cur <- Get_End(l_cur), n_cur <- Get_Start(l_cur))
            
            Qmatrix[old_spin,Get_ClusterIndex(n_cur)] <- Qmatrix[old_spin,Get_ClusterIndex(n_cur)]-w
            Qmatrix[new_spin,Get_ClusterIndex(n_cur)] <- Qmatrix[new_spin,Get_ClusterIndex(n_cur)]+w
            Qmatrix[Get_ClusterIndex(n_cur),old_spin] <- Qmatrix[Get_ClusterIndex(n_cur),old_spin]-w
            Qmatrix[Get_ClusterIndex(n_cur),new_spin] <- Qmatrix[Get_ClusterIndex(n_cur),new_spin]+w
            
            Qa[old_spin] <- Qa[old_spin]-w
            Qa[new_spin] <- Qa[new_spin]+w
            
            #l_cur=l_iter.Next() ##Move to the next link in the iterator
          }
        }
        
        #node=net_iter.Next() ##Move to the next node in the network iterator
        #SPIN=i_iter.Next() ##Move to the next (??)
        #P_SPIN=i_ier2.Next() ##Move to the next (??)
      }
    }
    
    #In case of a cyclic attractor, we want to interrupt
    if(cyclic==TRUE){
      print("Cyclic attractor!")
      acceptance <- 0.0
      
      return(0)
    }
    
    else {
      acceptance <- as.numeric(changes)/as.numeric(num_of_nodes)
      return(changes)
    }
  }
}

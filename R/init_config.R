#' Initial Configuration Assignment
#' 
#' Description of the initial configuration assignment function.  
#' 
#' From the pottsmodel_2 text file, translating the assign_initial_conf function. The
#' description in the C++ code is the following: assinging an initial random configuration
#' of spins to nodes if calledwith a negative argument or the spin used as argument when
#' called with a positive one.
#' 
#' @param spin an integer
#' 
#' @return the initial configuration of the model
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

init_config <- function(spin) {
  s < -0
  
  #DLList_Iter<NNode*> iter; ##Creating an iteration using the NNode constructor, function holding node-based info
  #DLList_Iter<NLink*> l_iter; ##Creating an iteration using the NLink constructor, function holding edge-based info
  #NNode *n_cur; ##The current node being examined
  #NLink *l_cur; ##The current link being examined
  
  sum_weight <-0
  av_k_squared <- 0
  av_k <- 0
  
  #Initialize color_field
  for (i in 0:q){
    color_field[i] = 0
  }
  
  total_degree_sum <- 0
  
  #n_cur=iter.First(net->node_list); ##the first node in the network list is the current node
  
  while (!iter.End()){ #Figure out how to code this into R, until the end of the iteration...
    ifelse(spin<0, s <- sample(1:q,1), s <- spin) #If the spin is soehow <0, randomly assign s to an integer, otherwise assign as normal
    #n_cur->Set_ClusterIndex(s); ##
    #l_cur=l_iter.First(n_cur->Get_Links()); ##From links of n_cur, first link in list is assigned as l_cur
    
    sum_weight <- 0
    
    while(!l_iter.End()){ #Figure out how to code this into R, until the end of the link iteration...
      sum_weight <- sum_weight + Get_Weight(l_cur) #weight should be one, in case we are not using it.
      #l_cur=l_iter.Next(); ##Move to the next link in the node's link list, making it the current link
      
    }
    ##We set the sum of the weights or the degreeas the weight of the node, this way we don't have to calculate it again
    #n_cur->Set_Weight(sum_weight);
    av_k_squared <- av_k_squared + (sum_weight)^2
    av_k <- av_k + sum_weight
    
    #In case we want all links to contribute equally - parameter gamma is fixed
    ifelse(operation_mode==0, color_field[s] = colorfield[s] + 1, 
           color_field[s] = color_field[s] + sum_weight)
    
    #Or in the case we want to use a weight of each link that is proportional to k_i*k_j
    total_degree_sum <- total_degree_sum + sum_weight
    #n_cur=iter.Next(); ##Move to the next node in the network list, making it the current node
  }
  av_k_squared <- av_k_squared / length(node_list)
  av_k <- av_k / length(node_list)
  
  #return net -> node_list -> Size()
}
  
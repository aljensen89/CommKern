#' Potts Model Function
#' 
#' Description of the Potts model function.  
#' 
#' From the pottsmodel_2 test file, translating the PottsModel function
#' 
#' @param network a network (potentially need more than one variable for this?)
#' @param qvalue an unsigned integer
#' @param m an integer
#' 
#' @return TBD
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export
Potts_Model <- function(network, qvalue, m){
  
  #DLList_Iter<NNode*> iter; ##Creating an iteration using the NNode constructor, function holding node-based info
  #NNode *N_cur; ##The current node being examined
  #unsigned int *i_ptr ##Other, integer-valued iterator
  
  net  <- n
  q <- qvalue #Number of communities
  operation_mode <- m
  k_max <- 0
  Qa <- rep(NA, q+1) #Needed in calculating modularity
  weights <- rep(NA, q+1) #Weights for each spin state needed in Monte Carlo process
  color_field <- rep(NA, q+1) #Bookkeeping of occupation numbers of spin states or the number of links in the community
  neighbors <- rep(NA, q+1)
  
  num_of_nodes <- length(node_list) #num_of_nodes=net->node_list->Size()
  num_of_links <- length(link_list) #num_of_links=net->link_list->Size()
  
  #n_cur=iter.First(net->node_list); ##the first node in the network list is the current node
  
  ##These lists are needed to keep track of spin states for parallel update mode
  new_spins <- vector() #new_spins=new DL_Indexed_List<unsigned int*>()
  previous_spins <- vector() #previous_spins=new DL_Indexed_List<unsigned int*>()
  
  while(!iter.End()) { #Figure out how to code this into R, until the end of the iteration...
    if(k_max < Get_Degree(n_cur)) {
      k_max <- Get_Degree(n_cur)
    }
    #i_ptr=new unsigned int;
    #*i_ptr=0;
    #new_spins->Push(i_ptr);
    #i_ptr=new unsigned int;
    #*i_ptr=0;
    #previous_spins->Push(i_ptr);
    #n_cur=iter.Next(); ##Move to the next node in the network list, making it the current node
  }
}

#' Find community from start
#' 
#' Description of the find community from start function.
#' 
#' From the pottsmodel_2 text file, translating the FindCommunityFromStart function. Here
#' we try to minimize the affinity to the rest of the network.
#' 
#' @param gamma double
#' @param prob double
#' @param nodename char #uses char *nodename
#' @param result dataframe #uses igraph)_vector_t *result
#' @param cohesion double #uses igraph_real_t *cohesion
#' @param adhesion double #uses igraph_real_t *adhesion
#' @param my_inner_links integer #uses igraph_integer_t *my_inner_links
#' @param my_outer_links integer #uses igraph_integer_t *my_outer_links
#' 
#' @return size
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

find_comm_from_start <- function(gamma,prob,nodename,result,cohesion,
                                 adhesion,my_inner_links,my_outer_links) {
  
  #DLList_Iter<NNode*> iter, iter2;
  #DLList_Iter<NLink*> l_iter;
  #DLList<NNoce*>* to_do;
  #DLList<NNoce*>* community;
  #NNode *start_node=0, *n_cur, *neighbor, *max_aff_node, *node;
  #NLink *l_cur;
  
  found <- FALSE #boolean
  add <- FALSE #boolean
  remove <- FALSE #boolean
  
  degree <- 0.0 #double
  delta_aff_add <- 0.0 #double
  delta_aff_rem <- 0.0 #double
  max_delta_aff <- 0.0 #double
  Ks <- 0.0 #double
  Kr <- 0.0 #double
  kis <- 0.0 #double
  kir <- 0.0 #double
  w <- 0.0 #double
  
  community_marker <- 5 #long
  to_do_marker <- 10 #long
  
  inner_links <- 0.0 #double
  outer_links <- 0.0 #double
  aff_r <- 0.0 #double
  aff_s <- 0.0 #double
  
  #IGRAPH_UNUSED(prob)
  
  #to_do=new DLList<NNode*>
  #community=new DLList<NNode*>
  
  #Find the node in the network
  #n_cur=iter.First(net->node_list)
  
  while(!found & !iter.End()){
    if(pracma::strcmp(n_cur->Get_Name(), nodename) == 0){
      start_node <- n_cur
      found <- TRUE
      start_node <- Set_Affinity(0.0)
      community <- Push(start_node)
      start_node <- Set_Marker(community_marker)
      Ks <- Get_Weight(start_node)
      Kr <- Get_Weight(total_degree_sum - start_node)
    }
    #n_cur=iter.Next()
  }
  
  if(!found){
    #delete to_do
    #delete community
    -1
  }
  
  #Initialize the to do list and community with the neighbors of start node
  neighbor <- iter.First(Get_Neighbors(start_node))
  while(!iter.End()){
    #Now add at the second neighbors to the to_do list
    neighbor <- iter2.First(Get_Neighbors(node))
    while(!iter2.End()){
      if(Get_Marker(neighbor) != community_marker & Get_Marker(neighbor) != to_do_marker){
        to_do <- Push(neighbor)
        neighbor <- Set_Marker(to_do_marker)
      }
      #neighbor=iter2.Next
    }
    #node=iter.Next()
  }
  
  #Repeat, as long as we are still adding nodes to the community
  add <- TRUE
  remove <- TRUE
  while(add | remove){
    #Calculate the affinity changes of all nodes for adding every node in the to_do
    #list to the community
    
    max_delta_aff <- 0.0
    max_aff_node <- NA
    add <- FALSE
    node <- iter.First(to_do)
    while(!iter.End()){
      degree <- Get_Weight(node)
      kis <- 0.0
      kir <- 0.0
      
      #For every of the neighbors, check, count the links to the community
      #l_cur=l_iter.First(node->Get_Links())
      
      while(!l_iter.End()){
        w <- Get_Weight(l_cur)
        if(node==Get_Start(l_cur)){
          n_cur <- Get_End(l_cur)
        }
        else {
          n_cur <- Get_Start(l_cur)
        }
        if(Get_Marker(n_cur) == community_marker){
          kis <- kis+w #the weight/number of links to the community
        }
        else {
          kir <- kir+w #the weight/number of links to the rest of the network
        }
        #l_cur=l_iter.Next()
      }
      
      aff_r <- kir-gamma/total_degree_sum*(Kr-degree)*degree
      aff_s <- kis-gamma/total_degree_sum*Ks_degree
      delta_aff_add <- aff_r-aff_s
      
      if(delta_aff <= max_delta_aff){
        node <- Set_Affinity(aff_s)
        max_delta_aff <- delta_aff_add
        max_aff_node <- node
        add <- TRUE
      }
    }
    
    #calculate the affinity changes for removing every single node from community
    inner_links <- 0.0
    outer_links <- 0.0
    remove <- FALSE
    #node=iter.First(community)
    while(!iter.End()){
      degree <- Get_Weight(node)
      kis <- 0.0
      kir <- 0.0
      
      #For every one of the neighbors, check, count the links to the community
      #l_cur=l_iter.First(node->Get_Links())
      
      while(!l_iter.End()){
        w <- Get_Weight(l_cur)
        
        if(node == Get_Start(l_cur)){
          n_cur <- Get_End(l_cur)
        }
        else{
          n_cur <- Get_Start(l_cur)
        }
        
        if(Get_Marker(n_cur)==community_marker){
          kis <- kis+w
          inner_links <- inner_links+w #summing all w gives twice the number of inner links/weights
        }
        else{
          kir <- kir+w
          outer_links <- outer_links+w
        }
        #l_cur=l_iter.Next()
      }
      
      aff_r <- kit-gamma/total_degree_sum*Kr*degree
      aff_s <- kis-gamma/total_degree_sum*(Ks-degree)*degree
      delta_aff_rem <- aff_s-aff_r
      node<-Set_Affinity(aff_s)
      #We should not remove the nodes we have just added
      
      if(delta_aff_rem < max_delta_aff){
        max_delta_aff <- delta_aff_rem
        max_aff_node <- node
        remove <- TRUE
        add <- FALSE
      }
      
      #node=iter.Next()
    }
    inner_links <- inner_links*0.5
    
    #Now check whether we want to remove or add a node
    if(add){
      #Add the node of maximum affinity to the community
      community <- Push(max_aff_node)
      max_aff_node <- Set_Marker(community_marker)
      
      #Delete node from to_do
      #to_do->fDelete(max_aff_node)
      
      #Update the sum of degrees in the community
      
      Ks <- Ks+Get_Weight(max_aff_node)
      Kr <- Kr-Get_Weight(max_aff_node)
      
      #Now add all neighbors of this node not already in the to_do list or in the community
      #neighbor=iter.First(max_aff_node->Get_Neighbors())
      
      while(!iter.End()){
        if(Get_Marker(neighbor) != community_marker & Get_Marker(neighbor) != to_do_marker){
          to_do <- Push(neighbor)
          neighbor <- Set_Marker(to_do_marker)
        }
        #neighbor=iter.Next()
      }
    }
    
    if(remove){
      #Remove those with negative affinities
      #community->fDelete(max_aff_node)
      max_aff_node <- Set_Marker(to_do_marker)
      
      #Update the sum of degrees in the community
      Ks <- Ks-Get_Weight(max_aff_node)
      Kr <- Kr+Get_Weight(max_aff_node)
      
      #Add the node to to_do again
      to_do <- Push(max_aff_node)
    }
    #IGRAPH_ALLOW_INTERRUPTION() /*This is not clean...*/
  }
  
  #Write the node in the community to a file
  if(cohesion){
    cohesion <- inner_links-gamma/total_degree_sum*Ks*Ks*0.5
  }
  if(adhesion){
    adhesion <- outer_links-gamma/total_degree_sum*Ks*Kr
  }
  if(my_inner_links){
    my_inner_links <- inner_links
  }
  if(my_outer_links){
    my_outer_links <- outer_links
  }
  if(result){
    node <- iter.First(community)
    igraph_vector_resize(result, 0)
    while(!iter.End()){
      #IGRAPH_CHECK(igraph_vector_push_back(result, node->Get_Index()))
      #node=iter.Next()
    }
  }
  size <- Size(community)
  size
}

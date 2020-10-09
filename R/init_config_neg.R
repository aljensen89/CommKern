#' Initial Configuration Assignment
#' 
#' Description of the initial configuration assignment negative function.  
#' 
#' From the pottsmodel_2 text file, translating the assign_initial_conf function. The
#' description in the C++ code is the following: assinging an initial random configuration
#' of spins to nodes if called with a negative argument or the spin used as argument when
#' called with a positive one.
#' 
#' @param init_spins a bollean
#' 
#' @return the initial configuration of the model
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

init_config_neg <- function(init_spins) {
 s <- 0
 
 if(init_spins){
   
   #Bookkeeping the various degrees (positive/negative) and (in/out)
   degree_pos_in <- rep(0,num_of_nodes) #Positive indegree of the nodes (or sum of weights)
   degree_neg_in <- rep(0,num_of_nodes) #Negative indegree of the nodes (or sum of weights)
   degree_pos_out <- rep(0,num_of_nodes) #Positive outdegree of the nodes (or sum of weights)
   degree_neg_out <- rep(0,num_of_nodes) #Negative outdegree of the nodes (or sum of weights)
   
   spin <- rep(0,num_of_nodes) #The spin state of each node
 }
 
 is_init <- TRUE
 
 #Bookkeeping of occupation numbers of spin states or the number of links in community
 degree_community_pos_in <- rep(0,q+1) #Positive sum of indegree for communities
 degree_community_neg_in <- rep(0,q+1) #Negative sum of indegree for communities
 degree_community_pos_out <- rep(0,q+1) #Positive sum of outdegree for communities
 degree_community_neg_out <- rep(0,q+1) #Negative sum of outdegree for communities
 
 #More bookkeeping of weights and neighbours for HeatBathLookup_neg() function
 weights <- rep(0,q+1) #The weights for changing to another spin state
 neighbours <- rep(0,q+1) #The number of neighbours (or weights) in different spin states
 csize <- rep(0,q+1) #The number of nodes in each community
 
 m_p <- 0
 m_n <- 0
 
 sum_weight_pos_in <- 0
 sum_weight_pos_out <- 0
 sum_weight_neg_in <-0
 sum_weight_neg_out <- 0
 
 for(v in 1:num_of_nodes){
   if(init_spins){s <- sample(1:q,1) spin[v] <- s} else{s <- spin[v]}
   
   n_cur <- network$vertexes$node_id[v]
   l_iter <- network$func_edges %>% 
     filter(network$func_edges$func_start_node==network$vertexes$node_id[n_cur] | 
              network$func_edges$func_end_node==network$vertexes$node_id[n_cur])
   
   for (j in 1:nrow(l_iter)){
     w <- l_iter$func_weight[j]
     if(l_iter$func_start_node[j]==n_cur){ #From this to other, so outgoing link
       if(w>0){
         sum_weight_pos_out <- sum_weight_pos_out+w #Increase positive outgoing weight
         sum_weight_pos_in <- sum_weight_pos_in+w #Increase positive incoming weight
       } else {
         sum_weight_neg_out <- sum_weight_neg_out-w #Increase negative outgoing weight
         }
     } else {
       sum_weight_neg_in <- sum_weight_neg_in -w #Increase negative incoming weight
     }
   }
   
   if (!is_directed){
     sum_weight_pos <- sum_weight_pos_in+sum_weight_pos_out
     sum_weight_pos_in <- sum_weight_pos
     sum_weight_pos_out <- sum_weight_pos
     
     sum_weight_neg <- sum_weight_neg_in+sum_weight_neg_out
     sum_weight_neg_in <- sum_weight_neg
     sum_weight_neg_out <- sum_weight_neg
   }
   
   if(init_spins){ #Set the degrees correctly
     degree_pos_in[v] <- sum_weight_pos_in
     degree_neg_in[v] <- sum_weight_neg_in
     degree_pos_out[v] <- sum_weight_pos_out
     degree_neg_out[v] <- sum_weight_neg_out
   }
   
   #Correct the community bookkeeping
   degree_community_pos_in[s] <- sum_weight_pos_in+degree_community_pos_in[s]
   degree_community_neg_in[s] <- sum_weight_neg_in+degree_community_neg_in[s]
   degree_community_pos_out[s] <- sum_weight_pos_out+degree_community_pos_out[s]
   degree_community_neg_out[s] <- sum_weight_neg_out+degree_community_neg_out[s]
   
   #Community just increased
   csize[s] <- csize[s]+1
   
   #Sum the weights (notice that the sum of indegrees equals sum of outdegrees)
   m_p <- m_p+sum_weight_pos_in
   m_n <- m_n+sum_weight_neg_in
 }
  return(network)
}

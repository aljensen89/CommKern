###Sketch of hierarchcial multimodal_spinglass algorithm###

#Questions for DG: 
# (1) Should the number of spins be constant through layers or vary? If vary, how?
# (2) Resources for parallelizing code?
hms <- function(functional_matrix,structural_matrix,network,spins,
                alpha,coolfact,gamma,parallel=FALSE){
  
  ##Initializing variables for the multimodal_spinglass function
  net <- network
  changes <- 1
  q <- spins
  num_of_nodes <- length(net$vertexes$node_id)
  best_communities <- rep(NA,num_of_nodes)
  best_hamiltonian <- NA
  mod_matrix <- compute_modularity_matrix(functional_matrix,net)
  
  num_layer<-1
  
  ##Checks on the function input values
  if (spins < 2 | spins > length(net$vertexes$node_id)) {
    stop("Invalid number of spins")
  }
  if (coolfact < 0 | coolfact >= 1) {
    stop("Invalid temperature cooling factor")
  }
  if (gamma < 0) {
    stop("Invalid gamma value")
  }
  if(alpha < 0) {
    stop("Invalid alpha value")
  }
  
  ##Finding the inital temperature for the heatbath_multimodal function
  initial_temp <- find_start_temp(gamma,alpha,1)
  temp <- initial_temp
  
  while(changes > 0 & temp > 1e-6){
    acc <- heatbath_multimodal(gamma,alpha,temp,50)
    if(acc < (1-(1/spins))*0.05){
      changes <- 0
    } else{
      changes <- 1
    }
    temp <- temp*coolfact
    net$vertexes$community <- best_communities
  }
  
  ##Checks for global stopping criteria: all nodes in same community or all nodes in their own community
  split_flag_global <- TRUE
  if(max(net$vertexes$community)==1){
    split_flag_global==FALSE
  }
  if(max(net$vertexes$community)==num_of_nodes){
    split_flag_global==FALSE
  }
  
  net_vertexes_split <- split(net$vertexes,net$vertexes$community)
  num_comm <- length(net_vertexes_split)
  net_names <- paste("sub_net",1:num_comm,sep="_")
  
  sub_net<-NA
  
  for(i in 1:length(net_names)){
    sub_net_vertex <- as.data.frame(net_vertexes_split[[i]])
    sub_func_edges <- net$func_edges %>%
      filter(func_start_node %in% sub_net_vertex$node_id & func_end_node %in% sub_net_vertex$node_id)
    
    sub_func_mat<-reshape2::acast(sub_net_1$func_edges,func_start_node~func_end_node,value.var="func_weight")
    
    sub_str_edges <- net$str_edges %>%
      filter(str_start_node %in% sub_net_vertex$node_id & str_end_node %in% sub_net_vertex$node_id)
    
    sub_str_mat<-reshape2::acast(sub_net_1$str_edges,str_start_node~str_end_node,value.var="str_weight")
    
    sub_net_vertex<-degree(sub_func_mat,sub_str_mat,sub_net_vertex)
    
    sub_net<-list(func_edges=sub_func_edges,str_edges=sub_str_edges,vertexes=sub_net_vertex)
    assign(net_names[i],sub_net)
  }
}
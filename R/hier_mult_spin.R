#' Hierarchical multimodal spinglass algorithm
#' 
#' Description of the hierarchcial multimodal spinglass algorithm function.  
#' 
#' This is the main function of the algorithm. After running checks on the input parameters, the algorithm
#' begins on the first layer of the network, finding the optimal configuration of nodes to communities using
#' the heatbath algorithm. Once the community assignments have been finalized, the set of nodes within each
#' of these communities is broken up and become their own subnetworks, on which the algorithm is applied again
#' to get further subnetwork community assignments. This continues until the maximum number of layers is reached.
#' 
#' The function returns a dataframe of nodes to communities across the layers.
#' 
#' @param input_net a network object in list form (see the matrix_to_df() function for more details)
#' @param spins an integer indicating the maximum number of spins, or communities, that can be used
#' @param alpha a double parameter balancing the use of the guidance matrix in modularity calculation
#' @param coolfact a double parameter that indicates how quickly (or slowly) to cool the heathbath algorithm, typically set to be 0.95-0.99
#' @param false_pos a double parameter that indicates the level of false positives to allow the system to make (if ground truth is known), typically set to 0.01-0.05
#' @param max_layers an integer parameter that specifies the maximum number of layers of communities within the network
#' @param parallel a boolean operator (not currently used)
#' 
#' @return comm_layers_tree a dataframe consisting of nodes and their community assignments across the layers
#'   
#' @export

hms <- function(input_net,spins,alpha,coolfact,false_pos,max_layers,parallel=FALSE){
  
  #Checks on the function input values
  if (spins < 2 | spins > length(input_net$vertexes$node_id)) {
    stop("Must provide a number of spins within [2,number of nodes in network]")
  }
  if (coolfact < 0 | coolfact >= 1) {
    stop("Must provide a temperature cooling factor within (0,1)")
  }
  if(alpha < 0) {
    stop("Must provide a strictly positive alpha value")
  }
  if (max_layers < 1){
    stop("Must provide a max number of layers greater than one")
  }
  
  #Initializing the layer counting object
  num_layer <- 0
  
  #Data frame holding the community assignments through the layers
  comm_layers_tree <- data.frame(node_id=input_net$vertexes$node_id)
  
  #Initializing variables that will stay constant through the layers
  q <- spins
  
  #Layer loop
  while(num_layer < max_layers){
    num_layer <- num_layer+1
    
    #Creating layer-specific net list object
    net_layer <- list()
    if(num_layer==1){
      net_layer[[1]] <- input_net
    } else{
      net_layer <- sub_net_layer
    }
    
    net_vertexes_split <- list()
    sub_net_layer <- list()
    layer_comms <- c()
    
    #Getting rid of singleton communities
    null_layers <- sapply(net_layer,is.null)
    
    if(sum(null_layers)>0){
      net_layer <- net_layer[-which(null_layers)]
    }
    
    for(j in seq_along(net_layer)){
     
      #Initializing variables to use within each sub network's algorithm
      net <- net_layer[[j]]
      changes <- 1
      num_of_nodes <- length(net$vertexes$node_id)
      best_communities <- rep(NA,num_of_nodes)
      best_hamiltonian <- NA
      mod_matrix <- compute_modularity_matrix(net)
      
      ##Finding the initial temperature for the heatbath_multimodal function
      initial_temp <- find_start_temp(alpha,1)
      temp <- initial_temp
      
      while(changes > 0 & temp > 1e-6){
        acc <- heatbath_multimodal(alpha,temp,50)
        if(acc < (1-(1/q))*false_pos){
          changes <- 0
        } else{
          changes <- 1
        }
        temp <- temp*coolfact
        net$vertexes$community <- best_communities
      }
      
      net_vertexes_split <- append(net_vertexes_split,split(net$vertexes,net$vertexes$community))
      sub_net_layer <- list()
      layer_comms <- c()
      
      for(i in seq_along(net_vertexes_split)){
        net_vertexes_split[[i]]$community <- i
      }
      
      for(k in seq_along(net_vertexes_split)){ #Issue with singleton communities!
        layer_comms <- rbind(layer_comms,net_vertexes_split[[k]][,c("node_id","community")])
        
        sub_net_nodes <- net_vertexes_split[[k]]$node_id
        
        #Stopping criteria: (1) check if sub-network is a singleton
        if(length(sub_net_nodes)==1){
          next
        }
        
        sub_net_funcmat <- input_net$func_matrix[rownames(input_net$func_matrix) %in% sub_net_nodes,
                                           colnames(input_net$func_matrix) %in% sub_net_nodes]
        
        sub_net_strmat <- input_net$str_matrix[rownames(input_net$str_matrix) %in% sub_net_nodes,
                                         colnames(input_net$str_matrix) %in% sub_net_nodes]
        
        sub_net_layer[[k]] <- subset_matrix_to_df(sub_net_funcmat,sub_net_strmat)
      }
    }
    layer_comms %<>% 
      arrange(node_id) %>%
      mutate(node_id=as.integer(node_id))
    
    comm_layers_tree %<>% 
      left_join(layer_comms,by="node_id")
      
    layer_name <- c(paste0("layer_",num_layer))
    names(comm_layers_tree)[num_layer+1] <- paste0("layer_",num_layer)
  }
  return(comm_layers_tree)
}
##Exponential multiplicative cooling
input_net <- sim_network
spins<-3
alpha<-0
coolfact<-0.99
false_pos<-0.01
gamma<-1
max_layers<-3

start.time <- Sys.time()

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
    initial_temp <- find_start_temp(gamma,alpha,1)
    temp <- initial_temp
    
    while(changes > 0 & temp > 1e-6){
      acc <- heatbath_multimodal(gamma,alpha,temp,50)
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

end.time <- Sys.time()
time.taken <- end.time - start.time #1.374131 minutes


##Logarithmical multiplicative cooling
input_net <- sim_network
spins<-3
alpha<-0
coolfact<-1/0.99,
false_pos<-0.01
gamma<-1
max_layers<-3

start.time <- Sys.time()

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
    initial_temp <- find_start_temp(gamma,alpha,1)
    temp <- initial_temp
    temp_cycle <- 0
    
    while(changes > 0 & temp > 1e-6){
      acc <- heatbath_multimodal(gamma,alpha,temp,50)
      if(acc < (1-(1/q))*false_pos){
        changes <- 0
      } else{
        changes <- 1
      }
      
      temp_cycle <- temp_cycle+1
      temp <- initial_temp/(1+(coolfact*log(1+temp_cycle)))
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

end.time <- Sys.time()
time.taken <- end.time - start.time #7.315658 minutes

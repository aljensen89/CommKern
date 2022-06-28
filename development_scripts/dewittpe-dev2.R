# load an object called "input_net"
rm(list = ls())
set.seed(42)
library(magrittr)
load("data/SBM_net.rda")
input_net = SBM_net
spins = 4
alpha = 0
coolfact = 0.99
false_pos = 0.01
max_layers = 1

################################################################################
compute_modularity_matrix <- function(net) { # {{{
  UseMethod("compute_modularity_matrix")
}

#' @export
compute_modularity_matrix.spinglass_net <- function(net) {
  m <- 0.5 * sum(net$func_matrix)
  d <- outer(net$vertexes$func_degree, net$vertexes$func_degree) / (2 * m)
  net$func_matrix - d
}
# }}}

################################################################################
find_start_temp <- function(alpha,ts) { # {{{
  kT <- ts
  acceptance <- 0
  
  while(acceptance < (1-(1/q))*0.95){ #want 95% acceptance
    kT <- kT*1.1
    acceptance <- heatbath_multimodal(alpha,kT,50)$acceptance
  }
  kT <- kT*1.1
  return(kT)
} #}}}

################################################################################
heatbath_multimodal <- function(alpha,temp,max_sweeps){ #{{{
  sweep <- 0
  rn <- 0
  changes <- 1
  
  current_communities <- net$vertexes$community
  current_hamiltonian <- compute_multimodal_mod(mod_matrix,net,current_communities,alpha)

  while(sweep<max_sweeps){
    sweep <- sweep+1
    rn <- -1
    
    new_communities <- current_communities
    new_hamiltonian <- current_hamiltonian
    
    #Look for a random node
    while(rn<0 | rn>num_of_nodes){
      rn <- sample(1:num_of_nodes,1)
    }
    
    node <- net$vertexes$node_id[rn]
    
    #Search optimal spin
    old_spin <- net$vertexes$community[net$vertexes$node_id==node]
    
    for(spin in 1:q){ #all possible new spins
      if(spin!=old_spin){ #except the old one
        new_communities[rn] <- spin
        new_hamiltonian <- compute_multimodal_mod(mod_matrix,net,new_communities,alpha)
        
        if (new_hamiltonian<current_hamiltonian){
          current_communities <- new_communities
          current_hamiltonian <- new_hamiltonian
          changes <- changes+1
        } else{
        #Otherwise, move to it with some probability
        probOfMoving <- exp(-(new_hamiltonian-current_hamiltonian)/temp)
        
        if(stats::runif(1,min=0,max=1)<probOfMoving){
          current_communities <- new_communities
          current_hamiltonian <- new_hamiltonian
          changes <- changes+1
        }
        }
      }
    }
  }
  
  acceptance <- changes/(max_sweeps*q) #Proportion of changes that occurred divided by total possible changes
  return(list(acceptance = acceptance, best_communities = current_communities, best_hamiltonian = current_hamiltonian))
} # }}}

################################################################################
compute_multimodal_mod_orig <- function(mod_matrix,net,communities,alpha){ # {{{
  sum <- 0
  
  for(i in 1:nrow(mod_matrix)){
    for(j in 1:ncol(mod_matrix)){
      if(i==j){
        next
      }

      if(communities[i] != communities[j]){
        next
      }
      M_ij <- mod_matrix[i,j]
      S_ij <- net$str_matrix[i,j]
      
      sum <- sum+(M_ij+(alpha*S_ij)) #We're adding instead of subtracting, and will negate later
    }
  }
  return(-1*sum) #Negate the sum, since we're adding instead of subtracting 
} 

compute_multimodal_mod <- function(mod_matrix, net, communities, alpha){
  mat <- mod_matrix + (alpha * net$str_matrix)
  diag(mat) <- 0
  rtn <- 0
  for (i in unique(communities)) {
    idx <- which(communities == i)
    rtn <- rtn + sum(mat[idx, idx]) # We're adding instead of subtracting, and will negate later
  }
  -rtn # Negate the sum, since we're adding instead of subtracting 
}
# }}}

################################################################################
subset_matrix_to_df<-function(func_matrix,str_matrix){ # {{{
  #Checking to see if both inputs are matrices
  if(!is.matrix(func_matrix) | !is.matrix(str_matrix)){
    stop("After subsetting, at least one of the inputs is no longer a matrix")
  }
  
  #Checking to see if the dimensions of the functional and structural matrices match
  if(nrow(func_matrix)!=nrow(str_matrix) | ncol(func_matrix)!=ncol(str_matrix)){
    stop("After subsetting, functional and structural matrices don't have the same dimensions")
  }
  
  #Checking to see if inputs are square matrices
  if(nrow(func_matrix)!=ncol(func_matrix) | nrow(str_matrix)!=ncol(str_matrix)){
    stop("After subsetting, at least one of the matrix inputs is not a square matrix")
  }
  
  #Functional matrix
  func_matrix2<-func_matrix
  
  ##Because symmetric matrix, replace upper triangle with something that can be filtered out
  func_matrix2[upper.tri(func_matrix2)]<-NA
  
  func_df<-reshape2::melt(func_matrix2)
  
  ##Filter out the upper matrix values, the self correlations, and value=0
  func_df<-dplyr::filter(func_df,!is.na(.data$value)) %>%
    dplyr::filter(.data$Var1 != .data$Var2) %>%
    dplyr::filter(.data$value != 0)
  
  ##Renaming the columns
  names(func_df)<-c("func_start_node","func_end_node","func_weight")
  
  #Structural matrix
  str_matrix2<-str_matrix
  
  ##Because symmetric matrix, replace upper triangle with something that can be filtered out
  str_matrix2[upper.tri(str_matrix2)]<-NA
  
  str_df<-reshape2::melt(str_matrix2)
  
  ##Filter out the upper matrix values, the self correlations, and value=0
  str_df<-dplyr::filter(str_df,!is.na(.data$value)) %>%
    dplyr::filter(.data$Var1 != .data$Var2) %>%
    dplyr::filter(.data$value != 0)
  
  ##Renaming the columns
  names(str_df)<-c("str_start_node","str_end_node","str_weight")
  
  #Creating the list object, rfid_final
  vertex_df<-data.frame(node_id=rownames(func_matrix))
  vertex_df$node_label<-NA
  vertex_df$func_degree<-NA
  vertex_df$str_degree<-NA
  vertex_df$community<-NA
  
  vertex_df<-degree(func_matrix,str_matrix,vertex_df)
  
  func_str_df<-list(func_edges=func_df,str_edges=str_df,vertexes=vertex_df,
                    func_matrix=func_matrix,str_matrix=str_matrix)
  
  return(func_str_df)
} # }}}

################################################################################
degree <- function(adj_matrix_func,adj_matrix_str,vertex_df){ # {{{
  for (i in seq(1, nrow(adj_matrix_func), by = 1)){
    vertex_df[i,3] <- colSums(adj_matrix_func)[i]
    vertex_df[i,4] <- colSums(adj_matrix_str)[i]
  }
  return(vertex_df)
} # }}}

################################################################################


####
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
      net$vertexes$community <- sample.int(spins, size = length(net$vertexes$community), replace = TRUE)
      initial_temp <- find_start_temp(alpha,1)
      temp <- initial_temp

      while(changes > 0 & temp > 1e-6){
        hb <- heatbath_multimodal(alpha,temp,50)
        acc <- hb$acceptance
        best_communities <- hb$best_communities
        best_hamiltonian <- hb$best_hamiltonian
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
      dplyr::arrange(.data$node_id) %>%
      dplyr::mutate(node_id = as.integer(.data$node_id))
    
    comm_layers_tree %<>% 
      dplyr::left_join(layer_comms, by = "node_id")
      
    layer_name <- c(paste0("layer_",num_layer))
    names(comm_layers_tree)[num_layer+1] <- paste0("layer_",num_layer)
  }
  comm_layers_tree



################################################################################

save(
     best_communities,
     best_hamiltonian,
     net,
     file = "dewittpe-dev2.RData")

stopifnot(
  identical(
  best_communities, 
   c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
   1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
   2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
   3L, 3L, 3L, 3L, 3L, 3L, 4L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 
   4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
   4L)
  )
)

stopifnot(all.equal( -360.765905284119, best_hamiltonian))

source("net2")
stopifnot(all.equal(net, net2))


################################################################################

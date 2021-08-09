#Ancillary function: after subsetting network, convert func and str matrices to DF
subset_matrix_to_df<-function(func_matrix,str_matrix){
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
  func_df<-filter(func_df,!is.na(value)) %>%
    filter(Var1 != Var2) %>%
    filter(value != 0)
  
  ##Renaming the columns
  names(func_df)<-c("func_start_node","func_end_node","func_weight")
  
  #Structural matrix
  str_matrix2<-str_matrix
  
  ##Because symmetric matrix, replace upper triangle with something that can be filtered out
  str_matrix2[upper.tri(str_matrix2)]<-NA
  
  str_df<-reshape2::melt(str_matrix2)
  
  ##Filter out the upper matrix values, the self correlations, and value=0
  str_df<-filter(str_df,!is.na(value)) %>%
    filter(Var1 != Var2) %>%
    filter(value != 0)
  
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
}

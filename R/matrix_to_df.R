#' Convert matrices to dataframe list for network
#' 
#' Description of the convert matrices to dataframe list for network function.  
#' 
#' This is an ancillary function that creates a dataframe list for the initial network. This is the form
#' of the network used for the spinglass algorithm
#'  
#' The function returns a dataframe list containing the functional matrix, structural matrix, a dataframe
#' of the functional edge weights, a dataframe of the structural edge weights, and nodal information (functional
#' degree, structural degree, community assignment, and label information)
#' 
#' @param func_mat a network object in list form (see the matrix_to_df() function for more details)
#' @param str_mat an integer indicating the maximum number of spins, or communities, that can be used
#' 
#' @return a list of dataframes for the network
#'
#' @examples
#'
#' # Using the example data SBM_net$func_matrix and SBM_net$str_mat
#' net <- matrix_to_df(SBM_net$func_mat, SBM_net$str_mat)
#' str(net)
#' identical(net, SBM_net)
#'
#' @export
#' 
matrix_to_df<-function(func_mat,str_mat){
  #Checking to see if both inputs are matrices
  if(!is.matrix(func_mat) | !is.matrix(str_mat)){
    stop("At least one of the inputs is not a matrix")
  }
  
  #Checking to see if the dimensions of the functional and structural matrices match
  if(nrow(func_mat)!=nrow(str_mat) | ncol(func_mat)!=ncol(str_mat)){
    stop("Functional and structural matrices don't have the same dimensions")
  }
  
  #Checking to see if inputs are square matrices
  if(nrow(func_mat)!=ncol(func_mat) | nrow(str_mat)!=ncol(str_mat)){
    stop("At least one of the matrix inputs is not a square matrix")
  }
  
  #Functional matrix
  func_mat2<-func_mat
  
  ##Because symmetric matrix, replace upper triangle with something that can be filtered out
  func_mat2[upper.tri(func_mat2)]<-NA
  
  func_df<-reshape2::melt(func_mat2)
  
  ##Filter out the upper matrix values, the self correlations, and value=0
  func_df<-dplyr::filter(func_df,!is.na(.data$value)) %>%
    dplyr::filter(.data$Var1 != .data$Var2) %>%
    dplyr::filter(.data$value != 0)
  
  ##Renaming the columns
  names(func_df)<-c("func_start_node","func_end_node","func_weight")
  
  #Structural matrix
  str_mat2<-str_mat
  
  ##Because symmetric matrix, replace upper triangle with something that can be filtered out
  str_mat2[upper.tri(str_mat2)]<-NA
  
  str_df<-reshape2::melt(str_mat2)
  
  ##Filter out the upper matrix values, the self correlations, and value=0
  str_df<-dplyr::filter(str_df,!is.na(.data$value)) %>%
    dplyr::filter(.data$Var1 != .data$Var2) %>%
    dplyr::filter(.data$value != 0)
  
  ##Renaming the columns
  names(str_df)<-c("str_start_node","str_end_node","str_weight")
  
  #Creating the list object, rfid_final
  vertex_df<-data.frame(node_id=seq(1:nrow(func_mat2)))
  vertex_df$node_label<-NA
  vertex_df$func_degree<-NA
  vertex_df$str_degree<-NA
  vertex_df$community<-NA
  
  vertex_df<-degree(func_mat,str_mat,vertex_df)
  
  func_str_df<-list(func_edges=func_df,str_edges=str_df,vertexes=vertex_df,
                    func_matrix=func_mat,str_matrix=str_mat)
  
  class(func_str_df) <- "hms_net"
  func_str_df
}

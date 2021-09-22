library(igraph)
library(igraphdata)
library(intergraph)
library(reshape)
library(dplyr)
library(MNS)

set.seed(80231)

#Example using rfid dataset from igraphdata for structural connectivity matrix
data("rfid") #An igraph graph with graph attributes "name" and "Citation",
             #vertex attribute "Status," and edge attribute "Time"

rfid_matrix_orig<-as.matrix(igraph::get.adjacency(rfid))
dim(rfid_matrix_orig) #75 x 75

#Example using MNS package for functional connectivity matrix
rand_net<-gen.Network(method="cohort",p=75,Nsub=1,sparsity=0.55,
                      REsize=10,REprob=0.5,REnoise=1)

func_conn<-rand_net$Networks[[1]]

#Zeroing out the diagonal and zeroing out any negative correlation - only when dealing with a matrix!
fMRI_mimic2<-function(data){
  q<-nrow(data)
  data2<-data
  for (k in 1:q){
    data2[k,k]<-0
  }
  for (i in 1:(q-1)){
    for (j in (i+1):q){
      ifelse(data2[i,j]<0,data2[i,j]<-0,data2[i,j])
      ifelse(data2[j,i]<0,data2[j,i]<-0,data2[j,i])
    }
  }
  return(data2)
}

func_conn<-fMRI_mimic2(func_conn)

##Calculating degree of each node
degree<-function(adj_matrix_func,adj_matrix_str,vertex_df){
  qf<-nrow(adj_matrix_func)
  for (i in 1:qf){
    vertex_df[i,3]<-colSums(adj_matrix_func)[i]
    vertex_df[i,4]<-colSums(adj_matrix_str)[i]
  }
  return(vertex_df)
}

#Func and struc matrix conversion to DF
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
  func_df<-filter(func_df,!is.na(value)) %>%
    filter(Var1 != Var2) %>%
    filter(value != 0)
  
  ##Renaming the columns
  names(func_df)<-c("func_start_node","func_end_node","func_weight")
  
  #Structural matrix
  str_mat2<-str_mat
  
  ##Because symmetric matrix, replace upper triangle with something that can be filtered out
  str_mat2[upper.tri(str_mat2)]<-NA
  
  str_df<-reshape2::melt(str_mat2)
  
  ##Filter out the upper matrix values, the self correlations, and value=0
  str_df<-filter(str_df,!is.na(value)) %>%
    filter(Var1 != Var2) %>%
    filter(value != 0)
  
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
  
  return(func_str_df)
}

#l_iter idea: rfid_df %>% filter(start_node==21 | end_node==21)

#####Data management for CMU dataset#####
#Libraries
library(dplyr)
library(tidyverse)
library(magrittr)
library(reshape)
library(MNS)
library(R.matlab)
library(data.table)

#Miscellaneous functions needed for fMRI data
##Zeroing out the diagonal and zeroing out any negative correlation - only when dealing with a matrix!
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

##Calculating degree of each node
degree<-function(adj_matrix_func,adj_matrix_str,vertex_df){
  qf<-nrow(adj_matrix_func)
  for (i in 1:qf){
    vertex_df[i,3]<-colSums(adj_matrix_func)[i]
    vertex_df[i,4]<-colSums(adj_matrix_str)[i]
  }
  return(vertex_df)
}

#Main function: functional and structural matrix conversion to DF
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

#Setting the SC working directory
setwd("/Users/jenseale/Dropbox/PhD_Dissertation_Work/CMU_SC_BOLD/SC")

#Bringing in the SC Matlab matrices
sc_full_files<-list.files(path=".",pattern="*.mat")
sc_full_mydata<-lapply(sc_full_files,function(X) readMat(X)$connectivity)
sc_array<-array(dim=c(length(sc_full_files),1,dim(sc_full_mydata[[1]])[1],dim(sc_full_mydata[[1]])[2]))

#Adding the SC data to the empty array
for (i in 1:nrow(sc_array)) {
  sc_array[i,1,,]<-sc_full_mydata[[i]]
}

#Grabbing the coaxid numbers for the SC matrices
sc_coaxid<-stringr::str_extract(sc_full_files, "^.{4}")

#Setting the FC working directory
setwd("/Users/jenseale/Dropbox/PhD_Dissertation_Work/CMU_SC_BOLD/")

#Bringing in the FC Matlab matrices
fc_full_files<-list.files(path=".",pattern="*.mat")
cmu_bold_files<-readMat(fc_full_files)

#Grabbing the attributes files for each study subject
##Rownames of the attributes
attr_vars<-rownames(as.data.frame(cmu_bold_files$attributes[,,1]$subject[,,1][1]))

attr_df<-data.frame()
for (k in attr_vars){
  attr_df[[k]]<-as.character()
}

##Filling in the attributes data frame
count<-1
for(i in 1:dim(cmu_bold_files$attributes[,,1]$subject)[1]){
  new_row<-t(as.data.frame(cmu_bold_files$attributes[,,1]$subject[,,1][i]))
  attr_df[count,]<-new_row
  count<-count+1
}

##Converting NaN to NA
attr_df[attr_df=="NaN"]<-NA

#Pulling the BOLD time series for each ROI and calculating cross-correlations for adjacency matrices
fc_array<-array(dim=c(length(names(cmu_bold_files$bold[,,1]$avgs[,,1])),1,
                      dim(sc_full_mydata[[1]])[1],dim(sc_full_mydata[[1]])[2]))

for (i in 1:2){
  names<-names(cmu_bold_files$bold[,,1]$avgs[,,1])
  for (j in 1:dim(sc_full_mydata[[1]])[1]){
    for (k in 1:dim(sc_full_mydata[[1]])[1]){
      fc_array[i,1,j,k]<-ccf(ts(cmu_bold_files$bold[,,1]$avgs[,,1][[names[i]]][,,1]$rois.data[j,]),
                             ts(cmu_bold_files$bold[,,1]$avgs[,,1][[names[i]]][,,1]$rois.data[k,]),
                             type=c("correlation"),plot=FALSE)$acf[,,1][24]
    }
  }
}

#Creating network object for first CMU dataset participant
func_mat <- fc_array[2,1,,]
func_mat <- func_mat[1:600,1:600] #Final 26 ROIs are cerebellar regions

func_na_rows <- which(is.na(func_mat[,1]),arr.ind=TRUE) #Which rows contain all NAs?

str_mat <- sc_array[2,1,,]
str_mat <- str_mat[1:600,1:600] #Excluding any cerebellar regions

func_mat_subset <- func_mat[-func_na_rows,-func_na_rows] #Deleting rows with no info
str_mat_subset <- str_mat[-func_na_rows,-func_na_rows] #Deleting same rows for struc matrix

func_mat_final <- fMRI_mimic2(func_mat_subset) #Deleting any non-positive correlations
rownames(func_mat_final) <- c(1:nrow(func_mat_final))
colnames(func_mat_final) <- c(1:nrow(func_mat_final))

str_mat_final <- log(str_mat_subset+1)/max(log(str_mat_subset+1)) #Applying transformation
rownames(str_mat_final) <- c(1:nrow(str_mat_final))
colnames(str_mat_final) <- c(1:nrow(str_mat_final))

CMU_network<-matrix_to_df(func_mat_final,str_mat_final) #Network structure object for first CMU participant

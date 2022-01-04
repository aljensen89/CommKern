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
sc_array <- array(dim=c(dim(sc_full_mydata[[1]])[1],dim(sc_full_mydata[[1]])[2],length(sc_full_files)))

#sc_array<-array(dim=c(length(sc_full_files),1,dim(sc_full_mydata[[1]])[1],dim(sc_full_mydata[[1]])[2]))

#Adding the SC data to the empty array
for (i in 1:dim(sc_array)[3]) {
  sc_array[,,i]<-sc_full_mydata[[i]]
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

attr_df$coaxid <- stringr::str_pad(attr_df$coaxid,pad="0",width=4)

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
fc_array<-array(dim=c(dim(sc_full_mydata[[1]])[1],dim(sc_full_mydata[[1]])[2],length(names(cmu_bold_files$bold[,,1]$avgs[,,1]))))

for (i in 1:length(names(cmu_bold_files$bold[,,1]$avgs[,,1]))){
  names<-names(cmu_bold_files$bold[,,1]$avgs[,,1])
  for (j in 1:dim(sc_full_mydata[[1]])[1]){
    for (k in 1:dim(sc_full_mydata[[1]])[1]){
      fc_array[j,k,i]<-ccf(ts(cmu_bold_files$bold[,,1]$avgs[,,1][[names[i]]][,,1]$rois.data[j,]),
                             ts(cmu_bold_files$bold[,,1]$avgs[,,1][[names[i]]][,,1]$rois.data[k,]),
                             type=c("correlation"),plot=FALSE)$acf[,,1][24]
    }
  }
}

##Finding the subjects who don't have a fMRI scan but do have a structural scan
sc_only_ids <- subset(sc_coaxid,!(sc_coaxid %in% attr_df$coaxid))
sc_only_vecloc <- match(sc_only_ids,sc_coaxid)

##Subsetting the sc_array to exclude the cerebellar ROIs
sc_array_subset<-array(dim=c(600,600,length(sc_full_files)))

for (i in 1:dim(sc_array)[3]){
  sc_array_subset[,,i] <- sc_array[1:600,1:600,i]

  rownames(sc_array_subset[,,i]) <- c(1:nrow(sc_array_subset[,,i]))
  colnames(sc_array_subset[,,i]) <- c(1:nrow(sc_array_subset[,,i]))
}

sc_array_subset <- sc_array_subset[,,-sc_only_vecloc]

##Calculating the mean sc_array
mean_sc_array <- rowMeans(sc_array_subset,dim=2,na.rm=TRUE)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_SC_Average.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(mean_sc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=0.5,limit=c(0,1),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Average Structural Adjacancy Matrix")
dev.off()

##Calculating the standard deviation of sc_array
sd_sc_array <- apply(sc_array_subset,1:2,sd)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_SC_StandDev.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(sd_sc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=0.5,limit=c(0,1),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Standard Deviation Structural Adjacancy Matrix")
dev.off()

##Calculating the coefficient of variation of sc_array
cf_sc_array <- (sd_sc_array/mean_sc_array)*100

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_SC_CoefVar.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(cf_sc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=400,limit=c(0,800),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Coefficient of Variation Structural Adjacancy Matrix")
dev.off()

##Subsetting the fc_array to exclude the cerebellar ROIs 
fc_array_subset<-array(dim=c(600,600,dim(fc_array)[3]))

for (i in 1:dim(fc_array)[3]){
  fc_array_subset[,,i] <- fc_array[1:600,1:600,i]
  
  rownames(fc_array_subset[,,i]) <- c(1:nrow(fc_array_subset[,,i]))
  colnames(fc_array_subset[,,i]) <- c(1:nrow(fc_array_subset[,,i]))
}

##Calculating the mean fc_array
mean_fc_array <- rowMeans(fc_array_subset,dim=2,na.rm=TRUE)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_FC_Average.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(mean_fc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=0.5,limit=c(0,1),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Average Functional Adjacancy Matrix")
dev.off()

##Calculating the standard deviation of fc_array
sd_fc_array <- apply(fc_array_subset,1:2,sd)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_SC_StandDev.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(sd_fc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=0.25,limit=c(0,0.5),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Standard Deviation Functional Adjacancy Matrix")
dev.off()

##Calculating the coefficient of variation of fc_array
cf_fc_array <- (sd_fc_array/mean_fc_array)*100

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_FC_CoefVar.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(cf_fc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=400,limit=c(0,800),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Coefficient of Variation Functional Adjacancy Matrix")
dev.off()

#Saving .Rdata objects
save(sc_array_subset,file="/Users/jenseale/Dropbox/PhD_Dissertation_Work/CMU_SC_BOLD/sc_array_subset.Rdata")
save(fc_array_subset,file="/Users/jenseale/Dropbox/PhD_Dissertation_Work/CMU_SC_BOLD/fc_array_subset.Rdata")

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

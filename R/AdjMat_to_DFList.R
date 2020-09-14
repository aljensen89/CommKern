library(igraph)
library(igraphdata)
library(intergraph)
library(reshape)
library(dplyr)

#Example using rfid dataset from igraphdata
data("rfid") #An igraph graph with graph attributes "name" and "Citation",
             #vertex attribute "Status," and edge attribute "Time"

rfid_matrix_orig<-as.matrix(igraph::get.adjacency(rfid))
dim(rfid_matrix_orig) #75 x 75

#Because it is a symmetrical matrix, replace upper triangle with something
#that can be filtered out
rfid_matrix<-rfid_matrix_orig
rfid_matrix[upper.tri(rfid_matrix)]<-NA

rfid_df<-melt(rfid_matrix)
dim(rfid_df) #5625 x 3

#Filter out the upper matrix values, the self correlations, and value=0
rfid_df<-filter(rfid_df, !is.na(value)) %>%
  filter(Var1 != Var2) %>%
  filter(value != 0)
dim(rfid_df) #1139 x 3

#Renaming the columns
names(rfid_df)<-c("start_node","end_node","weight")

#l_iter idea: rfid_df %>% filter(start_node==21 | end_node==21)

#Creating the list object, rfid_final
vertex_df<-data.frame(node_id=seq(1:nrow(rfid_matrix)))
vertex_df$node_label<-NA
vertex_df$degree<-NA
vertex_df$community<-NA

##Calculating degree of each node
degree<-function(adj_matrix,vertex_df){
  q<-nrow(adj_matrix)
  for (i in 1:q){
    vertex_df[i,3]<-colSums(adj_matrix)[i]
  }
  return(vertex_df)
}

vertex_df<-degree(rfid_matrix_orig,vertex_df)

rfid_final<-list(edges=rfid_df,vertexes=vertex_df)



# Libraries ----------------------------------------------------------------------------------------
library(tidyverse)
library(magrittr)

# Idea 1: data.frame to Newick format --------------------------------------------------------------
library(ape)

## Recursion function ------------------------------------------------------------------------------
traverse <- function(a,i,innerl){
  if(i < (ncol(df))){
    alevelinner <- as.character(unique(df[which(as.character(df[,i])==a),i+1]))
    desc <- NULL
    if(length(alevelinner) == 1) (newickout <- traverse(alevelinner,i+1,innerl))
    else {
      for(b in alevelinner) desc <- c(desc,traverse(b,i+1,innerl))
      il <- NULL; if(innerl==TRUE) il <- a
      (newickout <- paste("(",paste(desc,collapse=","),")",il,sep=""))
    }
  }
  else { (newickout <- a) }
}

## Data.frame to newick function -------------------------------------------------------------------
df2newick <- function(df, innerlabel=FALSE){
  alevel <- as.character(unique(df[,1]))
  newick <- NULL
  for(x in alevel) newick <- c(newick,traverse(x,1,innerlabel))
  (newick <- paste("(",paste(newick,collapse=","),");",sep=""))
}

## Example ------------------------------------------------------------------------------------------
df <- data.frame(layer_1=c('1','1','1','2','2','2'), layer_2=c('13','14','14','23','23','24'),
                 layer_3=c('135','145','146','235','236','245'))
myNewick <- df2newick(df)

mytree <- read.tree(text=myNewick, innerlabel=TRUE)
plot(mytree)

# Idea 2: creation using data.tree package
library(data.tree)
library(treemap)
library(DiagrammeR)
library(networkD3)
library(dendextend)

## Creating a tree from a data.frame ---------------------------------------------------------------
data("GNI2014")
head(GNI2014)

GNI2014$pathString <- paste("world", 
                            GNI2014$continent, 
                            GNI2014$country, 
                            sep = "/")

population <- as.Node(GNI2014)
print(population, "iso3", "population", "GNI", limit = 20)

## Another example ---------------------------------------------------------------------------------
data(acme)

#Tree
x <- ToDataFrameTree(acme, "pathString", "p", "cost")
x
xN <- as.Node(x)
print(xN, "p", "cost")

plot(as.dendrogram(xN),center=TRUE)

## Testing out dendextend --------------------------------------------------------------------------
xN %>% as.dendrogram() -> dend

# Add dots to indicate each break
dend %>% 
  dendextend::set("nodes_pch", 19)  %>% 
  dendextend::set("nodes_cex", 0.7) %>% 
  dendextend::set("nodes_col", "orange") %>% 
  plot(center=TRUE)

# Color in function of the cluster
par(mar=c(1,1,1,7))
dend %>%
  dendextend::set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  dendextend::set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  plot(horiz=TRUE, axes=FALSE, center=TRUE)

#' Write clusters
#' 
#' Description of the write clusters function.
#' 
#' From the pottsmodel_2 text file, translating the WriteClusters function. This function
#' writes the clusters to disk
#' 
#' @param modularity real
#' @param temperature real
#' @param csize vector
#' @param membership vector
#' @param kT double
#' @param gamma double
#' 
#' @return num_of_nodes
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

write_clusters <- function(modularity, temperature, csize, membership, kT, gamma){
 #NNode *n_cur, *n_cur2
 #DLList_Iter<NNode*> iter, iter2
 inner_links <- matrix(data=NA,nrow=x,ncol=x) #What does HugeArray<int> mean for dimensions?
 outer_links <- matrix(data=NA,nrow=x,ncol=x)
 nodes <- matrix(data=NA,nrow=x,ncol=x) #HugeArray defined somewhere else
 
 if(temperature){ #This temperature doesn't have a * before it, while all others do...?
   temperature <- kT #if temperature is not NULL, then do
 }
}

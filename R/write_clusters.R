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
 
 if(is.null(temperature)){
   temperature <- kT
 }
 
 if(is.null(csize) | is.null(membership) | is.null(modularity)){
    #To do: count the number of clusters
    for(spin in 1:q){
       inner_links[spin] <- 0
       outer_links[spin] <- 0
       nodes[spin] <- 0
       #n_cur=iter.First(net->node_list)
       
       while(!iter.End()){
          if(Get_ClusterIndex(n_cur)==spin){
             nodes[spin] <- nodes[spin]+1
             #n_cur2=iter.First(n_cur->Get_Neighbours())
             while(!iter2.End()){
                if(Get_ClusterIndex(n_cur2)==spin){
                   inner_links[spin] <- inner_links[spin]+1
                }
                else{
                   outer_links[spin] <- outer_links[spin]+1
                }
                #n_cur2=iter2.Next()
             }
          }
          #n_cur=iter.Next()
       }
    }
 }
    if(is.null(modularity)){
       modularity <- 0.0
       for(spin in 1:q){
          if(nodes[spin]>0){
             t1 <- inner_links[spin]/sum_weights(net)/2.0
             t2 <- (inner_links[spin]+outer_links[spin])/sum_weights(net)/2.0
             modularity <- modularity+t1
             modularity <- modularity-(gamma*t2*t2)
          }
       }
    }
    
    if(is.null(csize)){
       igraph_vector_resize(csize,0)
       for(spin in 1:q){
          if(nodes[spin]>0){
             inner_links[spin] <- inner_links[spin]/2
             N <- num_of_nodes
             n <- nodes[spin]
             lin <- inner_links[spin]
             lout <- outer_links[spin]
             a1 <- N*log(N)-n*log(n)*(N-n)*log(N-n)
             
             if(lin==(n*(n-1)*0.5+0.5) | n==1){
                a2 <- 0.0
             }
             else{
                a2 <- (n*(n-1)*0.5)*log(n*(n-1)*0.5)-
                   (n*(n-1)*0.5)-(n*(n-1)*0.5-lin)*log(n*(n-1)*0.5-lin)+
                   (n*(n-1)*0.5-lin)-lin*log(lin)+lin
             }
             
             if(lout==n*(N-n) | n==N){
                a3 <- 0.0
             }
             else{
                a3 <- (n*(N=n))*log(n*(N-n))-(n*(N-n))-
                   (n*(N-n)-lout)*log(n*(N-n)-lout)+(n*(N-n)-lout)-
                   lout*log(lout)+lout
             }
             p1 <- (lin+lout)*log(p)
             p2 <- (0.5*n(n-1)-lin+n(N-n)-lout)*log(1.0-p)
             
             #IGRAPH_CHECK(igraph_vector_push_back(csize,nodes[spin]))
          }
       }
    }
    #The elements of the cluster
    if(is.null(membership)){
       no <- -1
       #IGRAPH_CHECK(igraph_vector_resize(membership, num_of_nodes))
       for(spin in 1:q){
          if(nodes[spin]>0){
             no <- no+1
          }
          #n_cur=iter.First(net->node_list)
          while(!iter.End()){
             if(Get_ClusterIndex(n_cur)==spin){
                #VECTOR(*membership)[n_cur->Get_Index()]=no
             }
             #n_cur=iter.Next()
          }
       }
    }
    return(num_of_nodes)
}

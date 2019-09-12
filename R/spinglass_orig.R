#' Spinglass Algorithm
#' 
#' Description of the spinglass algorithm function.  
#' 
#' From the clustertool text file, translating the igraph_i_community_spinglass_orig function
#' 
#' @param graph an input graph
#' @param weights the weights of the edges
#' @param modularity
#' @param temperature
#' @param membership
#' @param csize
#' @param spins the number of spins to use (an integer constant)
#' @param parupdate a logical constant, whether to update the spins of the vertices in parallel or not
#' @param starttemp the start temperature, a real number
#' @param stoptemp the stop temperature, a real number
#' @param coolfactcooling factor for the simulated annealing
#' @param update_rule "simple" or "config" options
#' @param gamma gamma argument of algorithm, a real constant
#' 
#' @return the community membership of each vertex
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

spinglass_orig <- function(graph, weights, modularity, temperature,
                      membership, csize, spins, parupdate, starttemp,
                      stoptemp, coolfact, update_rule, gamma) {
  if (spins < 2 | spins > 500) {
    stop("Invalid number of spins")
  }
  if (coolfact < 0 | coolfact >= 1) {
    stop("Invalid cooling factor")
  }
  if (gamma < 0) {
    stop("Invalid gamma value")
  }
  if (starttemp/stoptemp < 1) {
    stop("The starttemp value should be larger in absolute value than the stoptemp")
  }
  
  #node_list<-nodes(graph)
  #link_list<-links(graph)
  #cluster_list<-initial_config?
  
  prob <- 2*(sum_weights)/length(node_list)/(length(node_list) - 1)
  
  #Random number generator - why??
  
  ifelse(stoptemp==0 & starttemp==0, zeroT = TRUE, zeroT = FALSE) 
  
  ifelse(zeroT==FALSE, kT <- FindStartTemp(gamma, prob, starttemp), kT=stoptemp)
  
  pm <- assign_initial_config(-1)
  
  runs=0
  changes=1
  
  while(changes > 0 & (kT/stoptemp > 1 | (zeroT==TRUE & runs < 150))) {
    runs = runs + 1
    if(zeroT==TRUE) {
      kT = kT*coolfact
      if(parupdate==TRUE) {
        changes <- HeatBathParallelLookup(gamma, prob, kT, 50)
      }
      else {
        acc <- HeatBathLookup(gamma, prob, kT, 50)
        ifelse(acc < (1-(1/spins) * 0.01),changes = 0, changes = 1)
      }
    }
    else {
      if(parupdate==TRUE) {
        changes <- HeatBathParallelLookupZeroTemp(gamma, prob, 50)
      }
      else {
        acc <- HeatBathLookupZeroTemp(gamma, prob, 50)
        ifelse(acc < (1-(1/spins) * 0.01),changes = 0, changes = 1)
      }
    }
  }
  pm < -WriteCLusters(modularity, temperature, csize, membership, kT, gamma)
  
  #while link_list size??
  #while node_list size??
  #while cluster_list size??

  #while the cluster list has size??
    #cl_cur<-cluster_list with first object deleted?
    #while the cluster list has size, delete first object?
    #delete cl_cur??
  
  #delete link_list
  #delete node_list
  #delete cluster_list
  
  #End the random number generator
  
  #Delete net
  #Delete pm
}


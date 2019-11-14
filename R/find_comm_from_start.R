#' Find community from start
#' 
#' Description of the find community from start function.
#' 
#' From the pottsmodel_2 text file, translating the FindCommunityFromStart function. Here
#' we try to minimize the affinity to the rest of the network.
#' 
#' @param gamma double
#' @param prob double
#' @param nodename char #uses char *nodename
#' @param result dataframe #uses igraph)_vector_t *result
#' @param cohesion double #uses igraph_real_t *cohesion
#' @param adhesion double #uses igraph_real_t *adhesion
#' @param my_inner_links integer #uses igraph_integer_t *my_inner_links
#' @param my_outer_links integer #uses igraph_integer_t *my_outer_links
#' 
#' @return size
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

find_comm_from_start <- function(gamma,prob,nodename,result,cohesion,
                                 adhesion,my_inner_links,my_outer_links) {
  
  #DLList_Iter<NNode*> iter, iter2;
  #DLList_Iter<NLink*> l_iter;
  #DLList<NNoce*>* to_do;
  #DLList<NNoce*>* community;
  #NNode *start_node=0, *n_cur, *neighbor, *max_aff_node, *node;
  #NLink *l_cur;
  
  found <- FALSE #boolean
  add <- FALSE #boolean
  remove <- FALSE #boolean
  
  degree <- 0.0 #double
  delta_aff_add <- 0.0 #double
  delta_aff_rem <- 0.0 #double
  max_delta_aff <- 0.0 #double
  Ks <- 0.0 #double
  Kr <- 0.0 #double
  kis <- 0.0 #double
  kir <- 0.0 #double
  w <- 0.0 #double
  
  
  
}

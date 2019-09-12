#' Initial Configuration Assignment
#' 
#' Description of the initial configuration assignment function.  
#' 
#' From the pottsmodel_2 text file, translating the assign_initial_conf function
#' 
#' @param spin an integer
#' 
#' @return the initial configuration of the model
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

init_config <- function(spin) {
  s < -0
  sum_weight <-0
  av_k_squared <- 0
  av_k <- 0
  
  for (i in 0:q){
    color_field[i] = 0
  }
  
  total_degree_sum <- 0
  
  n_cur <- iter[i] #also calling the node list here; using an iter.First call
  
  while (!iter[q+1]){
    ifelse(spin<0, s <- sample(1:q,1), s <- spin)
    Set_ClusterIndex(s) <- n_cur
    l_cur <- l_iter[i] #also calling the links list here; using an l_iter.First call
    sum_weight <- 0
    
    while(!l_iter[q+1]){
      sum_weight <- sum_weight + l_cur #also calling a Get_Weight variable here
      l_cur <- l_iter[i+1]
    }
    Set_Weight(sum_weight) <- n_cur
    av_k_squared <- av_k_squared + (sum_weight)^2
    av_k <- av_k + sum_weight
    
    ifelse(operation_mode==0, color_field[s] = colorfield[s] + 1, 
           color_field[s] = color_field[s] + sum_weight)
    
    total_degree_sum <- total_degree_sum + sum_weight
    n_cur <- iter[i+1]
  }
  av_k_squared <- av_k_squared / length(node_list)
  av_k <- av_k / length(node_list)
  
  #return net -> node_list -> Size()
}
  
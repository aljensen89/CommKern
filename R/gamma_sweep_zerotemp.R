#' Gamma sweep at zero temp
#' 
#' Description of the gamma sweep at zero temp function.
#' 
#' From the pottsmodel_2 text file, translating the GammaSweepZeroTemp function. This 
#' function performs a gamma sweep at zero temp
#' 
#' @param gamma_start real
#' @param gamma_stop real
#' @param prob real
#' @param steps integer
#' @param non_parallel boolean
#' @param repetitions integer
#' 
#' @return gamma
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

gamma_sweep <- function(gamma_start, gamma_stop, prob, steps, non_parallel, repetitions){
  stepsize <- 0.0
  changes <- 0.0
  gamma <- 0.0
  acc <- 0.0
  runs <- 0.0
  
  #NNode *n_cur, *n_cur2
  #DLList_iter<NNode*> iter, iter2
  
  stepsize <- (gamma_stop-gamma_start)/steps
  
  #n_cur=iter.First(net->node_list)
  
  while(!iter.End()){
    #correlation[n_cur->Get_Index()]=new HugeArray<double>()
    #n_cur2=iter2.First(net->node_list)
    while(!iter2.End()){
      #correlation[n_cur->Get_Index()]->Set(n_cur->Get_Index())=0.0
      #n_cur2=iter2.Next()
    }
    #n_cur=iter.Next()
  }
  for(n in 0:steps){
    assign_initial_config(-1)
    #initialize_Qmatrix()
    gamma <- gamma_start+stepsize*n
    
    for(i in 0:repetitions){
      changes <- 1
      assign_initial_config(-1)
      #initialize_Qmatrix()
      runs <- 0.0
      
      while (changes>0 & runs<250){
        if(!non_parallel){
          changes <- heatbath_parallel_zerotemp(gamma, prob, 1)
        }
        else{
          acc <- heatbath_zerotemp(gamma, prob, 1)
        }
        if(acc>(1-1/q*0.01)){
          changes <- 1
        }
        else{
          changes <- 0.0
        }
        runs <- runs + 1
      }
      #n_cur=iter.First(net->node_list)
      while(!iter.End()){
        if(n_cur->Get_ClusterIndex()==n_cur2->GetClusterIndex()){
          #correlation[n_cur->Get_Index()]->Set(n_cur2->Get_Index())+=0.5
          #correlation[n_cur2->Get_Index()]->Set(n_cur->Get_Index())+=0.5
        }
        #n_cur2=iter2.Next()
      }
      #n_cur=iter.Next()
    }
  }
  return(gamma)
}
#' Gamma sweep
#' 
#' Description of the gamam sweep function.
#' 
#' From the pottsmodel_2 text file, translating the GammaSweep function. This function
#' performs a gamma sweep
#' 
#' @param gamma_start real
#' @param gamma_stop real
#' @param prob real
#' @param steps integer
#' @param non_parallel boolean
#' @param repetitions integer
#' 
#' @return kT
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

gamma_sweep <- function(gamma_start, gamma_stop, prob, steps, non_parallel, repetitions){
  stepsize <- 0.0
  kT <- 0.0
  kT_start <- 0.0
  gamma <- 0.0
  acc <- 0.0
  
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
    assign_initial-config(-1)
    #initialize_Qmatrix()
    gamma <- gamma_start+stepsize*n
    kT <- 0.5
    acceptance <- 0.5
    
    while(acceptance<1.0-1.0/q*0.95){ #wollen 95% acceptance
      kT <- kT*1.1
      if(!non_parallel){
        heatbath_parallel_lookup(gamma, prob, kT, 25)
      }
      else{
        heatbath_lookup(gamma, prob, kT, 25)
      }
    }
    kT_start <- kT
    
    for(i in 0:repetitions){
      changes <- 1
      kT <- kT_start
      assign_initial_config(-1)
      #initialize_Qmatrix()
      
      while(changes>0 & kT>0.01){
        kT <- kT*0.99
        if(!non_parallel){
          changes <- heatbath_parallel_lookup(gamma, prob, kT, 50)
        }
        else{
          acc <- heatbath_lookup(gamma, prob, kT, 50)
        }
        ifelse(acc>(1.0-1.0/q*0.01),changes <- 1, changes <- 0)
      }
    }
      
      #Calculate the correlation
      #n_cur=iter.First(net->node_list)
      while(!iter.End()){
        #n_cur2=iter2.First(net->node_list)
        while(!iter2.End()){
          if(Get_ClusterIndex(n_cur)==Get_ClusterIndex(n_cur2)){
            #correlation[n_cur->Get_Index()]->Set(n_cur2->Get_Index())+=0.5
          }
          #n_cur2=iter2.Next()
        }
        #n_cur=iter.Next()
      }
  }
  return(kT)
}

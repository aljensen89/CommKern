#' Calculcate another form of initial Q matrix?
#' 
#' Description of the alternate Q matrix initialization function.  
#' 
#' From the pottsmodel_2 text file, translating the calculate_gen0 function
#' 
#' @return Q
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

calculate_gen0 <- function() { #Different from calculate_Q by a factor of gamma
  Q <- 0.0
  
  for (i in 0:q){
    Q <- Q+((Qmatrix[i,i]-(-gamma*Qa[i]^2))/(2.0*sum_weights(net)))
    if (Qa[i]<0.0 | Qmatrix[i,i]<0.0){
      #print("Negative Qa or Q[i,i]")
    }
  }
  
  Q <- Q/(2.0*sum_weights(net)) #Q is replaced by Q divided by 2 times the sum of all weights in network
  return(Q)
}
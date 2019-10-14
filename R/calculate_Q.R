#' Calculate Q
#' 
#' Description of the calculate Q function.  
#' 
#' From the pottsmodel_2 text file, translating the calculate_Q function. The documentation
#' from the C++ code is the following: this function does the actual calculation of Q from
#' the matrix. The normalization by num_of_links is done here.
#' 
#' @return Q
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export

calculate_Q <- function(){
  Q <- 0.0
  
  for (i in 0:q){
    Q <- Q+((Qmatrix[i,i]-Qa[i]^2)/(2.0*sum_weights))
    if (Qa[i]<0.0 | Qmatrix[i,i]<0.0){
      print("Negative Qa or Q[i,i]")
    }
  }
  
  Q <- Q/(2.0*sum_weights)
  
  Q
}
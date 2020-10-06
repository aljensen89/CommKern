#' pushR Function
#' 
#' Description of the pushR function.  
#' 
#' A workaround for the C++ function push in R
#' 
#' @param foo a list
#' @param bar a vector, dataframe, or list
#' 
#' @return foo
#'
#' @examples
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export
pushR <- function(foo, bar){
  foo[[length(foo)+1]] <- bar
  foo
}
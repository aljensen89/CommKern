#' popR Function
#' 
#' Description of the popR function.  
#' 
#' A workaround for the C++ function pop in R
#' 
#' @param foo a list
#' 
#' @return foo
#'
#' @examples
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export
popR <- function(foo){
  foo[[length(foo)]]
}
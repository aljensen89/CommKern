#' Example Function
#' 
#' Description of the Example Function.  
#' 
#' Details....  (can be many paragraphs)
#' 
#' @param x a numeric value
#' @param ymat a matrix
#' @param ... not currentl used
#' 
#' @return the scalar product of x by ymat divided by 2.
#'
#' @examples 
#' 
#' example_function(2, matrix(c(1, 2, 3, 4), ncol = 2))
#'   
#' @export
example_function <- function(x, ymat, ...) {
  x * ymat / 2
}

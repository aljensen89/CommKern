#' Sort pairs
#'
#' Description of the sort pairs function.
#'
#' A function to sort pairs of integers or factors and identify the pairs
#'
#' The function returns a list of six different objects:
#' levels (a list of the classes within each of the partitions a and b)
#' n_ij (a vector containing counts of nodes within all possible classification pairs from partitions a and b)
#' n_i. (a vector of the same length as pair_nb, specifying the order of classifications in pair_nb from parition a)
#' n_.j (a vector of the same length as pair_nb, specifying the order of classifications in pair_nb from parition b)
#' pair_a (a vector containing counts of nodes within each class for partition a)
#' pair_b (a vector containing counts of nodes within each class for partition b)
#'
#' @param a a vector of classifications
#' @param b a vector of classifications
#'
#' @return res a list of six objects used asthe basis to calculate many cluster evaluation metrics,
#' like NMI, ARI, and the Rand z-score
#'
sort_pairs <- function(a,b){
  #Stopping criteria
  if(anyNA(a) | anyNA(b)){
    stop("NAs are not supported")
  }
  if(((!is.vector(a) & !is.factor(a)) | is.list(a)) | ((!is.vector(b) & !is.factor(b)) | is.list(b))){
    stop("a and b must be vectors or factors but not lists")
  }
  if(length(a) != length(b)){
    stop("a and b must have the same length")
  }
  
  #Take length of one vector/factor since length(a)=length(b)
  n <- length(a)
  
  if (is.integer(a) & is.integer(b)) {
    my_levels <- list(a = unique(a), b = unique(b))
    a <- a - min(a)
    b <- b - min(b)
    
    if (!(max(a) <= n-1 & max(b) <= n-1)) {
      a <- as.integer(factor(a, levels = mylevels$a)) - 1L
      b <- as.integer(factor(b, levels = mylevels$b)) - 1L
    }
  } else if (is.factor(a) & is.factor(b)){
    my_levels <- list(a=levels(a), b=levels(b))
    a <- as.integer(a) - 1L
    b <- as.integer(b) - 1L
  } else {
    my_levels <- list(a=unique(a), b=unique(b))
    a <- as.integer(factor(a,levels=my_levels$a)) - 1L
    b <- as.integer(factor(b,levels=my_levels$b)) - 1L
  }
  
  i_order <- order(a,b,method="radix") - 1L
  out <- count_pairs(a,b,i_order)
  
  res <- list(levels=my_levels,nij=out$pair_nb,ni.=out$a_nb,n.j=out$b_nb,
              pair_a=out$pair_a,pair_b=out$pair_b)
  res
}

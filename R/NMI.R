#' Normalized mutual information (NMI)
#'
#' Description of the normalized mutual information function.
#'
#' In information theory, the mutual information (MI) of two random variables is a measure of the
#' mutual dependence between two variables, or the quantification of the 'amount of information' obtained
#' about one random variable by observing the other random variable. The normalization of the MI score scales
#' the resuts between 0 (no mutual information) and 1 (perfect correlation).
#'
#' @param a a vector of classifications; this must be a vector of characters, integers, numerics, or a factor, but not a list.
#' @param b a vector of classifications
#' @param variant a string in ('max', 'min', 'sqrt', 'sum', 'joint') that calculates different variants of the NMI.
#' The default use is 'max'.
#'
#' @return res, a scalar with the normalized mutual information (NMI).
#'
#' @export

NMI <- function(a,b,variant=c("max","min","sqrt","sum","joint")){
  variant <- match.arg(variant)
  H <- entropy(a,b)
  MI <- -H$uv + H$u + H$v

  D <- switch(variant,max=max(H$u,H$v),min=min(H$u,H$v),
              sqrt=sqrt(H$u*H$v),sum=0.5*(H$u+H$v),
              join=H$uv)
  res <- MI/D
  res
}

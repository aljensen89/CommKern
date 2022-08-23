#' Functional and Structural Matrix Plot
#'
#' Provide a graphical representation of the functional and structural matrices
#' within a \code{spinglass_net} object.
#'
#' @param x a \code{spinglass_net} object
#' @param ... additional arguments from other methods
#'
#' @return a \code{gtable} object
#'
#' @examples
#'
#' data(SBM_net)
#'
#' matrix_plot(SBM_net)
#'
#' @export
matrix_plot <- function(x, ...) {
  UseMethod("matrix_plot")
}

#' @export
matrix_plot.spinglass_net <- function(x, ...) {
  func_node_plot <-
    ggplot2::ggplot(data = reshape2::melt(x$func_matrix)) +
    ggplot2::theme_void() +
    ggplot2::aes_string(x = "Var1", y = "Var2", fill = "value") +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low = "navy",
      high = "goldenrod1",
      mid = "darkturquoise",
      midpoint = 0.5,
      limit = c(0, 1),
      space = "Lab",
      name = ""
      ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::ggtitle("Functional")

  str_node_plot <- 
    ggplot2::ggplot(data = reshape2::melt(x$str_matrix)) + 
    ggplot2::theme_void() +
    ggplot2::aes_string(x = "Var1", y = "Var2", fill = "value") +
    ggplot2::geom_tile() + 
    ggplot2::scale_fill_gradient2(
      low = "navy",
      high = "goldenrod1",
      mid = "darkturquoise",
      midpoint = 0.5,
      limit = c(0, 1),
      space = "Lab",
      name = "") +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.position = "none"
      ) +
    ggplot2::ggtitle("Structural")

   func_grob <- ggplot2::ggplotGrob(func_node_plot)
   str_grob <- ggplot2::ggplotGrob(str_node_plot)
   mxwdth <- grid::unit.pmax(func_grob$widths[2:5], str_grob$widths[2:5])
   func_grob$widths[2:5] <- mxwdth
   str_grob$widths[2:5] <- mxwdth
   gridExtra::grid.arrange(func_grob, str_grob, ncol = 2)
}


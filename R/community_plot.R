#' Communities by layer plot
#'
#' Generate a graphical representation of the communities and layers.
#'
#' This is an ancillary function that creates the plots seen in the manuscript,
#' with a heatmap-style plot on the top panel, derived from a network adjacency
#' matrix, and a community assignment plot on the bottom panel, separated by
#' layer.
#'
#' @param x a \code{spinglass_net} object created by \code{\link{hms}}
#' @param ... additional arguments from other methods
#'
#' @return a \code{gtable} object
#'
#' @seealso \code{link{hms}}
#'
#' @examples
#'
#' data(SBM_net)
#'
#'\donttest{
#' # plot with max of two layers
#' SBM_netcomm <- hms(
#'   input_net  = SBM_net,
#'   spins      = 4,
#'   alpha      = 0,
#'   coolfact   = 0.90,
#'   tol  = 0.05,
#'   max_layers = 2
#'   )
#'
#' community_plot(SBM_netcomm)
#'}
#'
#' \donttest{
#' # plot with three layers
#' # don't run automatically on CRAN; > 5 seconds
#' SBM_netcomm <- hms(
#'   input_net  = SBM_net,
#'   spins      = 4,
#'   alpha      = 0,
#'   coolfact   = 0.90,
#'   tol  = 0.05,
#'   max_layers = 3
#'   )
#'
#' community_plot(SBM_netcomm)
#' }
#'
#' @export
community_plot <- function(x, ...) {
  UseMethod("community_plot")
}

#' @export
community_plot.spinglass_hms <- function(x, ...) {
  node_data <- reshape2::melt(x$net$func_matrix)

  comm_data <- reshape2::melt(x$comm_layers_tree,
                              id.vars = "node_id",
                              variable.name = "layer",
                              value.name = "comm")

  comm_data$comm <-
    formatC(comm_data$comm, flag = 0, width = max(nchar(comm_data$comm), na.rm = TRUE))

  layers <- split(comm_data, f = comm_data$layer)

  comms <- lapply(layers, function(x) { unique(x$comm) })
  n_comms <- lapply(comms, length)

  comm_colors <-
    Map(f =
           function(n, s) {
             gp <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, s))
             gp(n)
           },
           n = n_comms,
           MoreArgs = list(s = "Paired")
    )

    comm_plot <-
      ggplot2::ggplot() +
      ggplot2::theme_minimal() +
      ggplot2::aes_string(x = "node_id", y = "layer")

    for (i in seq_along(layers)) {
      comm_plot <-
        comm_plot +
          ggplot2::geom_tile(data = layers[[i]],
                             mapping = ggplot2::aes_string(fill = "comm"),
                             color = "black") +
          ggplot2::scale_fill_manual(name = paste("layer", i, "communities"),
                                     values = comm_colors[[i]])

        if (i < length(layers)) {
          comm_plot <- comm_plot + ggnewscale::new_scale_fill()
        }
    }

    comm_plot <- comm_plot +
      ggplot2::labs(x = "node") +
      ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     plot.margin = grid::unit(c(0, 0, 0, 0.1), "in"))

    node_plot <-
      ggplot2::ggplot(data = node_data) +
      ggplot2::theme_minimal() +
      ggplot2::aes_string(x = "Var1", y = "Var2", fill = "value") +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient2(low = "navy",
                                    high = "goldenrod1",
                                    mid = "darkturquoise",
                                    midpoint = 0.5,
                                    limit = c(0, 1),
                                    space = "Lab",
                                    name = "") +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     plot.margin = grid::unit(c(0, 0, -0.2, 0.1), "in"))

    comm_plot <- comm_plot + ggplot2::theme(legend.position = "none")
    node_plot <- node_plot + ggplot2::theme(legend.position = "none")

    cp_grob <- ggplot2::ggplotGrob(comm_plot)
    np_grob <- ggplot2::ggplotGrob(node_plot)

    mxwdth <- grid::unit.pmax(cp_grob$widths[2:5], np_grob$widths[2:5])
    cp_grob$widths[2:5] <- mxwdth
    np_grob$widths[2:5] <- mxwdth

    gridExtra::grid.arrange(np_grob, cp_grob, ncol = 1)
}


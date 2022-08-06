#' Communities by layer plot
#' 
#' Description of the communities by layer plot function.  
#' 
#' This is an ancillary function that creates the plots seen in the manuscript, with a heatmap-style
#' plot on the top panel, derived from a network adjacency matrix, and a community assignment plot
#' on the bottom panel, separated by layer.
#' 
#' @return A 1x2 panel plot.
#' 
#' @param comm_data is a long dataframe with three columns: node_id, layer, and comm; this can be created from the comm_layers_tree object using the tidyr::gather() function
#' @param node_data is a melted version of the network adjacency matrix, using the reshape2::melt() function
#' 
#' @return the vertex_df dataframe to be incorporated into the network object
#'   
#' @examples 
#' 
#' data(SBM_net)
#' \dontrun{
#' SBM_netcomm <- hms(input_net=SBM_net,spins=4,alpha=0,coolfact=0.90,false_pos=0.05,max_layers=3)
#' 
#' community_plot(SBM_netcomm$comm_layers_tree,SBM_netcomm$func_matrix)
#' }
#' @export
community_plot <- function(comm_data, node_data) {
  comm_data$comm1 <- factor(paste0("c", formatC(comm_data$comm, flag = 0, width = 2)))
  comm_data$comm2 <- factor(paste0("c", formatC(comm_data$comm, flag = 0, width = 2)))
  comm_data$comm3 <- factor(paste0("c", formatC(comm_data$comm, flag = 0, width = 2)))
  
  layers <-
    list(
           l1 = subset(comm_data, comm_data$layer == "layer_1")
         , l2 = subset(comm_data, comm_data$layer == "layer_2")
         , l3 = subset(comm_data, comm_data$layer == "layer_3")
    )
  
  comms <- lapply(layers, function(x) {unique(x$comm2)})
  n_comms <- lapply(comms, length)
  comm_colors <- mapply(function(n, s) { gp <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, s)); gp(n) },
                        n_comms, s = list("Paired", "Paired", "Paired"))
  
  comm_plot <-
    ggplot2::ggplot() +
    ggplot2::theme_minimal() +
    ggplot2::aes_string(x = "node_id", y = "layer") +
    ggplot2::geom_tile(data = layers[[1]], mapping = ggplot2::aes_string(fill = "comm1"), color = "black") +
    ggplot2::scale_fill_manual(name = "layer 1 communities", values = comm_colors[[1]]) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_tile(data = layers[[2]], mapping = ggplot2::aes_string(fill = "comm2"), color = "black") +
    ggplot2::scale_fill_manual(name = "layer 2 communities", values = comm_colors[[2]]) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_tile(data = layers[[3]], mapping = ggplot2::aes_string(fill = "comm3"), color = "black") +
    ggplot2::scale_fill_manual(name = "layer 3 communities", values = comm_colors[[3]]) +
    ggplot2::labs(x = "node")  +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0.0, 0, 0, 0.10), "in"))
  
  node_plot <-
    ggplot2::ggplot(data = node_data) +
    ggplot2::theme_minimal() +
    ggplot2::aes_string(x = "Var1", y = "Var2", fill = "value") +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low  = "navy",
      high = "goldenrod1",
      mid  = "darkturquoise", 
      midpoint = 0.5,
      limit = c(0, 1),
      space = "Lab", 
      name="") +
    ggplot2::theme(axis.text  = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0, 0, -0.20, 0.10), "in"))
  
  comm_plot <- qwraps2::ggplot2_extract_legend(comm_plot)
  node_plot <- qwraps2::ggplot2_extract_legend(node_plot)
  
  cp_grob <- ggplot2::ggplotGrob(comm_plot$plot)
  np_grob <- ggplot2::ggplotGrob(node_plot$plot)
  mxwdth  <- grid::unit.pmax(cp_grob$widths[2:5], np_grob$widths[2:5])
  cp_grob$widths[2:5] <- mxwdth
  np_grob$widths[2:5] <- mxwdth
  
  gridExtra::grid.arrange(np_grob, cp_grob, ncol = 1)
}


#' Convert matrices to dataframe list for network
#'
#' Description of the convert matrices to data frame list for network function.
#'
#' This is an ancillary function that creates a data frame list for the initial
#' network. This is the form of the network used for the spinglass algorithm
#'
#' @param func_mat a square, symmetric matrix to be used as the main input
#' for the \code{\link{hms}} algorithm. For brain connectivity, this will be
#' a representation of functional (e.g., BOLD) connectivity.
#' @param str_mat a square, symmetric matrix to be used as the guidance
#' input for the \code{\link{hms}} algorithm. For brain connectivity, this 
#' will be a representation of structural (e.g., white matter) connectivity.
#'
#' @return A list containing the functional matrix, structural matrix, a data
#' frame of the functional edge weights, a data frame of the structural edge
#' weights, and nodal information (functional degree, structural degree,
#' community assignment, and label information)
#'
#' @examples
#'
#' # Using the example data SBM_net$func_matrix and SBM_net$str_mat
#' net <- matrix_to_df(SBM_net$func_mat, SBM_net$str_mat)
#' str(net)
#' identical(net, SBM_net)
#'
#' @export
#'
matrix_to_df <- function(func_mat, str_mat) {
    # Checking to see if both inputs are matrices
    if (!is.matrix(func_mat) | !is.matrix(str_mat)) {
        stop("At least one of the inputs is not a matrix")
    }

    # Checking to see if the dimensions of the functional and structural
    # matrices match
    if (nrow(func_mat) != nrow(str_mat) | ncol(func_mat) != ncol(str_mat)) {
        stop("Functional and structural matrices don't have the same dimensions")
    }

    # Checking to see if inputs are square matrices
    if (nrow(func_mat) != ncol(func_mat) | nrow(str_mat) != ncol(str_mat)) {
        stop("At least one of the matrix inputs is not a square matrix")
    }

    # Functional matrix
    func_mat2 <- func_mat

    ## Because symmetric matrix, replace upper triangle with something that can
    ## be filtered out
    func_mat2[upper.tri(func_mat2)] <- NA

    func_df <- reshape2::melt(func_mat2)

    ## Filter out the upper matrix values, the self correlations, and value=0
    func_df <- subset(func_df, !is.na(func_df$value))
    func_df <- subset(func_df, func_df$Var1 != func_df$Var2)
    func_df <- subset(func_df, func_df$value != 0)
    rownames(func_df) <- NULL

    ## Renaming the columns
    names(func_df) <- c("func_start_node", "func_end_node", "func_weight")

    # Structural matrix
    str_mat2 <- str_mat

    ## Because symmetric matrix, replace upper triangle with something that can
    ## be filtered out
    str_mat2[upper.tri(str_mat2)] <- NA

    str_df <- reshape2::melt(str_mat2)

    ## Filter out the upper matrix values, the self correlations, and value=0
    str_df <- subset(str_df, !is.na(str_df$value))
    str_df <- subset(str_df, str_df$Var1 != str_df$Var2)
    str_df <- subset(str_df, str_df$value != 0)
    rownames(str_df) <- NULL

    ## Renaming the columns
    names(str_df) <- c("str_start_node", "str_end_node", "str_weight")

    # Creating the list object, rfid_final
    vertex_df <- data.frame(node_id = seq(1:nrow(func_mat2)))
    vertex_df$node_label <- NA
    vertex_df$func_degree <- NA
    vertex_df$str_degree <- NA
    vertex_df$community <- NA

    vertex_df <- degree(func_mat, str_mat, vertex_df)

    func_str_df <- list(func_edges = func_df, str_edges = str_df, vertexes = vertex_df,
        func_matrix = func_mat, str_matrix = str_mat)

    class(func_str_df) <- "spinglass_net"
    func_str_df
}

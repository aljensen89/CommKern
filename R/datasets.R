#' Simulated functional and structural connectivity with nested 
#' hierarchical community structure
#'
#' A dataset containing multimodal network information simulated to emulate 
#' functional and structural brain connectivity data with a nested hierarchical
#' community structure. This dataset is a list containing five components in a
#' format used as an input to the \code{\link{hms}} function. The components,
#' and their associated variables, are as follows:
#'
#' @format A list containing five components:
#' \describe{
#'   \item{func_edges}{a dataframe containing 1233 rows and 3 columns: func_start_node,
#'     func_end_node, and func_weight. This dataframe describes the pairwise functional
#'     edge weights between nodes.}
#'   \item{str_edges}{a dataframe containing 453 rows and 3 columns: str_start_node,
#'     str_end_node, and str_weight. This dataframe describes the pairwise structural
#'     edge weights between nodes. There are fewer rows to this dataframe than func_edges
#'     as structural connectivity tends to be sparser than functional connectivity.}
#'   \item{vertexes}{a dataframe containing 80 rows and 5 columns: node_id, node_label,
#'     func_degree, str_degree, and community. The degree of a node is the sum of all edge
#'      weights connected to the node. In this simulated network, node_label is left as NA
#'      but, for other networks, a specific label may be used to denote additional information
#'      about the node. The community variable is left blank but is used by the \code{\link{hms}}
#'      algorithm.}
#'   \item{func_matrix}{an 80 x 80 matrix in the style of a network adjacency matrix. It
#'      contains the same information as func_edges, just in a wide, rather than long, format.}
#'   \item{str_matrix}{an 80 x 80 matrix in the style of a network adjacency matrix. It
#'      contains the same information as str_edges, just in a wide, rather than long, format.}
#' }
"SBM_net"


#' Simulated demographics dataset modeled of a subset of the preprocessed 
#' ABIDE database
#'
#' A dataset of demographics generated based on summary statistics for a subset
#' of the ABIDE preprocessed database (http://preprocessed-connectomes-project.org/abide/).
#' The variables are as follows:
#'
#' @format A dataframe with 49 rows and 8 columns:
#' \describe{
#'   \item{id}{a generic ID, an integer value}
#'   \item{dx_group}{diagnostic group (0=control, 1=Autism Spectrum Disorder (ASD)}
#'   \item{sex}{subject sex (0=male, 1=female)}
#'   \item{age}{subject age in years}
#'   \item{handedness}{subject handedness category, a factor with three level
#'     (0=right, 1=left, 2=ambidextrous)}
#'   \item{fullscale_IQ}{fullscale IQ score, simulated as if administered from the
#'     Wechsler Abbreviated Scales of Intelligence (WASI), an integer value in (50,160)}
#'   \item{verbal_IQ}{verbal IQ component, simulated as if administered from the
#'     Wechsler Abbreviated Scales of Intelligence (WASI), an integer value in (55,160)}
#'   \item{nonverbal_IQ}{nonverbal IQ component, simulated as if administered from the
#'     Wechsler Abbreviated Scales of Intelligence (WASI), an integer value in (53,160)}
#' }
"simasd_covars"


#' Simulated Hamiltonian values from HMS algorithm
#'
#' A dataset of Hamiltonian values from simulated group-level networks with
#' community structure. This dataset is complementary to the simasd_covars
#' dataset, which contains the demographic information related to this dataset.
#' For more information on how these group-level networks were simulated, please
#' refer to the example script titled "beta_simulation_data.set.R".
#' The variables are as follows:
#'
#' @format A dataframe with 49 rows and 2 columns:
#' \describe{
#'   \item{id}{a generic ID, corresponding to the id variable in simasd_covars}
#'   \item{hamil}{Hamiltonian value calculated from running the simulated network through the
#'     HMS algorithm, a numeric value}
#' }
"simasd_hamil_df"


#' Simulated partitions of nodes to communities from HMS algorithm
#'
#' A dataset of partitions of nodes to communities from simulated group-level
#' networks with community structures. This dataset is complementary to the
#' simasd_covars dataset, which contains the demographic information related
#' to this dataset. For more information on how these group-level networks 
#' were simulated, please refer to the example script titled "beta_simulation_data.set.R".
#' The variables are as follows:
#'
#' @format A dataframe with 80 rows and 49 columns, where rows correspond to
#' nodes within the simulated networks and columns correspond to the subject ID.
"simasd_comm_df"


#' Simulated Array
#'
#' A dataset containing an array of simulated adjacency matrices. The dimensions of
#' each matrix is 80 x 80, for a total of 49 simulated networks. This simulated array
#' is the basis of the simasd_hamil_df and simasd_comm_df datasets and is 
#' complementary to the simasd_covars dataframe.
#' 
#' @format An array of dimensions 49 x 80 x 80, denoting matrices for 49 simulated
#' networks, with each network's matrix corresponding to an adjacency matrix for 
#' an 80 node network.
"simasd_array"

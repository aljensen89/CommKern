library(CommKern)

# Does the trace of the degree matrix match column and row sums?
vertex_df_ex <- data.frame(node_id=seq(1:80),node_label=NA,
                           func_degree=NA,str_degree=NA,community=NA)

func_mat_ex <- SBM_net$func_matrix
str_mat_ex <- SBM_net$str_matrix

vertex_df_ex <- CommKern:::degree(func_mat_ex,str_mat_ex,vertex_df_ex)

stopifnot(all(vertex_df_ex$func_degree==colSums(func_mat_ex)))
stopifnot(all(vertex_df_ex$func_degree==rowSums(func_mat_ex)))
stopifnot(all(vertex_df_ex$str_degree==colSums(str_mat_ex)))
stopifnot(all(vertex_df_ex$str_degree==colSums(str_mat_ex)))




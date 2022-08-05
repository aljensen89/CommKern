library(CommKern)

################################################################################
# Test that the provided SBM_net data set regenerates itself
net <- matrix_to_df(func_mat = SBM_net$func_mat, str_mat = SBM_net$str_mat)
stopifnot(identical(net, SBM_net))

################################################################################
#                                 End of File                                  #
################################################################################


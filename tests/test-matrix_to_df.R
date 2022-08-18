library(CommKern)


# Test that the provided SBM_net data set regenerates itself
net <- matrix_to_df(func_mat = SBM_net$func_mat, str_mat = SBM_net$str_mat)
stopifnot(identical(net, SBM_net))

# Verify expected error handles - all matrices
func_mat_ex <- matrix(runif(25,min=0,max=1),nrow=5)
str_mat_ex <- 1

rtn <- tryCatch(matrix_to_df(func_mat_ex,str_mat_ex),
                error = function(e) {e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "At least one of the inputs is not a matrix")

# Verify expected error handles - func and str matrices have same dims
func_mat_ex <- matrix(runif(25,min=0,max=1),nrow=5)
str_mat_ex <- matrix(runif(16,min=0,max=1),nrow=4)

rtn <- tryCatch(matrix_to_df(func_mat_ex,str_mat_ex),
                error = function(e) {e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "Functional and structural matrices don't have the same dimensions")

# Verify expected error handles - all matrices are square
func_mat_ex <- matrix(runif(24,min=0,max=1),nrow=4)
str_mat_ex <- matrix(runif(24,min=0,max=1),nrow=4)

rtn <- tryCatch(matrix_to_df(func_mat_ex,str_mat_ex),
                error = function(e) {e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "At least one of the matrix inputs is not a square matrix")



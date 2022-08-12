library(CommKern)

# Verify expected error handles - grid granularity
y <- rnorm(10,mean=100,sd=10)
covs <- data.frame(a=rnorm(10,mean=100,sd=5),b=sample(c(0,1),10,replace=TRUE))

ex_mat <- matrix(runif(100,0,1),10)
ex_mat[lower.tri(ex_mat)] <- ex_mat[upper.tri(ex_mat)]

rtn <- tryCatch(CommKern::score_cont_semiparam(outcome=y,covars=covs,
                                               dist_mat=ex_mat,grid_gran=1),
                error = function(e) {e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "Need to specify a grid search length of at least 2")

# Verify expected error handles - outcome vector
ex_mat <- matrix(runif(100,0,1),10)
ex_mat[lower.tri(ex_mat)] <- ex_mat[upper.tri(ex_mat)]

covs <- data.frame(a=rnorm(10,mean=100,sd=5),b=sample(c(0,1),10,replace=TRUE))
y <- as.matrix(rnorm(10,mean=100,sd=10))

rtn <- tryCatch(CommKern::score_cont_semiparam(outcome=y,covars=covs,
                                               dist_mat=ex_mat,grid_gran=5000),
                error = function(e) {e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "Outcome must be specified as a vector")

# Verify expected error handles - numeric outcome
ex_mat <- matrix(runif(100,0,1),10)
ex_mat[lower.tri(ex_mat)] <- ex_mat[upper.tri(ex_mat)]

covs <- data.frame(a=rnorm(10,mean=100,sd=5),b=sample(c(0,1),10,replace=TRUE))
y <- as.list(rnorm(10,mean=100,sd=10))

rtn <- tryCatch(CommKern::score_cont_semiparam(outcome=y,covars=covs,
                                               dist_mat=ex_mat,grid_gran=5000),
                error = function(e) {e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "Outcome vector must be numeric")

# Verify expected error handles - square distance matrix
y <- rnorm(10,mean=100,sd=10)
covs <- data.frame(a=rnorm(10,mean=100,sd=5),b=sample(c(0,1),10,replace=TRUE))

ex_mat <- matrix(runif(90,0,1),10)

rtn <- tryCatch(CommKern::score_cont_semiparam(outcome=y,covars=covs,
                                               dist_mat=ex_mat,grid_gran=5000),
                error = function(e) {e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "The distance matrix must be a square matrix")

# Verify expected error handles - distance matrix and outcome dimensions
y <- rnorm(15,mean=100,sd=10)
covs <- data.frame(a=rnorm(10,mean=100,sd=5),b=sample(c(0,1),10,replace=TRUE))

ex_mat <- matrix(runif(100,0,1),10)
ex_mat[lower.tri(ex_mat)] <- ex_mat[upper.tri(ex_mat)]

rtn <- tryCatch(CommKern::score_cont_semiparam(outcome=y,covars=covs,
                                               dist_mat=ex_mat,grid_gran=5000),
                error = function(e) {e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "The number of rows in the distance matrix must be equal to the length of the outcome vector")

# Verify expected error handles - distance matrix and covariates dimensions
y <- rnorm(10,mean=100,sd=10)
covs <- data.frame(a=rnorm(12,mean=100,sd=5),b=sample(c(0,1),12,replace=TRUE))

ex_mat <- matrix(runif(100,0,1),10)
ex_mat[lower.tri(ex_mat)] <- ex_mat[upper.tri(ex_mat)]

rtn <- tryCatch(CommKern::score_cont_semiparam(outcome=y,covars=covs,
                                               dist_mat=ex_mat,grid_gran=5000),
                error = function(e) {e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "The number of rows in the distance matrix must be equal to the number of rows of the covariate dataframe")


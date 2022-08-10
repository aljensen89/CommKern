library(CommKern)

# Testing that the kernel function produces the same result as a a hard-coded kernel
test_mat <- matrix(data=runif(16,0,1),nrow=4,ncol=4)
test_rho <- 1.25

kernel_rtn <- kernel(test_mat,test_rho)

stopifnot(all.equal(
            kernel_rtn,exp(-(test_mat^2)/test_rho)
          )
        )

# More advanced check: does the kernel produce a positive semi-definite matrix?
sym_mat <- matrix(1:25,5)
sym_mat[lower.tri(sym_mat)] = t(sym_mat)[lower.tri(sym_mat)]

sym_rho <- 1.105

kernel_rtn <- kernel(sym_mat,sym_rho)

stopifnot(matrixcalc::is.positive.semi.definite(kernel_rtn)==TRUE)


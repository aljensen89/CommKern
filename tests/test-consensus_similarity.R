library(CommKern)

# Test to see if the output is the same length as the inputs
w <- c(1,1,1,1,2,3,1,2,3,3)
x <- c(2,2,2,1,3,3,3,1,1,1)
y <- c(1,1,1,1,1,3,1,2,2,3)
z <- c(1,2,3,1,2,3,1,2,3,1)

comm_mat <- cbind(w,x,y,z)

stopifnot(length(CommKern::consensus_similarity(comm_mat))==nrow(comm_mat))

# Test to see if the output is one of the inputs
w <- c(1,1,1,1,2,3,1,2,3,3)
x <- c(2,2,2,1,3,3,3,1,1,1)
y <- c(1,1,1,1,1,3,1,2,2,3)
z <- c(1,2,3,1,2,3,1,2,3,1)

comm_mat <- cbind(w,x,y,z)

stopifnot(CommKern::consensus_similarity(comm_mat) %in% comm_mat)

# Verify expected error handles - no NAs
w <- c(1,1,1,NA,2,3,1,2,3,3)
x <- c(2,2,2,1,3,3,3,1,1,1)
y <- c(1,1,1,1,1,3,1,2,2,3)
z <- c(1,2,3,1,2,3,1,2,3,1)

comm_mat <- cbind(w,x,y,z)

rtn <- tryCatch(CommKern::consensus_similarity(comm_mat),error = function(e) {e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "NAs are not supported")


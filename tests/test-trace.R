library(CommKern)

# Test that the trace function works for a square matrix
x <- matrix(1:9, ncol = 3)
stopifnot(CommKern:::tr(x) == sum(diag(x)))

# test that an error is thrown if a non square matrix is used.
x <- matrix(1:12, ncol = 3)
trc <- tryCatch(CommKern:::tr(x), error = function(e) {e})
stopifnot(inherits(trc, "simpleError"))
stopifnot(trc$message == "nrow(x) == ncol(x) is not TRUE")

# error if x is not an matrix
x <- 1:9
trc <- tryCatch(CommKern:::tr(x), error = function(e) {e})
stopifnot(inherits(trc, "simpleError"))
stopifnot(trc$message == "is.matrix(x) is not TRUE")


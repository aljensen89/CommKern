library(CommKern)


# check for square matrix
x <- 1:12
rtn <- tryCatch(CommKern:::up_low(x), error = function(e){e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "is.matrix(dist_mat) is not TRUE")

x <- matrix(x, nrow = 3)
rtn <- tryCatch(CommKern:::up_low(x), error = function(e){e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "nrow(dist_mat) == ncol(dist_mat) is not TRUE")

# check for integer inputs
x <- matrix(as.integer(1:9), ncol = 3)
rtn <- CommKern:::up_low(x)
stopifnot(identical(x, structure(1:9, dim = c(3L, 3L))))

# check for numeric inputs
x <- structure(c(-0.7, 1.2, 0.1, -0.7, 0.5, -0.9, 0.5, 1.8, 0.3, 0,
              -2.1, -0.6, -0.8, 0.4, -0.4, -2.3), dim = c(4L, 4L))
rtn <- CommKern:::up_low(x)
stopifnot(all.equal(rtn,
                    structure(c(0, 7.1, 6.56, 4.75, 7.1, 0, 8.22, 18.55, 6.56, 8.22,
                                0, 8.19, 4.75, 18.55, 8.19, 0), dim = c(4L, 4L))
                    ))

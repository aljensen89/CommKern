library(CommKern)

# -----------------------------------------------------------------------------------
# Verify the return from entropy is expected for a simple case

# Numeric inputs
x <- c(3,3,3,2,1,3,2,1,2,1)
y <- c(1,1,2,3,2,3,1,2,3,3)

numeric_rtn <- CommKern:::entropy(x,y)

stopifnot(all.equal(
          numeric_rtn,
          list(uv = 1.886697,
               u  = 1.0889,
               v  = 1.0889,
               sort_pairs = list(levels = (list(a=c(3,2,1),b=c(1,2,3))),
                                      nij    = c(2,1,1,1,2,2,1),
                                      ni.    = c(4,3,3),
                                      n.j    = c(3,3,4),
                                      pair_a = c(0,0,0,1,1,2,2),
                                      pair_b = c(0,1,2,0,2,1,2))),
          tolerance=0.00001)
        )

# Character inputs
x <- c("A", "A", "A", "A", "B", "B", "C", "A", "C", "C")
y <- c("C", "B", "B", "A", "A", "C", "C", "C", "B", "B")

char_rtn <- CommKern:::entropy(x,y)

stopifnot(all.equal(
  char_rtn,
  list(uv = 1.886697,
       u  = 1.029653,
       v  = 1.05492,
       sort_pairs = list(levels = (list(a=c("A", "B", "C"),b=c("C", "B", "A"))),
                         nij    = c(2,2,1,1,1,1,2),
                         ni.    = c(5,2,3),
                         n.j    = c(4,4,2),
                         pair_a = c(0,0,0,1,1,2,2),
                         pair_b = c(0,1,2,0,2,0,1))),
  tolerance=0.00001)
)

# Verify expected error handles - should be coming from sort_pairs function
x <- c(3,3,3,2,1,3,2,1,2,1)
y <- c(1,1,2,3,2,3,1,2,3,NA)
err_rtn <- tryCatch(CommKern:::entropy(x, y), error = function(e) {e})
stopifnot(inherits(err_rtn, "simpleError"))
stopifnot(err_rtn$message == "NAs are not supported")

x <- c(3,3,3,2,1,3,2,1,2,1)
y <- c(1,1,2,3,2,3,1,2,3)
err_rtn <- tryCatch(CommKern:::entropy(x, y), error = function(e) {e})
stopifnot(inherits(err_rtn, "simpleError"))
stopifnot(err_rtn$message == "a and b must have the same length")

x <- c(3,3,3,2,1,3,2,1,2,1)
y <- list(1,1,2,3,2,3,1,2,3,3)
err_rtn <- tryCatch(CommKern:::entropy(x, y), error = function(e) {e})
stopifnot(inherits(err_rtn, "simpleError"))
stopifnot(err_rtn$message == "a and b must be vectors or factors but not lists")

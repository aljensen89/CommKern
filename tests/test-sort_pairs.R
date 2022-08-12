library(CommKern)

################################################################################
# verify the of the return from sort_pairs is as expected for a simple case

# numeric inputs
x <- 1:5
y <- c(1, 1, 2, 2, 5)

numeric_rtn <- CommKern:::sort_pairs(x, y)

stopifnot(identical(
            numeric_rtn,
            list(levels = list(a = 1:5, b = c(1, 2, 5)),
                 nij    = c(1, 1, 1, 1, 1),
                 ni.    = c(1, 1, 1, 1, 1),
                 n.j    = c(2, 2, 1),
                 pair_a = c(0, 1, 2, 3, 4),
                 pair_b = c(0, 0, 1, 1, 2))
           )
         )

# integer inputs, same "value" as for the numeric_rtn but should have a differnt
# return
x <- as.integer(x)
y <- as.integer(y)
integer_rtn <- CommKern:::sort_pairs(x, y)

stopifnot(!identical(numeric_rtn, integer_rtn))
stopifnot(identical(
            integer_rtn,
            list(levels = list(a = 1:5, b = c(1L, 2L, 5L)),
                 nij    = c(1, 1, 1, 1, 1),
                 ni.    = c(1, 1, 1, 1, 1),
                 n.j    = c(2, 2, 1),
                 pair_a = c(0, 1, 2, 3, 4),
                 pair_b = c(0, 0, 1, 1, 2))
            )
           )

# character inputs
x <- c("A", "B", "b")
y <- c("B", "B", "D")
stopifnot(
  identical(
    CommKern:::sort_pairs(x, y),
    list(levels = list(a = c("A", "B", "b"), b = c("B", "D")), nij = c(1, 1, 1),
         ni. = c(1, 1, 1), n.j = c(2, 1), pair_a = c(0, 1, 2), pair_b = c(0, 0, 1))
    )
  )

# factor
x <- factor(x, levels = c("A", "B", "b")) # b comes before B if not specified
y <- factor(y)
stopifnot(
  identical(
    CommKern:::sort_pairs(x, y),
    list(levels = list(a = c("A", "B", "b"), b = c("B", "D")), nij = c(1, 1, 1),
         ni. = c(1, 1, 1), n.j = c(2, 1), pair_a = c(0, 1, 2), pair_b = c(0, 0, 1))
    )
  )

################################################################################
# Verify expected error handels
x <- c(x, NA)
rtn <- tryCatch(CommKern:::sort_pairs(x, y), error = function(e) {e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "NAs are not supported")

x <- 1:6
rtn <- tryCatch(CommKern:::sort_pairs(x, y), error = function(e) {e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "a and b must have the same length")

x <- list(1:6)
rtn <- tryCatch(CommKern:::sort_pairs(x, y), error = function(e) {e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "a and b must be vectors or factors but not lists")



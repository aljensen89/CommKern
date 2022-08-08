library(CommKern)

# verify the names of the return from sort_pairs

x <- 1:5
y <- c(1, 1, 2, 2, 5)

rtn <- CommKern:::sort_pairs(x, y)

stopifnot(
          identical(names(rtn),
                    c("levels", "nij", "ni.", "n.j", "pair_a", "pair_b")
                    )
          )

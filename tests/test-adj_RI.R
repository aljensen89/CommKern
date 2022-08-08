library(CommKern)

x <- c(1L, 3L, 1L, 2L, 3L, 3L, 3L, 2L, 1L, 2L, 1L, 2L)
y <- c(1L, 1L, 2L, 3L, 2L, 1L, 3L, 1L, 2L, 3L, 3L, 2L)

stopifnot(all.equal(adj_RI(x, y), -0.145833333333333))


library(CommKern)

# ------------------------------------------------------------------------------
# Verify the return from count_pairs is as expected for a simple case

# Numeric inputs
x <- c(0,0,0,1,2,0,1,2,1,2)
y <- c(0,0,1,2,1,2,0,1,2,2)
i_order <- c(0,1,2,5,6,3,8,4,7,9)

numeric_rtn <- CommKern:::count_pairs(x,y,i_order)

stopifnot(identical(
          numeric_rtn,
          list(pair_nb = c(2,1,1,1,2,2,1),
               pair_a  = c(0,0,0,1,1,2,2),
               pair_b  = c(0,1,2,0,2,1,2),
               a_nb    = c(4,3,3),
               b_nb    = c(3,3,4))
          )
        )
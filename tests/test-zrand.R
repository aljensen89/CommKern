library(CommKern)

# Numeric test no.1
x <- c(1,1,1,1,2,3,1,2,2,3)
y <- c(3,3,3,3,1,1,2,1,2,2)

stopifnot(all.equal(CommKern:::zrand(x,y),1.875521,tolerance=0.00001))

# Numeric test no.2
x <- c(1,3,1,2,3,3,3,2,1,2,1,2)
y <- c(1,1,2,3,2,1,3,1,2,4,4,2)

stopifnot(all.equal(CommKern:::zrand(x,y),-1.079724,tolerance=0.00001))

# Character test no. 1
a <- c("A", "A", "A", "A", "B", "C", "A", "B", "B", "C")
b <- c("C", "C", "C", "C", "A", "A", "B", "A", "B", "B")

stopifnot(all.equal(CommKern:::zrand(a,b),1.875521,tolerance=0.00001))

# Character test no. 2
a <- c("A", "A", "A", "A", "B", "C", "A", "B", "B", "C")
b <- c("C", "C", "D", "D", "A", "A", "B", "A", "B", "B")

stopifnot(all.equal(CommKern:::zrand(a,b),0.2908564,tolerance=0.00001))

# Mixed test no. 1
x <- c(1,3,1,2,3,3,3,2,1,2)
b <- c("C", "C", "C", "C", "A", "A", "B", "A", "B", "B")

stopifnot(all.equal(CommKern:::zrand(x,b),-0.8331956,tolerance=0.00001))

# Mixed test no. 2
a <- c("A", "A", "A", "A", "B", "C", "A", "B", "C", "C")
y <- c(1,1,1,1,2,3,1,2,3,3)

stopifnot(all.equal(CommKern:::zrand(a,y),6.633136,tolerance=0.00001))

# Verify expected error handles - partitions same length
x <- c(1,1,1,1,2,3,1,2,2,3)
y <- c(3,3,3,3,1,1,2,1,2,2,1)

rtn <- tryCatch(CommKern:::zrand(x,y),error = function(e) {e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "Partitions must be of the same length")

# Verify expected error handles - no NAs
x <- c(1,1,1,1,NA,2,3,1,2,2,3)
y <- c(3,3,3,3,1,1,2,1,2,2,1)

rtn <- tryCatch(CommKern:::zrand(x,y),error = function(e) {e})
stopifnot(inherits(rtn, "simpleError"))
stopifnot(rtn$message == "NAs are not supported")

library(CommKern)

# Numeric test no.1
x <- c(1,3,1,2,3,3,3,2,1,2,1,2)
y <- c(1,1,2,3,2,1,3,1,2,3,3,2)

stopifnot(all.equal(purity(x,y),0.5,tolerance=0.00001))

# Numeric test no.2
x <- c(1,3,1,2,3,3,3,2,1,2,1,2)
y <- c(1,1,2,3,2,1,3,1,2,4,4,2)

stopifnot(all.equal(purity(x,y),0.4545455,tolerance=0.00001))

# Character test no. 1
a <- c("A", "A", "A", "A", "B", "C", "A", "B", "B", "C")
b <- c("C", "C", "C", "C", "A", "A", "B", "A", "B", "B")

stopifnot(all.equal(purity(a,b),0.7,tolerance=0.00001))

# Character test no. 2
a <- c("A", "A", "A", "A", "B", "C", "A", "B", "B", "C")
b <- c("C", "C", "D", "D", "A", "A", "B", "A", "B", "B")

stopifnot(all.equal(purity(a,b),0.583333,tolerance=0.00001))

# Mixed test no. 1
x <- c(1,3,1,2,3,3,3,2,1,2)
b <- c("C", "C", "C", "C", "A", "A", "B", "A", "B", "B")

stopifnot(all.equal(purity(x,b),0.5,tolerance=0.00001))

# Mixed test no. 2
a <- c("A", "A", "A", "A", "B", "C", "A", "B", "C", "C")
y <- c(1,1,1,1,2,3,1,2,3,3)

stopifnot(all.equal(purity(a,y),1))




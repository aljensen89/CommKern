library(CommKern)

TOLERANCE = 1e-5

# Numeric test no.1
x <- c(1, 1, 1, 1, 2, 3, 1, 2, 2, 3)
y <- c(3, 3, 3, 3, 1, 1, 2, 1, 2, 2)

stopifnot(all.equal(NMI(x, y, variant = "max"),  0.4675499, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(x, y, variant = "min"),  0.4944531, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(x, y, variant = "sqrt"), 0.4808133, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(x, y, variant = "sum"),  0.4806253, tolerance = TOLERANCE))
#stopifnot(all.equal(NMI(x, y, variant = "joint"), 0.316311, tolerance = TOLERANCE))

# Numeric test no.2
x <- c(1, 3, 1, 2, 3, 3, 3, 2, 1, 2, 1, 2)
y <- c(1, 1, 2, 3, 2, 1, 3, 1, 2, 4, 4, 2)

stopifnot(all.equal(NMI(x, y, variant="max"), 0.1311733, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(x, y, variant="min"), 0.1587603, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(x, y, variant="sqrt"), 0.1443091, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(x, y, variant="sum"), 0.1436544, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(x, y, variant="joint"), 0.07738557, tolerance = TOLERANCE))

# Character test no. 1
a <- c("A", "A", "A", "A", "B", "C", "A", "B", "B", "C")
b <- c("C", "C", "C", "C", "A", "A", "B", "A", "B", "B")

stopifnot(all.equal(NMI(a, b, variant="max"), 0.4675499, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(a, b, variant="min"), 0.4944531, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(a, b, variant="sqrt"), 0.4808133, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(a, b, variant="sum"), 0.4806253, tolerance = TOLERANCE))
# stopifnot(all.equal(NMI(a, b, variant="joint"), 0.316311, tolerance = TOLERANCE))

# Character test no. 2
a <- c("A", "A", "A", "A", "B", "C", "A", "B", "B", "C")
b <- c("C", "C", "D", "D", "A", "A", "B", "A", "B", "B")

stopifnot(all.equal(NMI(a, b, variant="max"), 0.3726617, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(a, b, variant="min"), 0.4944531, tolerance = TOLERANCE))
# stopifnot(all.equal(NMI(a, b, variant="sqrt"), 0.4292495, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(a, b, variant="sum"), 0.4250042, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(a, b, variant="joint"), 0.2698447, tolerance = TOLERANCE))

# Mixed test no. 1
x <- c(1, 3, 1, 2, 3, 3, 3, 2, 1, 2)
b <- c("C", "C", "C", "C", "A", "A", "B", "A", "B", "B")

stopifnot(all.equal(NMI(x, b, variant="max"), 0.1400255, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(x, b, variant="min"), 0.1400255, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(x, b, variant="sqrt"), 0.1400255, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(x, b, variant="sum"), 0.1400255, tolerance = TOLERANCE))
stopifnot(all.equal(NMI(x, b, variant="joint"), 0.07528354, tolerance = TOLERANCE))

# Mixed test no. 2
a <- c("A", "A", "A", "A", "B", "C", "A", "B", "C", "C")
y <- c(1, 1, 1, 1, 2, 3, 1, 2, 3, 3)

stopifnot(all.equal(NMI(a, y, variant="max"), NMI(a, y, variant="min"),
     NMI(a, y, variant="sqrt"), NMI(a, y, variant="sum"),
     NMI(a, y, variant="joint")))

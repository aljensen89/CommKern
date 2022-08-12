library(CommKern)

# Testing to see if the allegiance matrix is square
id <- seq(1:10)
w <- c(1,1,1,1,2,3,1,2,3,3)
x <- c(2,2,2,1,3,3,3,1,1,1)
y <- c(1,1,1,1,1,3,1,2,2,3)
z <- c(1,2,3,1,2,3,1,2,3,1)

comm_mat <- cbind(id,w,z,y,z)

stopifnot(nrow(community_allegiance(comm_mat)) == ncol(community_allegiance(comm_mat)))

# Testing to see if the allegiance matrix is symmetric
id <- seq(1:10)
w <- c(1,1,1,1,2,3,1,2,3,3)
x <- c(2,2,2,1,3,3,3,1,1,1)
y <- c(1,1,1,1,1,3,1,2,2,3)
z <- c(1,2,3,1,2,3,1,2,3,1)

comm_mat <- cbind(id,w,z,y,z)

stopifnot(isSymmetric(community_allegiance(comm_mat)))

# Testing to see if the allegiance matrix has values in [0,1]
id <- seq(1:10)
w <- c(1,1,1,1,2,3,1,2,3,3)
x <- c(2,2,2,1,3,3,3,1,1,1)
y <- c(1,1,1,1,1,3,1,2,2,3)
z <- c(1,2,3,1,2,3,1,2,3,1)

comm_mat <- cbind(id,w,z,y,z)

stopifnot(all(community_allegiance(comm_mat)>=0) & 
            all(community_allegiance(comm_mat)<=1))

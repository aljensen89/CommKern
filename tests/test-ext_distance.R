library(CommKern)

# Testing that function returns a symmetric matrix
w <- c(1,2,2,1,1,2,2,2,3,2,1,3)
x <- c(2,2,3,1,3,1,3,3,2,2,1,1)
y <- c(3,3,2,1,1,1,1,2,2,3,2,3)
z <- c(1,1,2,3,2,3,2,1,1,2,3,3)

wxyz_comms <- data.frame(w_comm = w, x_comm = x, y_comm = y, z_comm = z)

wxyz_NMI_comms <- ext_distance(wxyz_comms, variant = 'NMI')
wxyz_ARI_comms <- ext_distance(wxyz_comms, variant = 'adj_RI')
wxyz_purity_comms <- ext_distance(wxyz_comms, variant = 'purity')

stopifnot(isSymmetric(wxyz_NMI_comms)==FALSE)
stopifnot(isSymmetric(wxyz_ARI_comms)==FALSE)
stopifnot(isSymmetric(wxyz_purity_comms)==FALSE)


# Testing that distance matrix has values >=0
w <- c(1,2,2,1,1,2,2,2,3,2,1,3)
x <- c(2,2,3,1,3,1,3,3,2,2,1,1)
y <- c(3,3,2,1,1,1,1,2,2,3,2,3)
z <- c(1,1,2,3,2,3,2,1,1,2,3,3)

wxyz_comms <- data.frame(w_comm = w, x_comm = x, y_comm = y, z_comm = z)

wxyz_NMI_comms <- ext_distance(wxyz_comms, variant = 'NMI')
wxyz_ARI_comms <- ext_distance(wxyz_comms, variant = 'adj_RI')
wxyz_purity_comms <- ext_distance(wxyz_comms, variant = 'purity')

stopifnot(all(wxyz_NMI_comms >=0)==TRUE)
stopifnot(all(wxyz_ARI_comms >=0)==TRUE)
stopifnot(all(wxyz_purity_comms >=0)==TRUE)

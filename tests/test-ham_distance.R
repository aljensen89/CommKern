library(CommKern)

# Testing that function returns a symmetric matrix
hamil_df <- data.frame(id  = seq(1:15),
                       ham = c(-160.5375, -167.8426, -121.7128, -155.7245,
                               -113.9834, -112.5262, -117.9724, -171.374,
                               -114.8563, -162.5725, -159.1265, -116.2658,
                               -163.7877, -115.9683, -118.8462))

ham_dist <- ham_distance(hamil_df)

stopifnot(isSymmetric(ham_dist)==FALSE)

# Testing that distance matrix has values >=0
hamil_df <- data.frame(id  = seq(1:15),
                       ham = c(-160.5375, -167.8426, -121.7128, -155.7245,
                               -113.9834, -112.5262, -117.9724, -171.374,
                               -114.8563, -162.5725, -159.1265, -116.2658,
                               -163.7877, -115.9683, -118.8462))

ham_dist <- ham_distance(hamil_df)

stopifnot(all(ham_dist >=0)==TRUE)

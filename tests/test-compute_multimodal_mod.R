library(CommKern)

# Verifying the output of compute_multimodal_mod
multimod_alt <- function(net, mod_matrix, communities, alpha){
  rtn <- 0
  
  for (i in 1:nrow(mod_matrix)){
    for (j in 1:ncol(mod_matrix)){
      if(i==j){
        next
      }
      
      if(communities[i]!=communities[j]){
        next
      }
      M_ij <- mod_matrix[i,j]
      S_ij <- net$str_matrix[i,j]
      
      rtn <- rtn + (M_ij+(alpha*S_ij))
    }
  }
  -rtn
}

mod_mat_ex <- CommKern:::compute_modularity_matrix(SBM_net)
comms <- sample(c(1,2),80,replace=TRUE)

stopifnot(all.equal(multimod_alt(net=SBM_net,mod_matrix=mod_mat_ex,
                           communities=comms,alpha=1),
                    CommKern:::compute_multimodal_mod(mod_matrix=mod_mat_ex,net=SBM_net,
                                                      communities=comms,alpha=1),
                    tolerance=0.00001))


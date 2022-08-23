# --------------------------------------------------------------------------------------------------
# Below is the code used to simulate the simasd_hamil_df and simasd_comm_df datasets, which are
# complementary to the simasd_covars dataset. Available within the CommKern package are these three
# datasets for end users to access.
# --------------------------------------------------------------------------------------------------

# Setting the seed
set.seed(42)

# Create the adjacency matrices for each of the groups
control_group_net <- group_network_perturb(n_nodes=80,n_comm=4,n_nets=26,perturb_prop=0.1,
                                           wcr=c(8,8),bcr=c(1.5,8),bfcr=NA,fuzzy_comms=NA)

control_group_adj <- group_adj_perturb(group_network=control_group_net,n_nets=26,n_nodes=80)

case_group_net <- group_network_perturb(n_nodes=80,n_comm=4,n_nets=23,perturb_prop=0.1,
                                        wcr=c(7,8),bcr=c(1.5,8),bfcr=NA,fuzzy_comms=NA)

case_group_adj <- group_adj_perturb(group_network=case_group_net,n_net=23,n_nodes=80)

# Quick check to see the general differences between the heatmaps of the cases and controls
pheatmap::pheatmap(control_group_adj[1,,],treeheight_row=0,treeheight_col=0,
                   main="Example Heatmap for Controls")

pheatmap::pheatmap(case_group_adj[1,,],treeheight_row=0,treeheight_col=0,
                   main="Example Heatmap for Cases")

# Putting case and control adj matrices in same order as in the simasd_covars dataset
# Do this via the dx_group variable, which tells us from which group to pull an adj matrix
dx_index <- simasd_covars$dx_group
simasd_array <- array(data=NA,dim=c(49,80,80))

case_counter <- 1
control_counter <- 1

for (i in 1:dim(simasd_array)[1]){
  if (dx_index[i]==0){
    simasd_array[i,,] <- control_group_adj[control_counter,,]
    control_counter <- control_counter +1
  } else if (dx_index[i]==1){
    simasd_array[i,,] <- case_group_adj[case_counter,,]
    case_counter <- case_counter +1
  }
}

# Making the hamiltonian_df and communities datasets

## Create empty structural matrix for the purposes of these simulations
sim_mat_str <- matrix(data=0.0,nrow=80,ncol=80)
rownames(sim_mat_str) <- seq(1:80)
colnames(sim_mat_str) <- seq(1:80)

## Create the hamiltonian dataframe
simasd_ham_df <- as.data.frame(matrix(data=NA,nrow=49,ncol=2))
colnames(simasd_ham_df) <- c("id","hamil")
simasd_ham_df$id <- seq(1:49)

## Create the communities dataframe
simasd_comm_df <- as.data.frame(matrix(data=NA,nrow=80,ncol=49))

for (b in 1:dim(simasd_array)[1]){
  sim_mat_func <- simasd_array[b,,]
  rownames(sim_mat_func) <- seq(1:80)
  colnames(sim_mat_func) <- seq(1:80)
  
  sim_network <- matrix_to_df(func_mat=sim_mat_func,str_mat=sim_mat_str)
  
  #Running the HMS algorithm and grabbing the resulting hamiltonian and communities
  hms_results <- hms(input_net=sim_network,spins=4,alpha=0,
                     coolfact=0.99,false_pos=0.01,max_layers=1)
  
  simasd_ham_df$hamil[b] <- hms_results$best_hamiltonian[1]
  simasd_comm_df[,b] <- hms_results$comm_layers_tree[,2]
}


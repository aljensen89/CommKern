### Simulations for Hierarchical, Multimodal Spinglass Algorithm ###

# Libraries and source functions ------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(reshape2)

source("community_plot.R")
source("matrix_to_df.R")
source("hier_mult_spin.R")

# Simulations based on Ashourvan et al. -----------------------------------------------------------
# Note: for these simulations, the structural matrix will be all zeros
sim_mat_str <- matrix(data=0.0,nrow=81,ncol=81)
rownames(sim_mat_str) <- seq(1:81)
colnames(sim_mat_str) <- seq(1:81)

# Simulation 1: symmetric, hierarchical ===========================================================
sim_mat <- matrix(data=0.0,nrow=81,ncol=81)

#Layer 1
sim_mat[1:27,1:27] <- 0.25
sim_mat[28:54,28:54] <- 0.15
sim_mat[55:81,55:81] <- 0.05

#Layer 2
sim_mat[1:9,1:9] <- 0.70
sim_mat[10:18,10:18] <- 0.77
sim_mat[19:27,19:27] <- 0.85
sim_mat[28:36,28:36] <- 0.45
sim_mat[37:45,37:45] <- 0.53
sim_mat[46:54,46:54] <- 0.60
sim_mat[55:63,55:63] <- 0.10
sim_mat[64:72,64:72] <- 0.17
sim_mat[73:81,73:81] <- 0.23

#Layer 3
sim_mat[1:3,1:3] <- 0.86
sim_mat[4:6,4:6] <- 0.87
sim_mat[7:9,7:9] <- 0.88
sim_mat[10:12,10:12] <- 0.91
sim_mat[13:15,13:15] <- 0.92
sim_mat[16:18,16:18] <- 0.93
sim_mat[19:21,19:21] <- 0.96
sim_mat[22:24,22:24] <- 0.97
sim_mat[25:27,25:27] <- 0.98
sim_mat[28:30,28:30] <- 0.62
sim_mat[31:33,31:33] <- 0.63
sim_mat[34:36,34:36] <- 0.64
sim_mat[37:39,37:39] <- 0.66
sim_mat[40:42,40:42] <- 0.67
sim_mat[43:45,43:45] <- 0.68
sim_mat[46:48,46:48] <- 0.73
sim_mat[49:51,49:51] <- 0.74
sim_mat[52:54,52:54] <- 0.75
sim_mat[55:57,55:57] <- 0.27
sim_mat[58:60,58:60] <- 0.28
sim_mat[61:63,61:63] <- 0.29
sim_mat[64:66,64:66] <- 0.31
sim_mat[67:69,67:69] <- 0.32
sim_mat[70:72,70:72] <- 0.33
sim_mat[73:75,73:75] <- 0.35
sim_mat[76:78,76:78] <- 0.36
sim_mat[79:81,79:81] <- 0.37

#Correlation map
sim_mat_melt <- reshape2::melt(sim_mat)

ggplot(sim_mat_melt,aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                       midpoint=0.5,limit=c(0,1),space="Lab", 
                       name="")+
  labs(x="Node",y="Node",title="Symmetric, Hierarchical Adjacancy Matrix")

#Creating network object for simulation 1
rownames(sim_mat) <- seq(1:81)
colnames(sim_mat) <- seq(1:81)

sim_network <- matrix_to_df(sim_mat,sim_mat_str)

#Running the hier_mult_spin function
sim_net_comm <- hms(input_net=sim_network,spins=3,alpha=0,coolfact=0.99,
                    false_pos=0.01,gamma=1,max_layers=3,parallel=FALSE)

#Plot of the communities
sim_mat_long <- reshape2::melt(sim_mat)
sim_comm_long <- 
  tidyr::gather(sim_net_comm,
                key = "layer",
                value = "comm",
                layer_1, layer_2, layer_3,
                factor_key = TRUE)

community_plot(comm_data = sim_comm_long, node_data = sim_mat_long)

# Simulation 1 + low noise (sd=0.01) ==============================================================
#Adding random noise
lownoise_sim1 <- matrix(rnorm(nrow(sim_mat)*ncol(sim_mat),mean=0,sd=0.01),
                        nrow=nrow(sim_mat))

sim_mat1_lownoise <- abs(sim_mat+lownoise_sim1) #to avoid negative correlation values
sim_mat1_lownoise[sim_mat1_lownoise > 1] <- 1 #to enforce correlation bounds [0,1]

#Correlation map
sim_mat_lownoise_melt <- reshape2::melt(sim_mat1_lownoise)

ggplot(sim_mat_lownoise_melt,aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                       midpoint=0.5,limit=c(0,1),space="Lab", 
                       name="")+
  labs(x="Node",y="Node",title="Symmetric, Hierarchical Adjacancy Matrix: Low Noise")

#Creating network object for simulation 1 + low noise
rownames(sim_mat1_lownoise) <- seq(1:81)
colnames(sim_mat1_lownoise) <- seq(1:81)

sim_lownoise_network <- matrix_to_df(sim_mat1_lownoise,sim_mat_str)

sim_lownoisenet_comm <- hms(input_net=sim_lownoise_network,spins=3,alpha=0,coolfact=0.99,
                            false_pos=0.01,gamma=1,max_layers=3,parallel=FALSE)

#Plot of the communities
sim_mat_low_long <- reshape2::melt(sim_mat1_lownoise)
sim_comm_low_long <- 
  tidyr::gather(sim_lownoisenet_comm,
                key = "layer",
                value = "comm",
                layer_1, layer_2, layer_3,
                factor_key = TRUE)

community_plot(comm_data = sim_comm_low_long, node_data = sim_mat_low_long)

# Simulation 1 + medium noise (sd=0.03) ===========================================================
#Adding random noise
mednoise_sim1 <- matrix(rnorm(nrow(sim_mat)*ncol(sim_mat),mean=0,sd=0.03),
                        nrow=nrow(sim_mat))

sim_mat1_mednoise <- abs(sim_mat+mednoise_sim1) #to avoid negative correlation values
sim_mat1_mednoise[sim_mat1_mednoise > 1] <- 1 #to enforce correlation bounds [0,1]

#Correlation map
sim_mat_mednoise_melt <- reshape2::melt(sim_mat1_mednoise)

ggplot(sim_mat_mednoise_melt,aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                       midpoint=0.5,limit=c(0,1),space="Lab", 
                       name="")+
  labs(x="Node",y="Node",title="Symmetric, Hierarchical Adjacancy Matrix: Medium Noise")

#Creating network object for simulation 1 + medium noise
rownames(sim_mat1_mednoise) <- seq(1:81)
colnames(sim_mat1_mednoise) <- seq(1:81)

sim_mednoise_network <- matrix_to_df(sim_mat1_mednoise,sim_mat_str)

sim_mednoisenet_comm <- hms(input_net=sim_mednoise_network,spins=3,alpha=0,coolfact=0.99,
                            false_pos=0.01,gamma=1,max_layers=3,parallel=FALSE)

#Plot of the communities
sim_mat_med_long <- reshape2::melt(sim_mat1_mednoise)
sim_comm_med_long <- 
  tidyr::gather(sim_mednoisenet_comm,
                key = "layer",
                value = "comm",
                layer_1, layer_2, layer_3,
                factor_key = TRUE)

community_plot(comm_data = sim_comm_med_long, node_data = sim_mat_med_long)

# Simulation 1 + high noise (sd=0.05) =============================================================
#Adding random noise
highnoise_sim1 <- matrix(rnorm(nrow(sim_mat)*ncol(sim_mat),mean=0,sd=0.05),
                         nrow=nrow(sim_mat))

sim_mat1_highnoise <- abs(sim_mat+highnoise_sim1) #to avoid negative correlation values
sim_mat1_highnoise[sim_mat1_highnoise > 1] <- 1 #to enforce correlation bounds [0,1]

#Correlation map
sim_mat_highnoise_melt <- reshape2::melt(sim_mat1_highnoise)

ggplot(sim_mat_highnoise_melt,aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                       midpoint=0.5,limit=c(0,1),space="Lab", 
                       name="")+
  labs(x="Node",y="Node",title="Symmetric, Hierarchical Adjacancy Matrix: High Noise")

#Creating network object for simulation 1 + high noise
rownames(sim_mat1_highnoise) <- seq(1:81)
colnames(sim_mat1_highnoise) <- seq(1:81)

sim_highnoise_network <- matrix_to_df(sim_mat1_highnoise,sim_mat_str)

sim_mednoisenet_comm <- hms(input_net=sim_highnoise_network,spins=3,alpha=0,coolfact=0.99,
                            false_pos=0.01,gamma=1,max_layers=3,parallel=FALSE)

#Plot of the communities
sim_mat_high_long <- reshape2::melt(sim_mat1_highnoise)
sim_comm_high_long <- 
  tidyr::gather(sim_mednoisenet_comm,
                key = "layer",
                value = "comm",
                layer_1, layer_2, layer_3,
                factor_key = TRUE)

community_plot(comm_data = sim_comm_high_long, node_data = sim_mat_high_long)

# Simulation 2: null graph ========================================================================
#This null graph was created by permuting the edges of simulation 1's graph
sim_mat_melt_null <- sim_mat_melt
sim_mat_melt_null$value <- sample(sim_mat_melt_null$value)

ggplot(sim_mat_melt_null,aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                       midpoint=0.5,limit=c(0,1),space="Lab", 
                       name="")+
  labs(x="Node",y="Node",title="Null Adjacancy Matrix")

sim_mat_null <- reshape2::acast(sim_mat_melt_null,Var1~Var2,value.var="value")

#Creating network object for simulation 2
rownames(sim_mat_null) <- seq(1:81)
colnames(sim_mat_null) <- seq(1:81)

sim_null_network <- matrix_to_df(sim_mat_null,sim_mat_str)

#Running the hier_mult_spin function
sim_null_comm <- hms(input_net=sim_null_network,spins=3,alpha=0,coolfact=0.99,
                     false_pos=0.01,gamma=1,max_layers=3,parallel=FALSE)

#Plot of the communities
sim_mat_null_long <- reshape2::melt(sim_mat_null)
sim_comm_null_long <- 
  tidyr::gather(sim_null_comm,
                key = "layer",
                value = "comm",
                layer_1, layer_2, layer_3,
                factor_key = TRUE)

community_plot(comm_data = sim_comm_null_long, node_data = sim_mat_null_long)

# Simulation 3: unequal community sizes, symmetric hierarchical ===================================
sim_mat3 <- matrix(data=0.0,nrow=81,ncol=81)

#Layer 1
sim_mat3[1:27,1:27] <- 0.25
sim_mat3[28:63,28:63] <- 0.15
sim_mat3[64:81,64:81] <- 0.05

#Layer 2
sim_mat3[1:9,1:9] <- 0.70
sim_mat3[10:18,10:18] <- 0.77
sim_mat3[19:27,19:27] <- 0.85
sim_mat3[28:39,28:39] <- 0.45
sim_mat3[40:51,40:51] <- 0.53
sim_mat3[52:63,52:63] <- 0.60
sim_mat3[64:69,64:69] <- 0.10
sim_mat3[70:75,70:75] <- 0.17
sim_mat3[76:81,76:81] <- 0.23

#Layer 3
sim_mat3[1:3,1:3] <- 0.86
sim_mat3[4:6,4:6] <- 0.87
sim_mat3[7:9,7:9] <- 0.88
sim_mat3[10:12,10:12] <- 0.91
sim_mat3[13:15,13:15] <- 0.92
sim_mat3[16:18,16:18] <- 0.93
sim_mat3[19:21,19:21] <- 0.96
sim_mat3[22:24,22:24] <- 0.97
sim_mat3[25:27,25:27] <- 0.98
sim_mat3[28:31,28:31] <- 0.62
sim_mat3[32:35,32:35] <- 0.63
sim_mat3[36:39,36:39] <- 0.64
sim_mat3[40:43,40:43] <- 0.66
sim_mat3[44:47,44:47] <- 0.67
sim_mat3[48:51,48:51] <- 0.68
sim_mat3[52:55,52:55] <- 0.73
sim_mat3[56:59,56:59] <- 0.74
sim_mat3[60:63,60:63] <- 0.75
sim_mat3[64:65,64:65] <- 0.27
sim_mat3[66:67,66:67] <- 0.28
sim_mat3[68:69,68:69] <- 0.29
sim_mat3[70:71,70:71] <- 0.31
sim_mat3[72:73,72:73] <- 0.32
sim_mat3[74:75,74:75] <- 0.33
sim_mat3[76:77,76:77] <- 0.35
sim_mat3[78:79,78:79] <- 0.36
sim_mat3[80:81,80:81] <- 0.37

#Correlation map
sim_mat3_melt <- reshape2::melt(sim_mat3)

ggplot(sim_mat3_melt,aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                       midpoint=0.5,limit=c(0,1),space="Lab", 
                       name="")+
  labs(x="Node",y="Node",title="Unequal Community Sizes \nHierarchical Adjacancy Matrix")

#Creating network object for simulation 3
rownames(sim_mat3) <- seq(1:81)
colnames(sim_mat3) <- seq(1:81)

sim3_network <- matrix_to_df(sim_mat3,sim_mat_str)

#Running the hier_mult_spin function
sim3_net_comm <- hms(input_net=sim3_network,spins=3,alpha=0,coolfact=0.99,
                     false_pos=0.01,gamma=1,max_layers=3,parallel=FALSE)

#Plot of the communities
sim_mat3_long <- reshape2::melt(sim_mat3)
sim_comm3_long <- 
  tidyr::gather(sim3_net_comm,
                key = "layer",
                value = "comm",
                layer_1, layer_2, layer_3,
                factor_key = TRUE)

community_plot(comm_data = sim_comm3_long, node_data = sim_mat3_long)

# Weighted SBM simulations ------------------------------------------------------------------------
#Note: this code was modified from the BTSBM functions in the HCD package

##BTSBM parameter inputs
## n: dimension of the network
## d: number of layers until leaves (excluding the root)
## a.seq: sequence of connection probability sequences along the tree
## lambda: average node degree, only used when alpha is not provided.
## noise_sd: standard deviation of the Gaussian noise added to adjacency matrix

Binary.Similarity <- function(s1,s2){
  n <- min(length(s1),length(s2))
  min(which(s1[1:n]!=s2[1:n]))
}

gen_P <- function(n,d,a.seq,lambda){
  K <- 2^d
  ## generate binary strings
  b.list <- list()
  for(k in 1:K){
    b.list[[k]] <- as.character(intToBits(k-1))[d:1]
  }
  ## construct B
  comm.sim.mat <- B <- matrix(0,K,K)
  for(i in 1:(K-1)){
    for(j in (i+1):K){
      s <- Binary.Similarity(b.list[[i]],b.list[[j]])-1
      comm.sim.mat[i,j] <- s+1
    }
  }
  comm.sim.mat <- comm.sim.mat+t(comm.sim.mat)
  diag(comm.sim.mat) <- d+1
  B[1:(K^2)] <- a.seq[d+2-as.numeric(comm.sim.mat)]
  w <- floor(n/K)
  g <- c(rep(seq(1,K),each=w),rep(K,n-w*K))
  Z <- matrix(0,n,K)
  Z[cbind(1:n,g)] <- 1
  P <- Z%*%B%*%t(Z)
  P <- P*lambda/mean(colSums(P))

  return(P)
}

# Functional and structural adjacency matrices ====================================================
##Probability matrix for structural connectivity
#Note: Structural connectivity is being simulated to be stronger than the functional connectivity
P_str <- P*1.2

##Probability matrix for functional connectivity
#Note: This code varies the average level of connectivity for each community, by layer

#Making changes to layer 3:
P[1:10,1:10] <- P[1:10,1:10]*1.5
P[11:20,11:20] <- P[11:20,11:20]*1.4
P[21:30,21:30] <- P[21:30,21:30]*1.3
P[31:40,31:40] <- P[31:40,31:40]*1.2
P[41:50,41:50] <- P[41:50,41:50]*1
P[51:60,51:60] <- P[51:60,51:60]*0.9
P[61:70,61:70] <- P[61:70,61:70]*0.8
P[71:80,71:80] <- P[71:80,71:80]*0.6

#Making changes to layer 2:
P[1:10,11:20] <- P[1:10,11:20]*1.1
P[11:20,1:10] <- P[11:20,1:10]*1.1

P[21:30,31:40] <- P[21:30,31:40]*1
P[31:40,21:30] <- P[31:40,21:30]*1

P[41:50,51:60] <- P[41:50,51:60]*0.8
P[51:60,41:50] <- P[51:60,41:50]*0.8

P[61:70,71:80] <- P[61:70,71:80]*0.6
P[71:80,61:70] <- P[71:80,61:70]*0.6

#Making changes to layer 1:
P[1:20,21:40] <- P[1:20,21:40]*2.5
P[21:40,1:20] <- P[21:40,1:20]*2.5

##Creating the adjacency matrices from the probability matrices
num_rows <- nrow(P)

upper.tri.index <- which(upper.tri(P))
tmp.rand <- runif(n=length(upper.tri.index))
A_func <- matrix(0,num_rows,num_rows)
A_str <- matrix(0,num_rows,num_rows)

P_func_keep <- upper.tri.index[(tmp.rand*0.5)<2*P[upper.tri.index]]
P_str_keep <- upper.tri.index[tmp.rand<P_str[upper.tri.index]]
P_func_noise <- rnorm(length(P_func_keep),mean=0,sd=noise_sd)
P_str_noise <- rnorm(length(P_str_keep),mean=0,sd=noise_sd)

#Functional connectivity should have fewer edges knocked out but a higher level of random noise
A_func[upper.tri.index[(tmp.rand*0.5)<2*P[upper.tri.index]]] <- P[P_func_keep] + (P_func_noise*2)
A_func <- A_func+t(A_func)
diag(A_func) <- 0

#Structural connectivity should have more edges knocked out (sparser matrix) but a lower level of random noise
A_str[upper.tri.index[tmp.rand<P_str[upper.tri.index]]] <- P_str[P_str_keep] + P_str_noise
A_str <- A_str+t(A_str)
diag(A_str) <- 0

##Plots of simulated structural and functional adjacency matrices
ggplot(data = melt(A_func), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=0.5,limit=c(0,1),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Functional Hierarchical Adjacancy Matrix")

ggplot(data = melt(A_str), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=0.5,limit=c(0,1),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Structural Hierarchical Adjacancy Matrix")

#Creating network object for SBM simulation
rownames(A_func) <- seq(1:80)
colnames(A_func) <- seq(1:80)

rownames(A_str) <- seq(1:80)
colnames(A_str) <- seq(1:80)

SBM_net <- matrix_to_df(A_func,A_str)

#Running the hier_mult_spin function - functional matrix only
SBM_funconly_comm <- hms(input_net=SBM_net,spins=2,alpha=0,coolfact=0.99,
                     false_pos=0.01,gamma=1,max_layers=3,parallel=FALSE)

#Plot of the communities
SBM_mat_long <- reshape2::melt(A_func)
SBM_funconly_comm_long <- 
  tidyr::gather(SBM_funconly_comm,
                key = "layer",
                value = "comm",
                layer_1, layer_2, layer_3,
                factor_key = TRUE)

community_plot(comm_data = SBM_funconly_comm_long, node_data = SBM_mat_long)

#Running the hier_mult_spin function - functional and structural matrices
SBM_funcstr_comm <- hms(input_net=SBM_net,spins=2,alpha=1,coolfact=0.99,
                         false_pos=0.01,gamma=1,max_layers=3,parallel=FALSE)

#Plot of the communities
SBM_mat_long <- reshape2::melt(A_func)
SBM_funcstr_comm_long <- 
  tidyr::gather(SBM_funcstr_comm,
                key = "layer",
                value = "comm",
                layer_1, layer_2, layer_3,
                factor_key = TRUE)

community_plot(comm_data = SBM_funcstr_comm_long, node_data = SBM_mat_long)

               
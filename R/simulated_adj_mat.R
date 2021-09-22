### Simulated hierarchical communities - based on Ashourvan et al simulations ###

##For all simulations, the structural matrix will be all zeros
sim_mat_str <- matrix(data=0.0,nrow=81,ncol=81)
rownames(sim_mat_str) <- seq(1:81)
colnames(sim_mat_str) <- seq(1:81)

##Simulation 1: symmetric, hierarchical
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

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/Symm_Hier_Graph.png",
    width=5,height=5,units="in",res=500)
ggplot(sim_mat_melt,aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                       midpoint=0.5,limit=c(0,1),space="Lab", 
                       name="")+
  labs(x="Node",y="Node",title="Symmetric, Hierarchical Adjacancy Matrix")
dev.off()

#Creating network object for simulation 1
rownames(sim_mat) <- seq(1:81)
colnames(sim_mat) <- seq(1:81)

sim_network <- matrix_to_df(sim_mat,sim_mat_str)

#Running the hier_mult_spin function
sim_net_comm <- hms(input_net=sim_network,spins=3,alpha=0,coolfact=0.99,
                    false_pos=0.01,gamma=1,max_layers=3,parallel=FALSE)

#Plot of the communities
sim_net_comm <- comm_layers_tree

sim_comm_long <- sim_net_comm %>%
  gather(layer,comm,layer_1:layer_3,factor_key=TRUE)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/Symm_Hier_Comm.png",
    width=5,height=5,units="in",res=500)
ggplot(sim_comm_long,aes(layer,node_id))+
  geom_raster(aes(fill=as.factor(comm)))+
  scale_fill_discrete(name="Community")+
  xlab("Layer")+ylab("Node ID")+ggtitle("Community Assignment by Layer")
dev.off()

hamiltonian_plot_sim <- data.frame(index=seq(1:length(hamiltonian_track)),
                               ham=hamiltonian_track)

ggplot(hamiltonian_plot_sim,aes(x=index,y=ham))+
  geom_line()+
  xlab("Index")+
  ylab("Hamiltonian Energy Value")+
  ggtitle("Hamiltonian Energy Value across Heatbath Algorithm")

##Adding noise to network - low noise (sd=0.01)
#Adding random noise
lownoise_sim1 <- matrix(rnorm(nrow(sim_mat)*ncol(sim_mat),mean=0,sd=0.01),
                nrow=nrow(sim_mat))

sim_mat1_lownoise <- abs(sim_mat+lownoise_sim1) #to avoid negative correlation values

#Correlation map
sim_mat_lownoise_melt <- reshape2::melt(sim_mat1_lownoise)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/LowNoise_Graph.png",
    width=5,height=5,units="in",res=500)
ggplot(sim_mat_lownoise_melt,aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                       midpoint=0.5,limit=c(0,1),space="Lab", 
                       name="")+
  labs(x="Node",y="Node",title="Symmetric, Hierarchical Adjacancy Matrix: Low Noise")
dev.off()

#Running the hier_mult_spin function
#Creating network object for simulation 1
rownames(sim_mat1_lownoise) <- seq(1:81)
colnames(sim_mat1_lownoise) <- seq(1:81)

sim_lownoise_network <- matrix_to_df(sim_mat1_lownoise,sim_mat_str)

sim_lownoisenet_comm <- hms(input_net=sim_lownoise_network,spins=3,alpha=0,coolfact=0.99,
                            false_pos=0.01,gamma=1,max_layers=3,parallel=FALSE)

#Plot of the communities
sim_lownoisenet_comm <- comm_layers_tree

sim_lownoisecomm_long <- sim_lownoisenet_comm %>%
  gather(layer,comm,layer_1:layer_3,factor_key=TRUE)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/LowNoise_Comm.png",
    width=5,height=5,units="in",res=500)
ggplot(sim_lownoisecomm_long,aes(layer,node_id))+
  geom_raster(aes(fill=as.factor(comm)))+
  scale_fill_discrete(name="Community")+
  xlab("Layer")+ylab("Node ID")+ggtitle("Low Noise Simulation: Community Assignment by Layer")
dev.off()

hamiltonian_plot_lownoisesim <- data.frame(index=seq(1:length(hamiltonian_track)),
                                   ham=hamiltonian_track)

ggplot(hamiltonian_plot_lownoisesim,aes(x=index,y=ham))+
  geom_line()+
  xlab("Index")+
  ylab("Hamiltonian Energy Value")+
  ggtitle("Hamiltonian Energy Value across Heatbath Algorithm: Low Noise")

##Adding noise to network - medium noise (sd=0.05)
#Adding random noise
mednoise_sim1 <- matrix(rnorm(nrow(sim_mat)*ncol(sim_mat),mean=0,sd=0.05),
                     nrow=nrow(sim_mat))

sim_mat1_mednoise <- abs(sim_mat+mednoise_sim1) #to avoid negative correlation values

#Correlation map
sim_mat_mednoise_melt <- reshape2::melt(sim_mat1_mednoise)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/MedNoise_Symm_Hier_Comm.png",
    width=5,height=5,units="in",res=500)
ggplot(sim_mat_mednoise_melt,aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                       midpoint=0.5,limit=c(0,1),space="Lab", 
                       name="")+
  labs(x="Node",y="Node",title="Symmetric, Hierarchical Adjacancy Matrix: Medium Noise")
dev.off()

#Running the hier_mult_spin function
#Creating network object for simulation 1
rownames(sim_mat1_mednoise) <- seq(1:81)
colnames(sim_mat1_mednoise) <- seq(1:81)

sim_mednoise_network <- matrix_to_df(sim_mat1_mednoise,sim_mat_str)

sim_mednoisenet_comm <- hms(input_net=sim_mednoise_network,spins=3,alpha=0,coolfact=0.99,
                         false_pos=0.01,gamma=1,max_layers=3,parallel=FALSE)

#Plot of the communities
sim_mednoisenet_comm <- comm_layers_tree

sim_mednoisecomm_long <- sim_mednoisenet_comm %>%
  gather(layer,comm,layer_1:layer_3,factor_key=TRUE)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/MedNoise_Symm_Hier_Comm.png",
    width=5,height=5,units="in",res=500)
ggplot(sim_noisecomm_long,aes(layer,node_id))+
  geom_raster(aes(fill=as.factor(comm)))+
  scale_fill_discrete(name="Community")+
  xlab("Layer")+ylab("Node ID")+ggtitle("Medium Noise Simulation: Community Assignment by Layer")
dev.off()

hamiltonian_plot_mednoisesim <- data.frame(index=seq(1:length(hamiltonian_track)),
                                        ham=hamiltonian_track)

ggplot(hamiltonian_plot_mednoisesim,aes(x=index,y=ham))+
  geom_line()+
  xlab("Index")+
  ylab("Hamiltonian Energy Value")+
  ggtitle("Hamiltonian Energy Value across Heatbath Algorithm: Medium Noise")



###Extra test: null graph with no hierarchical community structure###
sim_mat_melt_null <- sim_mat_melt
sim_mat_melt_null$value <- sample(sim_mat_melt_null$value)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/Null_Graph.png",
    width=5,height=5,units="in",res=500)
ggplot(sim_mat_melt_null,aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                       midpoint=0.5,limit=c(0,1),space="Lab", 
                       name="")+
  labs(x="Node",y="Node",title="Null Adjacancy Matrix")
dev.off()

sim_mat_null <- reshape2::acast(sim_mat_melt_null,Var1~Var2,value.var="value")

#Creating network object for simulation 1
rownames(sim_mat_null) <- seq(1:81)
colnames(sim_mat_null) <- seq(1:81)

sim_null_network <- matrix_to_df(sim_mat_null,sim_mat_str)

#Running the hier_mult_spin function
sim_null_comm <- hms(input_net=sim_null_network,spins=3,alpha=0,coolfact=0.99,
                    false_pos=0.01,gamma=1,max_layers=3,parallel=FALSE)

#Plot of the communities
sim_null_comm <- comm_layers_tree

sim_null_comm_long <- sim_null_comm %>%
  gather(layer,comm,layer_1:layer_3,factor_key=TRUE)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/Null_Comm.png",
    width=5,height=5,units="in",res=500)
ggplot(sim_null_comm_long,aes(layer,node_id))+
  geom_raster(aes(fill=as.factor(comm)))+
  scale_fill_discrete(name="Community")+
  xlab("Layer")+ylab("Node ID")+ggtitle("Community Assignment by Layer")
dev.off()


hamiltonian_plot_null <- data.frame(index=seq(1:length(hamiltonian_track)),
                                   ham=hamiltonian_track)

ggplot(hamiltonian_plot_null,aes(x=index,y=ham))+
  geom_line()+
  xlab("Index")+
  ylab("Hamiltonian Energy Value")+
  ggtitle("Hamiltonian Energy Value across Heatbath Algorithm: Null Graph")

###Extra tests: varying the false_pos value for just the first layer of simulation 1###
sim_net_comm_1 <- hms(input_net=sim_network,spins=3,alpha=0,coolfact=0.99,
                      false_pos=0.05,gamma=1,max_layers=1,parallel=FALSE)

sim_net_comm_1 <- comm_layers_tree #4 errors (4/81 = 0.0494)

sim_net_comm_2 <- hms(input_net=sim_network,spins=3,alpha=0,coolfact=0.99,
                      false_pos=0.025,gamma=1,max_layers=1,parallel=FALSE)

sim_net_comm_2 <- comm_layers_tree #2 errors (2/81 = 0.0247)

sim_net_comm_3 <- hms(input_net=sim_network,spins=3,alpha=0,coolfact=0.99,
                      false_pos=0.01,gamma=1,max_layers=1,parallel=FALSE)

sim_net_comm_3 <- comm_layers_tree #0 errors (0/81 = 0.000)

sim_net_comm_4 <- hms(input_net=sim_network,spins=3,alpha=0,coolfact=0.99,
                      false_pos=0.0075,gamma=1,max_layers=1,parallel=FALSE)

sim_net_comm_4 <- comm_layers_tree #0 errors (0/81 = 0.000)

##Simulation 2: not symmetric, hierarchical
sim_mat2 <- matrix(data=0.0,nrow=81,ncol=81)

#Layer 1
sim_mat2[1:27,1:27] <- 0.25
sim_mat2[28:54,28:54] <- 0.15
sim_mat2[55:81,55:81] <- 0.05

#Layer 2
sim_mat2[1:9,1:9] <- 0.70
sim_mat2[10:18,10:18] <- 0.77
sim_mat2[19:27,19:27] <- 0.85
sim_mat2[28:36,28:36] <- 0.45
sim_mat2[37:45,37:45] <- 0.53
sim_mat2[46:54,46:54] <- 0.60

#Layer 3
sim_mat2[1:3,1:3] <- 0.86
sim_mat2[4:6,4:6] <- 0.87
sim_mat2[7:9,7:9] <- 0.88
sim_mat2[10:12,10:12] <- 0.91
sim_mat2[13:15,13:15] <- 0.92
sim_mat2[16:18,16:18] <- 0.93
sim_mat2[19:21,19:21] <- 0.96
sim_mat2[22:24,22:24] <- 0.97
sim_mat2[25:27,25:27] <- 0.98

#Correlation map
sim_mat2_melt <- reshape2::melt(sim_mat2)

ggplot(sim_mat2_melt,aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                       midpoint=0.5,limit=c(0,1),space="Lab", 
                       name="")+
  labs(x="Node",y="Node",title="Not Symmetric, Hierarchical Adjacancy Matrix")

#Creating network object for simulation 2
rownames(sim_mat2) <- seq(1:81)
colnames(sim_mat2) <- seq(1:81)

sim2_network <- matrix_to_df(sim_mat2,sim_mat_str)

#Running the hier_mult_spin function
sim2_net_comm <- hms(input_net=sim2_network,spins=3,alpha=0,coolfact=0.99,
                    false_pos=0.01,gamma=1,max_layers=3,parallel=FALSE)

#Plot of the communities
sim2_net_comm <- comm_layers_tree

sim2_comm_long <- sim2_net_comm %>%
  gather(layer,comm,layer_1:layer_3,factor_key=TRUE)

ggplot(sim2_comm_long,aes(layer,node_id))+
  geom_raster(aes(fill=as.factor(comm)))+
  scale_fill_discrete(name="Community")+
  xlab("Layer")+ylab("Node ID")+ggtitle("Simulation 2: Community Assignment by Layer")

hamiltonian_plot_2 <- data.frame(index=seq(1:length(hamiltonian_track)),
                                    ham=hamiltonian_track)

ggplot(hamiltonian_plot_2,aes(x=index,y=ham))+
  geom_line()+
  xlab("Index")+
  ylab("Hamiltonian Energy Value")+
  ggtitle("Hamiltonian Energy Value across Heatbath Algorithm: Null Graph")

##Note: The issue seems to be that the true hamiltonian value equals zero when no community
##      structure exists. The algorithm can find this value of the hamiltonian, but it appears
##      that all community structures induced will produce a hamiltonian of zero.
##Note 2: An idea would be to run some kind of test to see if the hamiltonian is equal to zero
##        and, if this is the case, have an escape from the algorithm that doens't attempt a
##        further community structure. *Need to look into whether a hamiltonian of zero is 
##        possible outside of this unusual case*

#Grabbing the sub_net_layer that has no additional community structure
sim_mat2_sub <- sim_mat2[55:81,55:81]

#Adding random noise
noise <- matrix(rnorm(nrow(sim_mat2_sub)*ncol(sim_mat2_sub),mean=0,sd=0.001),
                nrow=nrow(sim_mat2_sub))

sim_mat2_noise <- sim_mat2_sub+noise

#Creating network object for simulation 2's noisy subnetwork
sim2_noise_network <- subset_matrix_to_df(sim_mat2_noise,sim_mat_str[55:81,55:81])

#Running the hier_mult_spin function
sim2_noise_net_comm <- hms(input_net=sim2_noise_network,spins=3,alpha=0,coolfact=0.99,
                           false_pos=0.01,gamma=1,max_layers=3,parallel=FALSE)

hamiltonian_plot_noise <- data.frame(index=seq(1:length(hamiltonian_track)),
                                 ham=hamiltonian_track)

ggplot(hamiltonian_plot_noise,aes(x=index,y=ham))+
  geom_line()+
  xlab("Index")+
  ylab("Hamiltonian Energy Value")+
  ggtitle("Hamiltonian Energy Value across Heatbath Algorithm: Noisy Subgraph")


##Simulation 3: unequal community sizes, symmetric hierarchical
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

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/Uneq_Hier_Graph.png",
    width=5,height=5,units="in",res=500)
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
sim3_net_comm <- comm_layers_tree

sim3_comm_long <- sim3_net_comm %>%
  gather(layer,comm,layer_1:layer_3,factor_key=TRUE)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/Uneq_Hier_Comm.png",
    width=5,height=5,units="in",res=500)
ggplot(sim3_comm_long,aes(layer,node_id))+
  geom_raster(aes(fill=as.factor(comm)))+
  scale_fill_discrete(name="Community")+
  xlab("Layer")+ylab("Node ID")+ggtitle("Community Assignment by Layer")
dev.off()

hamiltonian_plot_sim3 <- data.frame(index=seq(1:length(hamiltonian_track)),
                                   ham=hamiltonian_track)

ggplot(hamiltonian_plot_sim3,aes(x=index,y=ham))+
  geom_line()+
  xlab("Index")+
  ylab("Hamiltonian Energy Value")+
  ggtitle("Hamiltonian Energy Value across Heatbath Algorithm")


##Simulation 4: unequal community sizes, not symmetric hierarchical
sim_mat4 <- matrix(data=0.0,nrow=81,ncol=81)

#Layer 1
sim_mat4[1:54,1:54] <- 0.25
sim_mat4[55:81,55:81] <- 0.05

#Layer 2
sim_mat4[1:27,1:27] <- 0.77
sim_mat4[28:45,28:45] <- 0.53
sim_mat4[46:54,46:54] <- 0.60

#Layer 3
sim_mat4[1:3,1:3] <- 0.86
sim_mat4[4:9,4:9] <- 0.88
sim_mat4[10:18,10:18] <- 0.92
sim_mat4[19:21,19:21] <- 0.96
sim_mat4[22:27,22:27] <- 0.98
sim_mat4[28:30,28:30] <- 0.62
sim_mat4[31:36,31:36] <- 0.64
sim_mat4[37:39,37:39] <- 0.66
sim_mat4[40:45,40:45] <- 0.68

#Correlation map
sim_mat4_melt <- reshape2::melt(sim_mat4)

ggplot(sim_mat4_melt,aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                       midpoint=0.5,limit=c(0,1),space="Lab", 
                       name="")+
  labs(x="Node",y="Node",title="Unequal Community Sizes, not Symmetric \nHierarchical Adjacancy Matrix")


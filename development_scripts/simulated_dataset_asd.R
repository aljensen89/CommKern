#Libraries
library(tidyverse)
library(magrittr)
library(reshape2)
library(sn)

#Setting the seed
set.seed(80238)

#Creating the multinomial sample for handedness (1=R, 2=L, 3=Ambi)
hand_all <- rmultinom(2,23,c(0.8,0.15,0.05))

#Simulating the ASD portion of the dataframe
sim_asd <- data.frame(matrix(data=NA,nrow=23,ncol=8))
colnames(sim_asd) <- c("id","dx_group","sex","age","handedness","fullscale_IQ","verbal_IQ","nonverbal_IQ")

sim_asd$dx_group <- rep(1,nrow(sim_asd))
sim_asd$sex <- rbinom(nrow(sim_asd),1,0.3)
sim_asd$age <- round(sn::rsn(n=23,xi=26,omega=5,alpha=1,tau=0.5,dp=NULL),1)
sim_asd$handedness <- sample(c(rep(0,hand_all[1,1]),rep(1,hand_all[2,1]),rep(2,hand_all[3,1])))
sim_asd$verbal_IQ <- ceiling(rnorm(23,mean=107,sd=10))
sim_asd$nonverbal_IQ <- ceiling(72+(0.38*sim_asd$verbal_IQ))
sim_asd$fullscale_IQ <- ceiling(-17+(0.54*sim_asd$nonverbal_IQ)+(0.6*sim_asd$verbal_IQ))

#Simulating the control portion of the dataframe
cont_asd <- data.frame(matrix(data=NA,nrow=26,ncol=8))
colnames(cont_asd) <- c("id","dx_group","sex","age","handedness","fullscale_IQ","verbal_IQ","nonverbal_IQ")

cont_asd$dx_group <- rep(0,nrow(cont_asd))
cont_asd$sex <- rbinom(nrow(cont_asd),1,0.45)
cont_asd$age <- round(sn::rsn(n=26,xi=27,omega=6,alpha=-0.5,tau=-0.5,dp=NULL),1)
cont_asd$handedness <- sample(c(rep(0,hand_all[1,2]+3),rep(1,hand_all[2,2]),rep(2,hand_all[3,2])))
cont_asd$verbal_IQ <- ceiling(rnorm(26,mean=115,sd=10))
cont_asd$nonverbal_IQ <- ceiling(42+(0.60*cont_asd$verbal_IQ))
cont_asd$fullscale_IQ <- ceiling(-12+(0.54*cont_asd$nonverbal_IQ)+(0.55*cont_asd$verbal_IQ))

#Combining the two dataframes
simasd_covars <- rbind(sim_asd,cont_asd) 




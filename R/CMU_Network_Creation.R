### Data Management for the CMU Dataset ###

# Libraries ---------------------------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(magrittr)
library(reshape)
library(MNS)
library(R.matlab)
library(qwraps2)

# Structural connectivity work --------------------------------------------------------------------
#Setting the SC working directory
setwd("/Users/jenseale/Dropbox/PhD_Dissertation_Work/CMU_SC_BOLD/SC")

#Bringing in the SC Matlab matrices
sc_full_files<-list.files(path=".",pattern="*.mat")
sc_full_mydata<-lapply(sc_full_files,function(X) readMat(X)$connectivity)
sc_array <- array(dim=c(dim(sc_full_mydata[[1]])[1],dim(sc_full_mydata[[1]])[2],length(sc_full_files)))

#Adding the SC data to the empty array
for (i in 1:dim(sc_array)[3]) {
  sc_array[,,i]<-sc_full_mydata[[i]]
}

#Grabbing the coaxid numbers for the SC matrices
sc_coaxid<-stringr::str_extract(sc_full_files, "^.{4}")

# Functional connectivity work --------------------------------------------------------------------
#Setting the FC working directory
setwd("/Users/jenseale/Dropbox/PhD_Dissertation_Work/CMU_SC_BOLD/")

#Bringing in the FC Matlab matrices
fc_full_files<-list.files(path=".",pattern="*.mat")
cmu_bold_files<-readMat(fc_full_files)

# Subject demographics work -----------------------------------------------------------------------
#Rownames of the attributes
attr_vars<-rownames(as.data.frame(cmu_bold_files$attributes[,,1]$subject[,,1][1]))

attr_df<-data.frame()
for (k in attr_vars){
  attr_df[[k]]<-as.character()
}

attr_df$coaxid <- stringr::str_pad(attr_df$coaxid,pad="0",width=4)

#Filling in the attributes data frame
count<-1
for(i in 1:dim(cmu_bold_files$attributes[,,1]$subject)[1]){
  new_row<-t(as.data.frame(cmu_bold_files$attributes[,,1]$subject[,,1][i]))
  attr_df[count,]<-new_row
  count<-count+1
}

#Converting NaN to NA
attr_df[attr_df=="NaN"]<-NA

# Creating the functional connectivity adjacency matrices -----------------------------------------
#Pulling the BOLD time series for each ROI and calculating cross-correlations for adjacency matrices
fc_array<-array(dim=c(dim(sc_full_mydata[[1]])[1],dim(sc_full_mydata[[1]])[2],
                      length(names(cmu_bold_files$bold[,,1]$avgs[,,1]))))

for (i in 1:length(names(cmu_bold_files$bold[,,1]$avgs[,,1]))){
  names<-names(cmu_bold_files$bold[,,1]$avgs[,,1])
  for (j in 1:dim(sc_full_mydata[[1]])[1]){
    for (k in 1:dim(sc_full_mydata[[1]])[1]){
      fc_array[j,k,i]<-ccf(ts(cmu_bold_files$bold[,,1]$avgs[,,1][[names[i]]][,,1]$rois.data[j,]),
                             ts(cmu_bold_files$bold[,,1]$avgs[,,1][[names[i]]][,,1]$rois.data[k,]),
                             type=c("correlation"),plot=FALSE)$acf[,,1][24]
    }
  }
}

# Creating the analytic dataset -------------------------------------------------------------------
#Finding the subjects who don't have a fMRI scan but do have a structural scan
sc_only_ids <- subset(sc_coaxid,!(sc_coaxid %in% attr_df$coaxid))
sc_only_vecloc <- match(sc_only_ids,sc_coaxid)

#Subsetting the sc_array to exclude the cerebellar ROIs
sc_array_subset<-array(dim=c(600,600,length(sc_full_files)))

for (i in 1:dim(sc_array)[3]){
  sc_array_subset[,,i] <- sc_array[1:600,1:600,i]

  rownames(sc_array_subset[,,i]) <- c(1:nrow(sc_array_subset[,,i]))
  colnames(sc_array_subset[,,i]) <- c(1:nrow(sc_array_subset[,,i]))
}

sc_array_subset <- sc_array_subset[,,-sc_only_vecloc]

#Subsetting the fc_array to exclude the cerebellar ROIs 
fc_array_subset<-array(dim=c(600,600,dim(fc_array)[3]))

for (i in 1:dim(fc_array)[3]){
  fc_array_subset[,,i] <- fc_array[1:600,1:600,i]
  
  rownames(fc_array_subset[,,i]) <- c(1:nrow(fc_array_subset[,,i]))
  colnames(fc_array_subset[,,i]) <- c(1:nrow(fc_array_subset[,,i]))
}

# Mean, SD, and CV of structural and functional connectivity --------------------------------------
#Calculating the mean sc_array
mean_sc_array <- rowMeans(sc_array_subset,dim=2,na.rm=TRUE)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_SC_Average.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(mean_sc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=0.5,limit=c(0,1),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Average Structural Adjacancy Matrix")
dev.off()

#Calculating the standard deviation of sc_array
sd_sc_array <- apply(sc_array_subset,1:2,sd)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_SC_StandDev.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(sd_sc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=0.5,limit=c(0,1),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Standard Deviation Structural Adjacancy Matrix")
dev.off()

#Calculating the coefficient of variation of sc_array
cf_sc_array <- (sd_sc_array/mean_sc_array)*100

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_SC_CoefVar.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(cf_sc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=400,limit=c(0,800),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Coefficient of Variation Structural Adjacancy Matrix")
dev.off()

#Calculating the mean fc_array
mean_fc_array <- rowMeans(fc_array_subset,dim=2,na.rm=TRUE)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_FC_Average.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(mean_fc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=0.5,limit=c(0,1),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Average Functional Adjacancy Matrix")
dev.off()

#Calculating the standard deviation of fc_array
sd_fc_array <- apply(fc_array_subset,1:2,sd)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_SC_StandDev.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(sd_fc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=0.25,limit=c(0,0.5),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Standard Deviation Functional Adjacancy Matrix")
dev.off()

#Calculating the coefficient of variation of fc_array
cf_fc_array <- (sd_fc_array/mean_fc_array)*100

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_FC_CoefVar.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(cf_fc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=400,limit=c(0,800),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Coefficient of Variation Functional Adjacancy Matrix")
dev.off()

#Saving .Rdata objects
#save(sc_array_subset,file="/Users/jenseale/Dropbox/PhD_Dissertation_Work/CMU_SC_BOLD/sc_array_subset.Rdata")
#save(fc_array_subset,file="/Users/jenseale/Dropbox/PhD_Dissertation_Work/CMU_SC_BOLD/fc_array_subset.Rdata")

# CMU Demographics --------------------------------------------------------------------------------
#Variables to include in Table 1: age, sex, handedness
attr_df$age <- as.numeric(attr_df$age)
attr_df$handedness_mod <- ifelse(attr_df$handedness=="R" | attr_df$handedness=="Right","Right",
                             ifelse(attr_df$handedness=="L" | attr_df$handedness=="Left","Left",NA))

tabone_sum <- list("Age (years)" =
                    list("mean (sd)" = ~qwraps2::mean_sd(age,na_rm=TRUE)),
                  "Sex" =
                    list("Male (n (%))" = ~qwraps2::n_perc0(sex=="M",na_rm=TRUE,show_symbol=TRUE),
                         "Female (n (%))" = ~qwraps2::n_perc0(sex=="F",na_rm=TRUE,show_symbol=TRUE)),
                  "Handedness" =
                    list("Left (n (%))" = ~qwraps2::n_perc0(handedness_mod=="Left",na_rm=TRUE,show_symbol=TRUE),
                         "Right (n (%))" = ~qwraps2::n_perc0(handedness_mod=="Right",na_rm=TRUE,show_symbol=TRUE)))

table_one <- summary_table(attr_df,tabone_sum)
knitr::kable(table_one)



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
setwd("/Users/jenseale/Dropbox/PhD_Dissertation_Work/CMU_SC_BOLD/2015_11_10 ARL connectivity matrix")

#Bringing in the SC Matlab matrices
sc_full_files<-list.files(path=".",pattern="*AAL626.count.end.connectivity.mat")
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
fc_full_files<-list.files(path=".",pattern="*CMU_BOLD.mat")
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

# Creating the functional connectivity matrices ---------------------------------------------------
fc_array<-array(dim=c(dim(sc_full_mydata[[1]])[1],dim(sc_full_mydata[[1]])[2],
                      length(names(cmu_bold_files$bold[,,1]$avgs[,,1]))))

for (i in 1:length(names(cmu_bold_files$bold[,,1]$avgs[,,1]))){
  names<-names(cmu_bold_files$bold[,,1]$avgs[,,1])
  ts_list <- t(cmu_bold_files$bold[,,1]$avgs[,,1][[names[i]]][,,1]$rois.data)
  cor_mat <- corpcor::cor.shrink(ts_list,verbose=TRUE)
  fc_array[,,i] <- cor_mat[1:626,1:626]
}

#Pulling the BOLD time series for each ROI and calculating cross-correlations for adjacency matrices
# fc_array<-array(dim=c(dim(sc_full_mydata[[1]])[1],dim(sc_full_mydata[[1]])[2],
#                       length(names(cmu_bold_files$bold[,,1]$avgs[,,1]))))
# 
# for (i in 1:length(names(cmu_bold_files$bold[,,1]$avgs[,,1]))){
#   names<-names(cmu_bold_files$bold[,,1]$avgs[,,1])
#   for (j in 1:dim(sc_full_mydata[[1]])[1]){
#     for (k in 1:dim(sc_full_mydata[[1]])[1]){
#       fc_array[j,k,i]<-ccf(ts(cmu_bold_files$bold[,,1]$avgs[,,1][[names[i]]][,,1]$rois.data[j,]),
#                              ts(cmu_bold_files$bold[,,1]$avgs[,,1][[names[i]]][,,1]$rois.data[k,]),
#                              type=c("correlation"),plot=FALSE)$acf[,,1][24]
#     }
#   }
# }

# Creating the analytic dataset -------------------------------------------------------------------
#Finding the subjects who don't have a fMRI scan but do have a structural scan
attr_df$coaxid <- stringr::str_pad(attr_df$coaxid,pad="0",width=4)
sc_only_ids <- subset(sc_coaxid,!(sc_coaxid %in% attr_df$coaxid))
sc_only_vecloc <- match(sc_only_ids,sc_coaxid)

#Subsetting the sc_array to exclude the cerebellar ROIs
sc_array_subset<-array(dim=c(600,600,length(sc_full_files)))

for (i in 1:dim(sc_array)[3]){
  sc_array_subset[,,i] <- sc_array[1:600,1:600,i]
  
  sc_array_subset[,,i] <- log(sc_array_subset[,,i]+1)/max(log(sc_array_subset[,,i]+1))

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

# Mean, SD, and histograms of structural and functional connectivity -------------------------------
## Calculating the mean sc_array -------------------------------------------------------------------
mean_sc_array <- rowMeans(sc_array_subset,dim=2,na.rm=TRUE)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_SC_Average.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(mean_sc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=0.5,limit=c(0,1),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Average Structural Adjacancy Matrix")
dev.off()

## Calculating the standard deviation of sc_array -------------------------------------------------
sd_sc_array <- apply(sc_array_subset,1:2,sd)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_SC_StandDev.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(sd_sc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=0.2,limit=c(0,0.4),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Standard Deviation Structural Adjacancy Matrix")
dev.off()

## Creating histogram of fiber counts - individual subject ----------------------------------------
sc_fibers_subject <- reshape2::melt(sc_array[1:600,1:600,1])
sc_fibers_subnoneg <- sc_fibers_subject %>% filter(value>0)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_FiberCountsHist_IndSub.png",
    width=5,height=5,units="in",res=500)
ggplot(sc_fibers_subnoneg, aes(value)) + geom_histogram(bins=100,colour="black",fill="aquamarine4") +
  scale_y_continuous(trans="log10") + scale_x_continuous(breaks=c(0,500,1000,1500,2000,2500,3000)) +
  labs(x="Fiber Count",y="Frequency (log10)",title="SC Fiber Count Histogram: Individual Subject")
dev.off()

## Creating histogram of fiber counts - all subjects ----------------------------------------------
sc_fibers_subset <- sc_array[,,-sc_only_vecloc]
sc_fibers_all <- data.frame()

for (i in 1:dim(sc_fibers_subset)[3]){
  melt <- reshape2::melt(sc_array[1:600,1:600,i])
  sc_fibers_all <- rbind(sc_fibers_all,melt)
}

sc_fibers_allnoneg <- sc_fibers_all %>% filter(value>0)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_FiberCountsHist_AllSub.png",
    width=5,height=5,units="in",res=500)
ggplot(sc_fibers_allnoneg, aes(value)) + geom_histogram(bins=100,colour="black",fill="aquamarine4") +
  scale_y_continuous(trans="log10") + scale_x_continuous(breaks=c(0,2500,5000,7500,10000,12500,15000)) +
  labs(x="Fiber Count",y="Frequency (log10)",title="SC Fiber Count Histogram: All Subjects")
dev.off()

## Creating histogram of normalized fiber counts - individual subject -----------------------------
sc_norm_subject <- reshape2::melt(sc_array_subset[,,1])
sc_norm_subnoneg <- sc_norm_subject %>% filter(value>0)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_NormFiber_IndSub.png",
    width=5,height=5,units="in",res=500)
ggplot(sc_norm_subnoneg, aes(value)) + geom_histogram(colour="black",fill="mediumorchid4") +
  labs(x="Normalized Fiber Value",y="Frequency",
       title="SC Normalized Fiber Count Histogram: Individual Subject")
dev.off()

## Creating histogram of normalized fiber counts - all subjects -----------------------------------
sc_norm_all <- data.frame()

for (i in 1:dim(sc_array_subset)[3]){
  melt <- reshape2::melt(sc_array_subset[,,i])
  sc_norm_all <- rbind(sc_norm_all,melt)
}

sc_norm_allnoneg <- sc_norm_all %>% filter(value>0)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_FiberCountsHist_AllSub.png",
    width=5,height=5,units="in",res=500)
ggplot(sc_norm_allnoneg, aes(value)) + geom_histogram(colour="black",fill="mediumorchid4") +
  scale_y_continuous(trans="log10") +
  labs(x="Normalized Fiber Value",y="Frequency (log10)",
       title="SC Normalized Fiber Count Histogram: All Subjects")
dev.off()

## Calculating the mean fc_array ------------------------------------------------------------------
mean_fc_array <- rowMeans(fc_array_subset,dim=2,na.rm=TRUE)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_FC_Average.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(mean_fc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=0.5,limit=c(0,1),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Average Functional Adjacancy Matrix")
dev.off()

## Calculating the standard deviation of fc_array -------------------------------------------------
sd_fc_array <- apply(fc_array_subset,1:2,sd)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_FC_StandDev.png",
    width=5,height=5,units="in",res=500)
ggplot(data = reshape2::melt(sd_fc_array), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low="navy",high="goldenrod1",mid="darkturquoise", 
                                   midpoint=0.25,limit=c(0,0.5),space="Lab", 
                                   name="")+
  labs(x="Node",y="Node",title="Standard Deviation Functional Adjacancy Matrix")
dev.off()

## Histogram of functional correlation values - individual subject ---------------------------------
fc_subject <- reshape2::melt(fc_array_subset[,,1])
fc_subject_noneg <- fc_subject %>% filter(value>0)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_FC_IndSub.png",
    width=5,height=5,units="in",res=500)
ggplot(fc_subject_noneg, aes(value)) + geom_histogram(colour="black",fill="lightsalmon3") +
  labs(x="Correlation Value",y="Frequency",title="FC Histogram: Individual Subject")
dev.off()

## Histogram of functional correlation values - all subjects ---------------------------------------
fc_all <- data.frame()

for (i in 1:dim(fc_array_subset)[3]){
  melt <- reshape2::melt(fc_array_subset[,,i])
  fc_all <- rbind(fc_all,melt)
}

fc_allnoneg <- fc_all %>% filter(value>0)

png(filename="/Users/jenseale/Dropbox/PhD_Dissertation_Work/Figures/CMU_FC_AllSub.png",
    width=5,height=5,units="in",res=500)
ggplot(fc_allnoneg, aes(value)) + geom_histogram(colour="black",fill="lightsalmon3") +
  labs(x="Correlation Value",y="Frequency",title="FC Histogram: All Subjects")
dev.off()

# Saving .Rdata objects ----------------------------------------------------------------------------
#save(sc_array_subset,file="/Users/jenseale/Dropbox/PhD_Dissertation_Work/CMU_SC_BOLD/sc_array_subset.Rdata")
#save(fc_array_subset,file="/Users/jenseale/Dropbox/PhD_Dissertation_Work/CMU_SC_BOLD/fc_array_subset.Rdata")

# CMU Demographics --------------------------------------------------------------------------------
#Variables to include in Table 1: age, sex, handedness
attr_df$age <- as.numeric(attr_df$age)
attr_df$handedness_mod <- ifelse(attr_df$handedness=="R" | attr_df$handedness=="Right","Right",
                             ifelse(attr_df$handedness=="L" | attr_df$handedness=="Left","Left",NA))

tabone_sum <- list("Age (years)" =
                    list("mean (sd)" = ~qwraps2::mean_sd(age,na_rm=TRUE),
                         "min" = ~ min(age,na.rm=TRUE),
                         "max" = ~max(age,na.rm=TRUE)),
                  "Sex" =
                    list("Male (n (%))" = ~qwraps2::n_perc0(sex=="M",na_rm=TRUE,show_symbol=TRUE),
                         "Female (n (%))" = ~qwraps2::n_perc0(sex=="F",na_rm=TRUE,show_symbol=TRUE)),
                  "Handedness" =
                    list("Left (n (%))" = ~qwraps2::n_perc0(handedness_mod=="Left",na_rm=TRUE,show_symbol=TRUE),
                         "Right (n (%))" = ~qwraps2::n_perc0(handedness_mod=="Right",na_rm=TRUE,show_symbol=TRUE)))

table_one <- summary_table(attr_df,tabone_sum)
knitr::kable(table_one)

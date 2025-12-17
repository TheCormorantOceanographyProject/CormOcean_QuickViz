library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table) #rename
library(stringr)
library(R.utils)
library(tidyr)
library(R.matlab)

if(Sys.info()[7]=="rachaelorben") {
  usrdir<-"/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/"
  datadir<-'/data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'/data/Field Data/Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

if(Sys.info()[7]=="alexa") {
  usrdir<-"/Users/alexa/Box Sync/DASHCAMS/"
  datadir<-'data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'data/Field Data/Deployment_Field_Data.csv'
  source('/Users/alexa/git_repos/CormOcean_QuickViz/MakeDive.R')
}

op <- options(digits.secs=3)

#  Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))
deploy_matrix$DeploymentStartDatetime_Local<-mdy_hm(deploy_matrix$DeploymentStartDatetime_Local)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
dm<-deploy_matrix%>%dplyr::select(Bird_ID,TagSerialNumber,Project_ID,
                                  DeploymentStartDatetime_Local,Deployment_End_Short,
                                  DeploymentEndDatetime_UTC,TagManufacturer)%>%
  filter(is.na(TagSerialNumber)==FALSE)

#all project names
prjt_all<-unique(dm$Project_ID)

#projects with tags currently deployed
tags_current<-dm%>%filter(is.na(DeploymentEndDatetime_UTC))
prjt_current<-unique(tags_current$Project_ID)

#projects with all end dates complete
prjt_complete<-prjt_all[!(prjt_all %in% prjt_current)]

#projects to process: change as needed
#prjt<-prjt_current
prjt<-prjt_all
prjt<-prjt[prjt!="USACRBRDO14"]
#prjt<-prjt[prjt!="USACRBRPE19"] #I am not sure why this one is missing

prjt
# Loop through each project -----------------------------------------------
# you can run project individually by picking an i value i=10 gives you "PERIPGU22_SC" etc. 
# then just run the code after the initial for statement
for (i in 54:length(prjt)){
 #"KORGUGR25
  #"SRILAIN25"
  #"LITKAGR25"
  
  # Find Project Data Files 
  Files<-list.files(paste0(usrdir,savedir,"Processed_2_DiveID_ByBird/"), pattern = prjt[i], full.names = TRUE)
  filenames<-list.files(paste0(usrdir,savedir,"Processed_2_DiveID_ByBird/"),pattern = prjt[i])
  
  Birds_dpth<-NULL
  for (k in 1:length(Files)){
    birdy_d<-readRDS(paste0(usrdir,savedir,"Processed_2_DiveID_ByBird/",filenames[k]))
    Birds_dpth<-bind_rows(Birds_dpth,birdy_d)
  }
  rm(birdy_d)

saveRDS(Birds_dpth, paste0(usrdir,savedir,"Processed_3_DiveID_ByDeployment/",prjt[i],"_DiveID.rds"))
#filename=paste0(usrdir,savedir,"Processed_3_DiveID_ByDeployment_mat/",prjt[i],"_DiveID.mat")

#rao: commented out Sept 2024 since data and column names don't match 
#rao: I think these might have been for Jim, but I am not sure if they are still needed?
# writeMat(filename,
#          oid=Birds_dpth$oid,
#          ID=Birds_dpth$ID,
#          datetime=Birds_dpth,
#          fixNames=TRUE)
}



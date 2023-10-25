library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table) #rename
library(stringr)
library(R.utils)
library(tidyr)

if(Sys.info()[7]=="rachaelorben") {
  usrdir<-"/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/"
  datadir<-'/data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

op <- options(digits.secs=3)

#  Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
dm<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC,TagManufacture)%>%
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

#NO DIVE NOTES: i =1:6
#"USACRBRPE19"  "USAMIPE20"    "UAEBUSO20"    "UAESISO20"    "LITCUGR21"    "USAFIBR21" 
#DIVE NOTES: i =7
#BAHHASO21, USACRBR22 - possibly started midway through also NOTE_DIVING in datatype - not on and off

# Loop through each project -----------------------------------------------
for (i in i:length(prjt)){
  
  # Find Project Data Files -------------------------------------------------
  Files<-list.files(paste0(usrdir,savedir,"Processed_DiveID_Deployment_Data/"), pattern = prjt[i],full.names = TRUE)
  filenames<-list.files(paste0(usrdir,savedir,"Processed_DiveID_Deployment_Data/"),pattern = prjt[i])
  
  Birds_dpth<-NULL
  for (k in 1:length(Files)){
    birdy_d<-readRDS(paste0(usrdir,savedir,"Processed_DiveID_Deployment_Data/",filenames[k]))
    Birds_dpth<-rbind(Birds_dpth,birdy_d)
  }
  rm(birdy_d)
  
  
  
}


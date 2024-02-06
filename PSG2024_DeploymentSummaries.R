### Deployment Summaries ###
library(data.table) #fread
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(sf)
library(MetBrewer)

if(Sys.info()[7]=="alexa") {
  usrdir<-"/Users/alexa/Box Sync/DASHCAMS/"
  datadir<-'data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  #savedir<-'Research Workspace/Project Metadata/Bounding Coordinates'
  deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/alexa/git_repos/CormOcean_QuickViz/MakeDive.R')
}

if(Sys.info()[7]=="rachaelorben") {
  usrdir<-"/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/"
  datadir<-'data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

#  Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
dm<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,Deployment_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC,TagManufacture)%>%
  filter(is.na(TagSerialNumber)==FALSE) #selects columns wanted in dm, added deployment ID to my selection

# Project info ------------------------------------------------------------
prj_info<-read.csv(paste0(usrdir,"/data/Field Data/Project Titles and IDs.csv"))

prjt<-unique(deploy_matrix$Project_ID)
rm(deploy_matrix)
prjt<-prjt[prjt!="USACRBRDO14"] #removes MCR 2014
prjt<-prjt[prjt!="SAUJUSO23"]
prjt

# Find Project Data Files -------------------------------------------------
# eventually change to pull in gps only files
# I can't seem to get the for loop below to work - Alexa
for (i in 1:length(prjt)){
  Files<-list.files(paste0(usrdir,datadir,prjt[i],"/gps_sensors_v2"), full.names = TRUE)
  filenames<-list.files(paste0(usrdir,datadir,prjt[i],"/gps_sensors_v2"))
  
  if (length(Files)==0) next

}
prjt


library(data.table) #fread
library(dplyr)
library(ggplot2)

if(Sys.info()[7]=="alexa") {
  usrdir<-"/Users/alexa/Box Sync/DASHCAMS/"
  datadir<-'data/ornitela_for_ATN/'
  savedir<-'Research Workspace/Project Metadata/Bounding Coordinates'
  deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
}

#  Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
dm<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,Deployment_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC,TagManufacture)%>%
  filter(is.na(TagSerialNumber)==FALSE) #selects columns wanted in dm, added deployment ID to my selection

prjt<-unique(dm$Project_ID)
prjt<-prjt[prjt!="USACRBRDO14"] #removes MCR 2014
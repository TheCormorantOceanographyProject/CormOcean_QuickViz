library(data.table) #fread
library(dplyr)
library(ggplot2)


if(Sys.info()[7]=="rachaelorben") {
  usrdir<-"/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/"
  datadir<-'/data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

#  Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
dm<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC,TagManufacture)%>%
  filter(is.na(TagSerialNumber)==FALSE)

#all project names
prjt<-unique(deploy_matrix$Project_ID)
rm(deploy_matrix)

# needs the following to be ajusted to work with data from Ornitela

extent<-NULL
for (i in 1:length(prjt)){
  
  locs<-readRDS(paste0(usrdir,savedir,"Processed_Deployment_Data/",prjt[i],"_GPS_SpeedFiltered.rds"))
  #dat<-dat%>%filter(is.na(LatDegree)==FALSE)
  dat<-dat%>%select(BirdID,LatDegree,LongDegree,ObsDepth,CalDepth)
  
  
  
  DAT$species<-sapply(strsplit(DAT$BirdID, split='_', fixed=TRUE), function(x) (x[1]))

  min(DAT$LatDegree, na.rm=TRUE)
  max(DAT$LatDegree, na.rm=TRUE)
  
  min(DAT$LongDegree, na.rm=TRUE)
  max(DAT$LongDegree, na.rm=TRUE)

  max(abs(DAT$CalDepth), na.rm =TRUE)
  
  info<-c(prjt[i],)
  extent<-rbind(extent,info)

}


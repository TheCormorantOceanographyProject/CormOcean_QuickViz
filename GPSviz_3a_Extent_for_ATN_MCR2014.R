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

if(Sys.info()[7]=="alexa") {
  usrdir<-"/Users/alexa/Box Sync/DASHCAMS/"
  datadir<-'data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  #savedir<-'Research Workspace/Project Metadata/Bounding Coordinates'
  deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/alexa/git_repos/CormOcean_QuickViz/MakeDive.R')
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

# Map Check --------------------------------------------------------------------
# Pulls in saved data and plots

prjt

for (i in 1:length(prjt)){
  
  locs<-readRDS(paste0(usrdir,savedir,"Processed_Deployment_Data/",prjt[i],"_GPS_SpeedFiltered.rds"))
  
DAT<-NULL
for (j in 1:length(Files)){
  dat<-read.csv(file=Files[j],stringsAsFactors=FALSE,sep = ",",fill=TRUE) 
  #dat<-dat%>%filter(is.na(LatDegree)==FALSE)
  dat<-dat%>%select(BirdID,LatDegree,LongDegree,ObsDepth,CalDepth)
  DAT<-rbind(DAT,dat)
}
}

DAT$species<-sapply(strsplit(DAT$BirdID, split='_', fixed=TRUE), function(x) (x[1]))

min(DAT$LatDegree, na.rm=TRUE)
max(DAT$LatDegree, na.rm=TRUE)

min(DAT$LongDegree, na.rm=TRUE)
max(DAT$LongDegree, na.rm=TRUE)

max(abs(DAT$CalDepth), na.rm =TRUE)

ggplot()+
  geom_path(data=DAT%>%filter(is.na(LatDegree)==FALSE), 
            aes(x=LongDegree,y=LatDegree, group=BirdID, color=species))+
  #facet_wrap(~species)+
  NULL



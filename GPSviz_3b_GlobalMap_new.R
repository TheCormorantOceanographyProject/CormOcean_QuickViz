library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table) #rename
library(stringr)
library(R.utils)
library(tidyr)
library(argosfilter)
library(sf)
library(MetBrewer)
library(cowplot)

wrap360 = function(lon) {
  lon360<-ifelse(lon<0,lon+360,lon)
  return(lon360)
}

if(Sys.info()[7]=="rachaelorben") {
  userdir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  #source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

if(Sys.info()[7]=="Jessica") { 
  userdir<-'/Users/jessica/Library/CloudStorage/Box-Box/DASHCAMS/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'  
  #source('/Users/jessica/git_repos/CormOcean/MakeDive.R')
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

prjt<-prjt[prjt!="USACRBRDO14"]
prjt<-prjt[prjt!="LITCUGR23"] #data missing
prjt<-prjt[prjt!="SOUDIAP23"] #data missing
prjt<-prjt[prjt!="NZEHGSP923"] #data missing
prjt<-prjt[prjt!="AUSNIBF23"] #data missing

# compile tags a minute or two
locs<-NULL
for (i in 1:length(prjt)){
  locs1<-readRDS(paste0(usrdir,savedir,"Processed_Deployment_Data/",prjt[i],"_GPS_SpeedFiltered.rds"))
  names(locs1)
  locs1<-locs1%>%select(device_id,Project_ID, datetime,lat,lon)%>%ungroup()
  locs<-rbind(locs,locs1)
}

unique(locs$Project_ID)


# Maps --------------------------------------------------------------------
w2hr<-map_data('world')

locs_wgs84<-st_as_sf(locs,coords=c('lon','lat'),remove = F,crs = 4326)
dt=Sys.Date()
#quartz()
ggplot()+
  geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray60",color="grey25",size=0.1)+
  geom_sf(data = locs_wgs84, aes(color=Project_ID), size=.3)+
  scale_color_manual(values=met.brewer("Johnson", 18))+
  theme_bw()+
  theme(legend.position ="none",axis.title = element_blank())
ggsave(paste0(userdir,savedir,"WorldCormorants_",dt,".png"), dpi=300)

#the rest of the code is under constuction...
WORLD<-ggplot()+
  geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray25",color="grey60",size=0.1)+
  geom_sf(data = locs_wgs84, color="orange", size=.3)+
  scale_color_manual(values=met.brewer("Johnson", 18))+
  theme_bw()+
  theme(legend.position ="none",axis.title = element_blank())
ggsave("/Users/rachaelorben/Desktop/WorldCormorants_orange.png", dpi=300)


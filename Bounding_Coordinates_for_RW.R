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
    usrdir<-"/Users/rachaelorben/Box Sync/DASHCAMS/"
    datadir<-'data/ornitela_for_ATN/'
    savedir<-'Analysis/DataViz/'
    #savedir<-'Research Workspace/Project Metadata/Bounding Coordinates'
    deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
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
  
  
  # compile tag data - takes a minute or so
  locs<-NULL
  for (i in 1:length(prjt)){
    locs1<-readRDS(paste0(usrdir,savedir,"Processed_GPS_Deployment_Data/",prjt[i],"_GPS_SpeedFiltered.rds"))
    names(locs1)
    locs1<-locs1%>%select(device_id,Project_ID, datetime,lat,lon)%>%ungroup()
    locs<-rbind(locs,locs1)
  }
  
  locs<-left_join(locs,prj_info,by="Project_ID")
  
  #fixes species in 2019 BRAC/PECO deployment
  locs$Species[locs$Species=="PECO & BRAC"]<-"BRAC"
  locs$Species_Long[locs$Species_Long=="Pelagic Cormorant & Brandt's Cormorant"]<-"Brandt's Cormorant"
  locs$Species[locs$device_id==192761 & locs$Project_ID=="USACRBRPE19"]<-"PECO"
  locs$Species_Long[locs$device_id==192761 & locs$Project_ID=="USACRBRPE19"]<-"Pelagic Cormorant"
  locs$Species[locs$device_id==192760 & locs$Project_ID=="USACRBRPE19"]<-"PECO"
  locs$Species_Long[locs$device_id==192760 & locs$Project_ID=="USACRBRPE19"]<-"Pelagic Cormorant"
  
  unique(locs$Project_ID)
  nC<-length(unique(locs$Project_ID))
  
  locs_wgs84<-st_as_sf(locs,coords=c('lon','lat'),remove = F,crs = 4326)
  

# Min-max lat/long by project

for (i in 1:length(prjt)){
 Bound<-locs_wgs84%>%group_by(Project_ID)%>%
   summarise(MAXLAT = lat[which.max(lat)],
             MINLAT = lat[which.min(lat)],
             MAXLON = lon[which.max(lon)],
             MINLON = lon[which.min(lon)],)
  
}
  
ProjBounds<-Bound %>% st_drop_geometry() # remove the geometry column

ProjBounds2<- ProjBounds %>% rename(
    North = MAXLAT,
    South = MINLAT,
    East = MAXLON,
    West = MINLON,
  )

write.csv(ProjBounds2, paste0(usrdir,savedir,"Bounding_Coordinates_by_Project.csv"))



     
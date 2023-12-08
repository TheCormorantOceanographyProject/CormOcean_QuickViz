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
  
  #for (i in 1:length(prjt)){
  
  # locs<-readRDS(paste0(usrdir,savedir,"Processed_GPS_Deployment_Data/",prjt[i],"_GPS_SpeedFiltered.rds"))
  
  #DAT<-NULL
  #for (j in 1:length(Files)){
  # dat<-read.csv(file=Files[j],stringsAsFactors=FALSE,sep = ",",fill=TRUE) 
  #dat<-dat%>%filter(is.na(LatDegree)==FALSE)
  # dat<-dat%>%select(BirdID,LatDegree,LongDegree,ObsDepth,CalDepth)
  #DAT<-rbind(DAT,dat)
  #  }
  #}
  
  # DAT$species<-sapply(strsplit(DAT$BirdID, split='_', fixed=TRUE), function(x) (x[1]))
  
  #min(DAT$LatDegree, na.rm=TRUE)
  #max(DAT$LatDegree, na.rm=TRUE)
  
  #min(DAT$LongDegree, na.rm=TRUE)
  #max(DAT$LongDegree, na.rm=TRUE)
  
  max(abs(DAT$CalDepth), na.rm =TRUE)
  
  ggplot()+
    geom_path(data=DAT%>%filter(is.na(LatDegree)==FALSE), 
              aes(x=LongDegree,y=LatDegree, group=BirdID, color=species))+
    #facet_wrap(~species)+
    NULL
  
  
# below needs work, currently gives the lat long for all points not by project!

for (i in 1:length(prjt)){
 pS<-locs_wgs84%>%group_by(Project_ID)%>%
   summarise(MINLAT = lat[which.min(lat)],
             MAXLAT = lat[which.max(lat)],
             MINLON = lon[which.min(lon)],
             MAXLON = lon[which.max(lon)])
  
}
  

  psummarise(MINLAT=min(locs_wgs84$lat, na.rm = TRUE),
            MAXLAT=max(locs_wgs84$lat, na.rm = TRUE),
            MINLON=min(locs_wgs84$lon, na.rm = TRUE),
            MAXLON=max(locs_wgs84$lon, na.rm = TRUE))  
#Bound<-NULL    
 # for (i in 1:length(prjt)){
    #MINLAT<- min(locs_wgs84$lat, na.rm = TRUE)
    #MAXLAT<- max(locs_wgs84$lat, na.rm = TRUE)
    #MINLONG<- min(locs_wgs84$lon, na.rm = TRUE)
    #MAXLONG<- max(locs_wgs84$lon, na.rm = TRUE)
#  }  
   # Bound<-rbind(MINLAT,MAXLAT,MINLONG, MAXLONG)
  
  
     
library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table) #rename
library(stringr)
library(R.utils)
library(tidyr)
library(zoo)#dive phase
library(pracma)#matlab style gradient function
library(argosfilter)#forward-backward speed filter for GPS data
library(sf)
library(MetBrewer)

if(Sys.info()[7]=="rachaelorben") {
  usrdir<-"/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/"
  datadir<-'/data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

# Map Check --------------------------------------------------------------------
# Pulls in saved data and plots

for (i in 1:length(prjt)){
  
  locs<-readRDS(paste0(usrdir,savedir,"Processed_Deployment_Data/",prjt[i],"_GPS_SpeedFiltered.rds"))
  dm_prj<-dm%>%filter(Project_ID==prjt[i])
  
  w2hr<-map_data('world')
  
  locs_wgs84<-st_as_sf(locs,coords=c('lon','lat'),remove = F,crs = 4326)
  
  y_min<-min(locs$lat)-.25
  y_max<-max(locs$lat)+.25
  x_min<-min(locs$lon)-.25
  x_max<-max(locs$lon)+.25
  
  dt<-Sys.Date()
  ids<-unique(locs$Uni_ID)
  
  #quartz()
  ggplot()+
    geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray50",color="grey5",linewidth=0.1)+
    geom_sf(data = locs_wgs84, aes(color=Uni_ID), size=.01)+
    scale_color_manual(values=met.brewer("Tam", length(ids)))+
    coord_sf(xlim = c(x_min,x_max),ylim=c(y_min,y_max))+
    xlab("Longitude")+
    ylab("Latitude")+
    theme_bw()+
    theme(legend.title=element_blank(),
          legend.text = element_blank())+
    guides(colour = guide_legend(override.aes = list(size=3)))
  ggsave(paste0(usrdir,savedir,"PLOTS/DeploymentMaps/",prjt[i],"_",dt,"_Map.png"), dpi=300)
  
  # All Bird Data Coverage --------------------------------------------------
  locs$date<-date(locs$UTC_datetime)
  names(locs)
  dm_prj_O<-dm_prj%>%filter(TagManufacture=="Ornitela")
  dm_prj_O$start<-date(dm_prj_O$DeploymentStartDatetime)
  dm_prj_O$end<-date(dm_prj_O$DeploymentEndDatetime_UTC)
  dm_prj_O<-dm_prj_O%>%rename("device_id"="TagSerialNumber")
  dm_prj_O<-dm_prj_O%>%select(device_id,start,end)
  dm_prj<-dm_prj_O %>% 
    pivot_longer(-device_id, names_to = "d_info", values_to = "date")
  
  
  ggplot()+
    geom_point(data=locs, aes(y=as.factor(device_id),x=date), size=0.05)+
    geom_point(data=dm_prj, aes(y=as.factor(device_id),x=date,color=d_info, fill=d_info))+
    ylab("") 
  ggsave(paste0(usrdir,savedir,"PLOTS/DeploymentCoverage/",prjt[i],"_",dt,"_TimeFrame.png"), dpi=300)
  
}


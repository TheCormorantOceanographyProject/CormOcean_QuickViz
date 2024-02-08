library(ggplot2)
library(lubridate)
library(dplyr)
library(MetBrewer)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(patchwork)
library(lutz)

wrap360 = function(lon) {
  lon360<-ifelse(lon<0,lon+360,lon)
  return(lon360)
}

savedir<-'Analysis/DataViz/'
deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'

if(Sys.info()[7]=="rachaelorben") {usrdir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/'}
if(Sys.info()[7]=="alexa") {usrdir<-"/Users/alexa/Box Sync/DASHCAMS/"}
if(Sys.info()[7]=="Jessica") {usrdir<-'/Users/jessica/Library/CloudStorage/Box-Box/DASHCAMS/'}


# read in GPS data --------------------------------------------------------
locs<-readRDS(paste0(usrdir,savedir,"AllProjects_GPSonly.rda"))

locs$BirdID<-paste0(locs$Project_ID,"_",locs$device_id)

#calculates gc distance between all points
ids<-unique(locs$BirdID)
LOCS<-NULL
for (i in 1:length(ids)){
  birdy<-locs%>%filter(BirdID==ids[i])
  
  #Great Circle Distances are calculated using spherical trigonometry. See details on formulae in Zwillinger (2003).
  dis_km<-distanceTrack(birdy$lat,birdy$lon)
  birdy$dis_km<-c(NA,dis_km)
  LOCS<-rbind(LOCS,birdy)
}

#finds central place to identify tz for "local time" - 
  #potentially some issues, but as long as we split tracks during the night 
  #most daily distance traveled should be OK
prj<-unique(locs$Project_ID)
LOCS_2<-NULL
for(i in 1:length(prj)){
  birds<-LOCS%>%filter(Project_ID==prj[i])
  u_lat<-mean(birds$lat)
  u_lon<-mean(birds$lon)
  tz_str<-tz_lookup_coords(u_lat,u_lon, method = "accurate")
  birds$datetime_lc<-with_tz(time = birds$datetime,tzone = tz_str)
  LOCS_2<-rbind(LOCS_2,birds)
} 

locs<-LOCS_2
remove(LOCS,LOCS_2,birds,birdy,dis,dis_km,i,tz_str,u_lat,u_lon)

locs$date_lc<-date(locs$datetime_lc)
locs$Jdate_lc<-yday(locs$datetime_lc)

locs_dailysum<-locs%>%group_by(Project_ID,device_id,BirdID,Species_Long,date_lc,Jdate_lc)%>%
  summarize(dailyDist_km=sum(dis_km, na.rm=TRUE))

ggplot()+
  geom_point(data=locs_dailysum, 
            aes(x=Jdate_lc,y=dailyDist_km, group=BirdID, color=Project_ID))+
  facet_wrap(~Species_Long)


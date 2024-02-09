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
library(adehabitatLT) #masks dplyr select!!!

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


# do the same thing, but with interpolated data (hr) ----------------------

locs$lon360<-wrap360(locs$lon)

locs_sl<-locs %>% 
  group_by(BirdID,datetime_lc) %>% 
  slice(2)

# make ltraj (adehabitat)
tracks_lt<-adehabitatLT::as.ltraj(xy = cbind(locs_sl$lon360,locs_sl$lat),
                                  date = locs_sl$datetime_lc,
                                  id = locs_sl$BirdID,
                                  burst = locs_sl$BirdID,
                                  typeII = T,slsp="remove",infolocs = locs_sl,
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# resample at 60 minutes
tracks_lt_redis<-adehabitatLT::redisltraj(l = tracks_lt,u = 60,type = "time")
tracks.60min<-adehabitatLT::ld(tracks_lt_redis) %>% 
  mutate(device_id=as.character(burst),BirdID=(as.character(id)), datetime_lc = date)

tracks.60min$date_lc<-date(tracks.60min$datetime_lc)
tracks.60min$Jdate_lc<-yday(tracks.60min$datetime_lc)

locs_dailysum_60<-tracks.60min%>%group_by(device_id,BirdID,date_lc,Jdate_lc)%>%
  summarize(dailyDist_km=sum(dist, na.rm=TRUE))

ggplot()+
  geom_point(data=locs_dailysum_60, 
             aes(x=Jdate_lc,y=dailyDist_km, group=BirdID))

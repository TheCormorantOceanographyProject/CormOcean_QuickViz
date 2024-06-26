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
library(argosfilter)
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

#calculates gc distance between all points (takes awhile)
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
  #deployments don't cross 180 or 0 long so mean long is OK
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
remove(LOCS,LOCS_2,birds,birdy,dis_km,i,tz_str,u_lat,u_lon)

locs$date_lc<-date(locs$datetime_lc)
locs$Jdate_lc<-yday(locs$datetime_lc)

locs_dailysum<-locs%>%group_by(Project_ID,device_id,BirdID,Species_Long,date_lc,Jdate_lc)%>%
  summarize(dailyDist_km=sum(dis_km, na.rm=TRUE))

#rough plot of daily dist traveled - some data cleaning still needed (end times for Arg IMSH)
ggplot()+
  geom_point(data=locs_dailysum, 
            aes(x=Jdate_lc,y=dailyDist_km, group=BirdID, color=Project_ID))+
  facet_wrap(~Species_Long)

ggplot()+
  geom_point(data=locs_dailysum%>%
               filter(Species_Long!='Imperial Cormorant' & Species_Long!='Humboldt Penguin'), #removes some species, add more here?
             aes(x=Jdate_lc,y=dailyDist_km, group=BirdID, color=Project_ID))+
  facet_wrap(~Species_Long)+
  theme(legend.position = "none")

ggplot()+
  geom_boxplot(data=locs_dailysum%>%
               filter(Species_Long!='Imperial Cormorant' & Species_Long!='Humboldt Penguin'), #removes some species, add more here?
             aes(y=dailyDist_km, group=Species_Long, fill=Species_Long))+
  ylab("Daily Distance Traveled (km)")+
  theme_classic()+
  theme(axis.text.x = element_blank())
ggsave(paste0(usrdir,savedir,"PLOTS/Species_DailyDistanceTraveled.png"), dpi=300)


longdistDays<-locs_dailysum%>%filter(dailyDist_km>200)
write.csv(longdistDays, paste0(usrdir,savedir,"AllProjects_GPSraw_DailyDistT_longestdays.csv"))

ggplot()+
  geom_point(data=longdistDays, 
             aes(x=Jdate_lc,y=dailyDist_km, color=Species_Long))+
  ylab("Daily Distance Traveled (km)")+
  xlab("Day of Year (Julian date)")+
  theme_classic()
ggsave(paste0(usrdir,savedir,"PLOTS/Species_DailyDistanceTraveled_LongestDays.png"), dpi=300)


saveRDS(locs_dailysum, paste0(usrdir,savedir,"AllProjects_GPSraw_DailyDistT.rda"))

# does the same thing, but with interpolated data (hr): takes forever! ----------------------
locs$lon360<-wrap360(locs$lon)

# make ltraj (adehabitat)
tracks_lt<-adehabitatLT::as.ltraj(xy = cbind(locs$lon360,locs$lat),
                                  date = locs$datetime_lc,
                                  id = locs$device_id,
                                  burst = locs$BirdID,
                                  typeII = T,slsp="remove",infolocs = locs,
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# resample at 60 minutes (takes a really really long time...)
tracks_lt_redis<-adehabitatLT::redisltraj(l = tracks_lt,u = 60,type = "time")

tracks.60min<-adehabitatLT::ld(tracks_lt_redis) %>% 
  mutate(device=as.character(id),BirdID=(as.character(burst)), datetime_lc = date)
unique(tracks.60min$BirdID)

saveRDS(tracks.60min, paste0(usrdir,savedir,"AllProjects_GPSinterpolated_DailyDistT.rda"))

remove(tracks_lt_redis)

#can start here once file is made
tracks.60min<-readRDS(paste0(usrdir,savedir,"AllProjects_GPSinterpolated_DailyDistT.rda"))
names(tracks.60min)

tracks.60min$date_lc<-date(tracks.60min$date)
tracks.60min$Jdate_lc<-yday(tracks.60min$date)

locs_dailysum_60<-tracks.60min%>%group_by(device,BirdID,date_lc,Jdate_lc)%>%
  summarize(dailyDist_km=sum(dist, na.rm=TRUE))

locs_dailysum_60$Project_ID <- sapply(strsplit(locs_dailysum_60$BirdID, split='_', fixed=TRUE), function(x) (x[1]))

# Project info ------------------------------------------------------------
prj_info<-read.csv(paste0(usrdir,"/data/Field Data/Project Titles and IDs.csv"))
locs_dailysum_60<-left_join(locs_dailysum_60,prj_info,by="Project_ID")

names(locs_dailysum_60)

ggplot()+
  geom_point(data=locs_dailysum_60, 
             aes(x=Jdate_lc,y=dailyDist_km, group=Species_Long, color=Species_Long))




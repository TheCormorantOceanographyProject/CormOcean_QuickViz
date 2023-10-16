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
  usrdir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  #source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

if(Sys.info()[7]=="Jessica") { 
  usrdir<-'/Users/jessica/Library/CloudStorage/Box-Box/DASHCAMS/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'  
  #source('/Users/jessica/git_repos/CormOcean/MakeDive.R')
}

# Project info ------------------------------------------------------------
prj_info<-read.csv(paste0(usrdir,"/data/Field Data/Project Titles and IDs.csv"))

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
quartz(width=10, height=5)
ggplot()+
  geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray60",color="grey25",size=0.1)+
  geom_sf(data = locs_wgs84, aes(color=Project_ID), size=.3)+
  scale_color_manual(values=met.brewer("Johnson", 25))+
  theme_bw()+
  #theme(legend.position ="none",axis.title = element_blank())
ggsave(paste0(usrdir,savedir,"WorldCormorants_",dt,".png"), dpi=300)

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

quartz(width=12, height=5)
ggplot() +
  geom_sf(data = world) +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")+
  geom_sf(data = locs_wgs84, aes(color=Project_ID), size=.3)+
  scale_color_manual(values=met.brewer("Johnson", 25))+
  theme_bw()+
  guides(colour = guide_legend(override.aes = list(size=3)))
  #theme(legend.position ="none",axis.title = element_blank())
  


library(tidyverse)
library(ggthemes)

world_map = map_data("world") %>% 
  filter(! long > 180)

countries = world_map %>% 
  distinct(region) %>% 
  rowid_to_column()

countries %>% 
  ggplot() +
  geom_map(map = world_map,aes(fill = rowid, map_id = region)) +
  geom_sf(data = locs_wgs84, aes(color=Project_ID), size=.3)+
  scale_color_manual(values=met.brewer("Johnson", 25))+
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("moll") +
  theme_map()


#the rest of the code is under construction...
WORLD<-ggplot()+
  geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray25",color="grey60",size=0.1)+
  geom_sf(data = locs_wgs84, color="orange", size=.3)+
  scale_color_manual(values=met.brewer("Johnson", 25))+
  theme_bw()+
  theme(legend.position ="none",axis.title = element_blank())
ggsave("/Users/rachaelorben/Desktop/WorldCormorants_orange.png", dpi=300)


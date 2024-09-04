library(ggplot2)
library(lubridate)
library(dplyr)
library(MetBrewer)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(patchwork)

wrap360 = function(lon) {
  lon360<-ifelse(lon<0,lon+360,lon)
  return(lon360)
}

savedir<-'Analysis/DataViz/'
deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'

if(Sys.info()[7]=="rachaelorben") {
  usrdir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/'
}

if(Sys.info()[7]=="alexa") {
  usrdir<-"/Users/alexa/Box Sync/DASHCAMS/"
}


if(Sys.info()[7]=="Jessica") { 
  usrdir<-'/Users/jessica/Library/CloudStorage/Box-Box/DASHCAMS/'
}

# Project info ------------------------------------------------------------
prj_info<-read.csv(paste0(usrdir,"/data/Field Data/Project Titles and IDs.csv"))

#  Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
dm<-deploy_matrix%>%dplyr::select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC,TagManufacture)%>%
  filter(is.na(TagSerialNumber)==FALSE)

#all project names
prjt<-unique(deploy_matrix$Project_ID)
rm(deploy_matrix)

# Map Check --------------------------------------------------------------------
# Pulls in saved data and plots

prjt<-prjt[prjt!="USACRBRDO14"]

# compile tag data - takes a minute or so
locs<-NULL
for (i in 1:length(prjt)){
  locs1<-readRDS(paste0(usrdir,savedir,"Processed_GPS_Deployment_Data/",prjt[i],"_GPS_SpeedFiltered.rds"))
  names(locs1)
  locs1<-locs1%>%
    ungroup()%>%
    dplyr::select(device_id,Project_ID, datetime,lat,lon,gpsDiveburstID,gpsNum)
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

locs$BirdID<-paste0(locs$Project_ID,"_",locs$device_id)
dubs<-locs %>% 
  janitor::get_dupes(BirdID,datetime)
unique(dubs$BirdID)

saveRDS(locs, paste0(usrdir,savedir,"AllProjects_GPSonly.rda"))

locs_wgs84<-st_as_sf(locs,coords=c('lon','lat'),remove = F,crs = 4326)
dt=date(Sys.Date())

# Square World Map  --------------------------------------------------------------------
w2hr<-map_data('world')

ggplot()+
  geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray75",color="white",linewidth=0.1)+
  geom_sf(data = locs_wgs84, aes(color=Project_ID), size=.3)+
  #scale_color_manual(values=met.brewer("Johnson", nC))+
  scale_color_manual(values=met.brewer("Tam", nC))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  theme(panel.background = element_rect(fill="darkslategray3"),
        legend.title=element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
  #theme(legend.position ="none",axis.title = element_blank())
ggsave(paste0(usrdir,savedir,"WorldCormorants_",dt,".png"), dpi=300,width=12, height=5)


# Projection objects ------------------------------------------------------

robinson <- "+proj=robin +over"

countries <- ne_countries(scale = "medium", returnclass = "sf")
class(countries)

# create a bounding box for the robinson projection
# we'll use this as "trim" to remove jagged edges at
# end of the map (due to the curved nature of the
# robinson projection)
bb <- sf::st_union(sf::st_make_grid(
  st_bbox(c(xmin = -180,
            xmax = 180,
            ymax = 90,
            ymin = -90), crs = st_crs(4326)),
  n = 100))
bb_robinson <- st_transform(bb, as.character(robinson))

# transform the coastline to robinson
countries_robinson <- st_transform(countries, robinson)

# transform data to robinson
locs_robinson<-st_transform(locs_wgs84, robinson)

# PROJECTS: Robertson projection (natural earth version) ---------------------------------------------------

ggplot() +
  geom_sf(data=countries_robinson,
          colour='grey75',
          linetype='solid',
          fill= 'grey40',
          size=0.3) +
  geom_sf(data=bb_robinson,
          colour='black',
          linetype='solid',
          fill = NA,
          size=0.7) +
  geom_sf(data = locs_robinson, aes(color=Project_ID), size=.5)+
  #scale_color_manual(values=met.brewer("Johnson", 25))+
  scale_color_manual(values=met.brewer("Tam", (nC+10)))+
  #xlab("Longitude")+
  #ylab("Latitude")+
  labs(
    title = "Cormorant Oceanography Project",
    subtitle = "Coastal tracking data from Cormorants, Shags, & Penguins (2019-2024)",
    caption = "Data: Cormorant Oceanography Project") +
  theme_bw()+
  theme(legend.title=element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave(paste0(usrdir,savedir,"WorldCormorants_Projects_RobertsonPrj_",dt,".png"), dpi=300,width=12, height=5)

# YEARS: Robertson projection -------------------------------------------------------------------
ggplot() +
  geom_sf(data=countries_robinson,
          colour='grey75',
          linetype='solid',
          fill= 'grey40',
          size=0.3) +
  geom_sf(data=bb_robinson,
          colour='black',
          linetype='solid',
          fill = NA,
          size=0.7) +
  geom_sf(data = locs_robinson, aes(color=Project_ID), size=.5)+
  #scale_color_manual(values=met.brewer("Johnson", 25))+
  scale_color_manual(values=met.brewer("Tam", (nC+10)))+
  #xlab("Longitude")+
  #ylab("Latitude")+
  labs(
    title = "Cormorant Oceanography Project",
    subtitle = "Coastal tracking data from Cormorants, Shags, & Penguins (2019-2024)",
    caption = "Data: Cormorant Oceanography Project") +
  theme_bw()+
  theme(legend.title=element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  facet_wrap(~Year_Initated)
ggsave(paste0(usrdir,savedir,"WorldCormorants_Projects_by_Years_RobertsonPrj_",dt,".png"), dpi=300,width=12, height=5)

# SPECIES: Robertson projection (natural earth version) ---------------------------------------------------

nS<-length(unique(locs$Species))

ggplot() +
  geom_sf(data=countries_robinson,
          colour='grey75',
          linetype='solid',
          fill= 'grey40',
          size=0.3) +
  geom_sf(data=bb_robinson,
          colour='black',
          linetype='solid',
          fill = NA,
          size=0.7) +
  geom_sf(data = locs_robinson, aes(color=Species_Long), size=.5)+
  #scale_color_manual(values=met.brewer("Johnson", 25))+
  scale_color_manual(values=met.brewer("Tam", nS))+
  #xlab("Longitude")+
  #ylab("Latitude")+
  labs(
    title = "Cormorant Oceanography Project",
    subtitle = "Coastal tracking data from Cormorants, Shags, & Penguins (2019-2024)",
    caption = "Data: Cormorant Oceanography Project") +
  theme_bw()+
  theme(legend.title=element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave(paste0(usrdir,savedir,"WorldCormorants_Species_RobertsonPrj_",dt,".png"), dpi=300,width=12, height=5)


# COUNTRIES: Robertson projection (natural earth version) ---------------------------------------------------

nS<-length(unique(locs$Country))

ggplot() +
  geom_sf(data=countries_robinson,
          colour='grey75',
          linetype='solid',
          fill= 'grey40',
          size=0.3) +
  geom_sf(data=bb_robinson,
          colour='black',
          linetype='solid',
          fill = NA,
          size=0.7) +
  geom_sf(data = locs_robinson, aes(color=Country), size=.5)+
  #scale_color_manual(values=met.brewer("Johnson", 25))+
  scale_color_manual(values=met.brewer("Tam", nS))+
  #xlab("Longitude")+
  #ylab("Latitude")+
  labs(
    title = "Cormorant Oceanography Project",
    subtitle = "Coastal tracking data from Cormorants, Shags, & Penguins (2019-2024)",
    caption = "Data: Cormorant Oceanography Project") +
  theme_bw()+
  theme(legend.title=element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave(paste0(usrdir,savedir,"WorldCormorants_byCountry_RobertsonPrj_",dt,".png"), dpi=300,width=12, height=5)



# Federal Fiscal Year (Oct 22- Sept 23) - Project: Robertson projection (natural earth version) ---------------------------------------------------

# transform the data to robinson
countries_robinson <- st_transform(countries, robinson)
locs_robinson<-st_transform(locs_wgs84, robinson)
locs_robinson_ft<-locs_robinson%>%filter(datetime>"2022-09-30 00:00" & datetime<"2023-10-01 00:00")
locs_robinson_ft%>%filter(Project_ID=="USACRBRPE19")

nS<-length(unique(locs_robinson_ft$Project_ID))

ggplot() +
  geom_sf(data=countries_robinson,
          colour='grey75',
          linetype='solid',
          fill= 'grey40',
          size=0.3) +
  geom_sf(data=bb_robinson,
          colour='black',
          linetype='solid',
          fill = NA,
          size=0.7) +
  geom_sf(data = locs_robinson_ft, aes(color=Project_ID), size=.5)+
  #scale_color_manual(values=met.brewer("Johnson", 25))+
  scale_color_manual(values=met.brewer("Tam", nS))+
  #xlab("Longitude")+
  #ylab("Latitude")+
  labs(
    title = "Cormorant Oceanography Project",
    subtitle = "Coastal tracking data from Cormorants, Shags, & Penguins (Oct 2022 -Sept 2023)",
    caption = "Data: Cormorant Oceanography Project") +
  theme_bw()+
  theme(legend.title=element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave(paste0(usrdir,savedir,"WorldCormorants_FY2223_",dt,".png"), dpi=300,width=12, height=5)

# FFY (Oct 22- Sept 23) - Countries: Robertson projection (natural earth version) ---------------------------------------------------


# transform the coastline to robinson
countries_robinson <- st_transform(countries, robinson)
locs_robinson<-st_transform(locs_wgs84, robinson)
locs_robinson_ft<-locs_robinson%>%filter(datetime>"2022-09-30 00:00" & datetime<"2023-10-01 00:00")
locs_robinson_ft%>%filter(Project_ID=="USACRBRPE19")

nS<-length(unique(locs_robinson_ft$Project_ID))

ggplot() +
  geom_sf(data=countries_robinson,
          colour='grey75',
          linetype='solid',
          fill= 'grey40',
          size=0.3) +
  geom_sf(data=bb_robinson,
          colour='black',
          linetype='solid',
          fill = NA,
          size=0.7) +
  geom_sf(data = locs_robinson_ft, aes(color=Country), size=.5)+
  #scale_color_manual(values=met.brewer("Johnson", 25))+
  scale_color_manual(values=met.brewer("Tam", nS))+
  #xlab("Longitude")+
  #ylab("Latitude")+
  labs(
    title = "Cormorant Oceanography Project",
    subtitle = "Coastal tracking data from Cormorants, Shags, & Penguins (Oct 2022 -Sept 2023)",
    caption = "Data: Cormorant Oceanography Project") +
  theme_bw()+
  theme(legend.title=element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave(paste0(usrdir,savedir,"WorldCormorants_FY2223_byCountry_",dt,".png"), dpi=300,width=12, height=5)






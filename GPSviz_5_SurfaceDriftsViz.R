library(ggplot2)
library(lubridate)
library(dplyr)
library(MetBrewer)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(patchwork)
library(cowplot)

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

locs<-readRDS(paste0(usrdir,savedir,"AllProjects_GPSonly.rda"))

head(locs)
unique(locs$gpsDiveburstID)

locs_d<-locs%>%filter(gpsNum>4)%>%
  group_by(Project_ID,device_id,gpsDiveburstID)%>%
  slice_max(gpsNum)%>%filter(gpsNum<17) #removes long surface GPS bursts from Bahrain deployment in 2021


proj_surSum<-locs_d%>%group_by(Project_ID)%>%
  summarise(n=n())%>%filter(n>20) #removes a few records from projects with <21 surface drift meas.

w2hr<-map_data('world')

for (i in 1:nrow(proj_surSum)){
  prj<-proj_surSum$Project_ID[i]
  samples<-proj_surSum$n[proj_surSum$Project_ID==prj]
  
  locs<-locs_d%>%filter(Project_ID==prj)
  y_min<-min(locs$lat)-.4
  y_max<-max(locs$lat)+.4
  
  x_min<-min(locs$lon)-.5
  x_max<-max(locs$lon)+.5
  
A<-ggplot()+
  geom_histogram(data=locs_d%>%filter(Project_ID==prj), 
                 aes(x=gpsNum, group=device_id, fill=as.factor(device_id)), binwidth=1)+
  scale_x_continuous(breaks = seq(0, 17, 1)) +
  xlim(0,20)+
  labs(fill = "Birds")

B<-ggplot()+
  geom_polygon(data=w2hr,aes(long,lat,group=group),fill="grey70",color="grey60",linewidth=0.1)+
  geom_point(data=locs_d%>%filter(Project_ID==prj),aes(x=lon, y=lat, color=gpsNum))+
  labs(color = paste0("GPS points n = ",samples)) +
  xlab("Longitude")+
  ylab("Latitude")+
  coord_fixed(ratio=1.7,xlim = c(x_min,x_max),ylim=c(y_min,y_max))+
  theme_bw()

locs_d$date<-date(locs_d$datetime)

C<-ggplot()+
  geom_histogram(data=locs_d%>%filter(Project_ID==prj), 
                 aes(x=date, group=device_id, fill=as.factor(device_id)), binwidth=1)+
  labs(fill = "Birds")+
  xlab("")

P1<-plot_grid(A,B,C)
save_plot(paste0(usrdir,savedir,"PLOTS/SurfaceDrifts/", prj, "_SufaceDrifts.pdf"), 
          P1, ncol = 2,  nrow=2, base_asp = 1.1, base_width=8, base=6)

}

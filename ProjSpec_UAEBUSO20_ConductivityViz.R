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
  datadir<-'data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

prjt<-c("UAEBUSO20")
dt<-Sys.Date()

#  Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
deploy_matrix<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC)%>%
  filter(is.na(TagSerialNumber)==FALSE)

IDx<-which(deploy_matrix$Project_ID %in% prjt)
dm<-deploy_matrix[IDx,] #selects the file names with IDs that are currently deployed

list.files(paste0(usrdir,savedir,"Processed_Dive_Deployment_Data"),pattern=prjt)

# Find Project Dive Data Files -------------------------------------------------


birds<-NULL
for (i in 1:length(prjt)){
  
  Files.fu<-list.files(paste0(usrdir,savedir,"Processed_Dive_Deployment_Data"),pattern=prjt[i], full.names=TRUE)
  files.sh<-list.files(paste0(usrdir,savedir,"Processed_Dive_Deployment_Data"),pattern=prjt[i])
  
  # Loads Data ------------------------------------------------

  for (j in 1:length(Files.fu)){
    
    tagID<-sapply(strsplit(files.sh[j], split='_', fixed=TRUE), function(x) (x[1]))
    deply_sel<-dm[dm$TagSerialNumber==tagID[1],]
    
    dat <- readRDS(Files.fu[j])
    birds<-rbind(birds,dat)
  }
 }

# Find Project GPS Data Files -------------------------------------------------

birds_gps<-NULL
for (i in 1:length(prjt)){
  
  Files.fu<-list.files(paste0(usrdir,savedir,"Processed_GPS_Deployment_Data"),pattern=prjt[i], full.names=TRUE)
  files.sh<-list.files(paste0(usrdir,savedir,"Processed_GPS_Deployment_Data"),pattern=prjt[i])
  
  # Loads Data ------------------------------------------------

  for (j in 1:length(  Files.fu)){
    
    tagID<-sapply(strsplit(files.sh[j], split='_', fixed=TRUE), function(x) (x[1]))
    deply_sel<-dm[dm$TagSerialNumber==tagID[1],]
    
    dat <- readRDS(Files.fu[j])
    birds_gps<-rbind(birds_gps,dat)
  }
}

# Conductivity Plots ------------------------------------------------------
names(birds)
str(birds)
birds1<-birds%>%filter(conductivity_mS.cm<1000 & conductivity_mS.cm>0)
#quick outlier removal
birds_most<-birds1%>%filter(device_id!=201411 & device_id!=203013 & device_id!=203014)
birds_201411<-birds1%>%filter(device_id==201411)%>%filter(conductivity_mS.cm<200)
birds_203013<-birds1%>%filter(device_id==203013)%>%filter(conductivity_mS.cm<400)
birds_203014<-birds1%>%filter(device_id==203014)%>%filter(conductivity_mS.cm<300)
birdsc<-rbind(birds_most,birds_201411,birds_203013,birds_203014)
birdsc$ext_temperature_C[birdsc$device_id==203029]<-NA
birdsc$ext_temperature_C[birdsc$ext_temperature_C>60]<-NA


ggplot()+
  geom_point(data=birdsc,
             aes(x=datetime,y=conductivity_mS.cm, color=conductivity_mS.cm))+
  scale_color_gradientn(colors = met.brewer("Archambault")) +
  facet_wrap(~device_id, scales="free")
ggsave(paste0(usrdir,savedir,"PLOTS/",prjt[i],"_",dt,"_ConductivitySensorData_byTagID_timeseries.png"), dpi=300, width=12, height=8)

quartz()
ggplot()+
  geom_point(data=birdsc,aes(x=datetime,y=ext_temperature_C, color=ext_temperature_C))+
  scale_color_gradientn(colors = met.brewer("Archambault")) +
  facet_wrap(~device_id, scales="free")
ggsave(paste0(usrdir,savedir,"PLOTS/",prjt[i],"_",dt,"_ConductivitySensorData_byTagID_timeseries_colTemp.png"), dpi=300, width=12, height=8)


quartz()
ggplot()+
  geom_point(data=birdsc,aes(x=datetime,y=conductivity_mS.cm, color=ext_temperature_C))+
  scale_color_gradientn(colors = met.brewer("Archambault")) +
  facet_wrap(~device_id, scales="free")
ggsave(paste0(usrdir,savedir,"PLOTS/",prjt[i],"_",dt,"_ConductivitySensorData_byTagID_timeseries_colTemp.png"), dpi=300, width=12, height=8)

ggplot()+
  geom_point(data=birds,aes(x=datetime,y=conductivity_mS.cm, color=depth_m))+
  scale_color_gradientn(colors = met.brewer("Archambault")) +
  facet_wrap(~device_id, scales="free")
ggsave(paste0(usrdir,savedir,"PLOTS/",prjt[i],"_",dt,"_ConductivitySensorData_byTagID_timeseries_colDepth.png"), dpi=300, width=12, height=8)

ggplot()+
  geom_point(data=birds%>%filter(depth_m<5),aes(x=datetime,y=conductivity_mS.cm, color=depth_m))+
  scale_color_gradientn(colors = met.brewer("Archambault")) +
  facet_wrap(~device_id, scales="free")
ggsave(paste0(usrdir,savedir,"PLOTS/",prjt[i],"_",dt,"_ConductivitySensorData_byTagID_timeseries_colDepth<5.png"), dpi=300, width=12, height=8)

ggplot()+
  geom_point(data=birds%>%filter(depth_m>5 & depth_m<10),aes(x=datetime,y=conductivity_mS.cm, color=depth_m))+
  scale_color_gradientn(colors = met.brewer("Archambault")) +
  facet_wrap(~device_id, scales="free")
ggsave(paste0(usrdir,savedir,"PLOTS/",prjt[i],"_",dt,"_ConductivitySensorData_byTagID_timeseries_colDepth_5-10m.png"), dpi=300, width=12, height=8)

ggplot()+
  geom_point(data=birds%>%filter(depth_m>10 & depth_m<20),aes(x=datetime,y=conductivity_mS.cm, color=depth_m))+
  scale_color_gradientn(colors = met.brewer("Archambault")) +
  facet_wrap(~device_id, scales="free")
ggsave(paste0(usrdir,savedir,"PLOTS/",prjt[i],"_",dt,"_ConductivitySensorData_byTagID_timeseries_colDepth_10-20m.png"), dpi=300, width=12, height=8)

ggplot()+
  geom_point(data=birds%>%filter(depth_m>20 & depth_m<30),aes(x=datetime,y=conductivity_mS.cm, color=depth_m))+
  scale_color_gradientn(colors = met.brewer("Archambault")) +
  facet_wrap(~device_id, scales="free")
ggsave(paste0(usrdir,savedir,"PLOTS/",prjt[i],"_",dt,"_ConductivitySensorData_byTagID_timeseries_colDepth_20-30m.png"), dpi=300, width=12, height=8)

ggplot()+
  geom_point(data=birds%>%filter(depth_m>30 ),aes(x=datetime,y=conductivity_mS.cm, color=depth_m))+
  scale_color_gradientn(colors = met.brewer("Archambault")) +
  facet_wrap(~device_id, scales="free")
ggsave(paste0(usrdir,savedir,"PLOTS/",prjt[i],"_",dt,"_ConductivitySensorData_byTagID_timeseries_colDepth_>30m.png"), dpi=300, width=12, height=8)


ggplot()+
  geom_histogram(data=birds,aes(y=conductivity_mS.cm))+
  facet_wrap(~device_id, scales="free")
ggsave(paste0(usrdir,savedir,"PLOTS/",prjt[i],"_",dt,"_ConductivitySensorData_byTagID_histogram.png"), dpi=300, width=12, height=8)

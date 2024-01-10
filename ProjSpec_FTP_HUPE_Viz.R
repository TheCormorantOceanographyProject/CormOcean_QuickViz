library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table) #rename
library(stringr)
library(R.utils)
library(tidyr)
library(MetBrewer)

#if(require("argosfilter")==FALSE) install.packages("argosfilter")
#library(argosfilter)

# Link to local Box Sync folder ---- 
#To find user/computer specific username use: Sys.getenv("LOGNAME")


if(Sys.info()[7]=="rachaelorben") {
  datadir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/data/ornitela_ftp_data/'
  usrdir<-"/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/"
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

#  Pulls in deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(deplymatrix)
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
deploy_matrix<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC)%>%
  filter(is.na(TagSerialNumber)==FALSE)

dm<-deploy_matrix%>%filter(Project_ID=="PERPSJHU23")

tag_ids<-dm$TagSerialNumber
Files<-list.files(datadir,pattern = ".csv",full.names = FALSE)

my_files <- fileSnapshot(path=datadir)
Files1<-rownames(my_files$info[1])[which(my_files$info[1] > 309)] #selects files with >309 bytes (1 header row)

Tags <- sapply(strsplit(Files, split='_', fixed=TRUE), function(x) (x[1]))

sel_files<-Files1[Tags %in%  tag_ids]

# Cycles through selected data files ----------------------------------------------
Birds<-NULL
for (i in 1:length(sel_files)){
  
  fileN<-sel_files[i]
  tagID<-sapply(strsplit(sel_files[i], split='_', fixed=TRUE), function(x) (x[1]))
  
  dat<-read.csv(file = paste0(datadir,sel_files[i]),sep = ",") #could switch to fread to be quicker...
  
  dat<-rename(dat,lat="Latitude")
  dat<-rename(dat,lon="Longitude")
  #dat<-rename(dat,UTC_timestamp="UTC_datetime")
  dat[is.na(dat)==TRUE]<-NA
  
  dat$datetime<-ymd_hms(dat$UTC_timestamp)
  dat$Foid<-1:nrow(dat)
  Birds<-rbind(Birds,dat)
}


Birds[is.na(Birds)==TRUE]<-NA
#Bird$depth_m[Bird$lat<0]<-0 #only when there is a non-zero location

Birds$lat[Birds$lat>=0]<-NA
Birds$lon[Birds$lon>=0]<-NA

# identify dives ----------------------------------------------------------
Birds_dpth<-Birds%>%filter(is.na(depth_m)==FALSE)
Birds_dpth$tdiff_sec <-difftime(Birds_dpth$datetime, lag(Birds_dpth$datetime, 1),units = "secs")

names(Birds_dpth)
id_num <- which(colnames(Birds_dpth) == "device_id") 
dt_num <- which(colnames(Birds_dpth) == "datetime") 
dp_num <- which(colnames(Birds_dpth) == "depth_m") 
td_num <- which(colnames(Birds_dpth) == "tdiff_sec") 

Birds_dpth<-MakeDive(Birds_dpth,idCol=id_num, #column index with unique ID
                     dtCol=dt_num, #column index with datetime
                     depthCol=dp_num, #column index with depth
                     tdiffCol=td_num, #column index with time difference in seconds
                     DepthCutOff=1, #depth that dives happen below (meters)
                     DiveDepthYes=2, #dives need to reach 3 meters to be considered a dive event
                     TimeDiffAllowed_sec=2, #consecutive points need to have a time difference <2 to be in the same event
                     NumLocCut=3) #dives need to contain three points to be considered a dive, could change this to a duration

Birds_dpth$date<-date(Birds_dpth$datetime)
Birds_dpth<-rename(Birds_dpth, "dive_id" = "divedatYN")
#write.csv(Birds_dpth,paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_DiveAnnotated.csv"))


B_sum<-Birds_dpth%>%group_by(ID)%>%
  summarise(minDt=min(datetime),
            maxDt=max(datetime),
            maxDepth=max(depth,na.rm=TRUE),
            n=n(),
            uDepth=round(mean(depth,na.rm=TRUE),2),
            sdDepth=round(sd(depth,na.rm=TRUE),2))%>%
  mutate(dur=round(maxDt-minDt,2)) 
#write.csv(B_sum,paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_SumBird.csv"))

daily_sum<-Birds_dpth%>%group_by(ID, date)%>%
  summarise(minDt=min(datetime),
            maxDt=max(datetime),
            maxDepth=max(depth,na.rm=TRUE),
            nDives=n_distinct(dive_id),
            uDepth=round(mean(depth,na.rm=TRUE),2),
            sdDepth=round(sd(depth,na.rm=TRUE),2))
#write.csv(daily_sum,paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_SumByDay.csv"))

dive_sum<-Birds_dpth%>%group_by(ID, date,dive_id)%>%
  summarise(minDt=min(datetime),
            maxDt=max(datetime),
            maxDepth=max(depth,na.rm=TRUE),
            nDives=n_distinct(dive_id),
            uDepth=round(mean(depth,na.rm=TRUE),2),
            sdDepth=round(sd(depth,na.rm=TRUE),2))%>%
  mutate(Div_dur=round(maxDt-minDt,2)) 
dive_sum<-dive_sum%>%filter(is.na(dive_id)==FALSE)
#write.csv(dive_sum,paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_SumByDive.csv"))


ggplot()+
  geom_path(data=Birds%>%filter(lon>-100)%>%filter(lat<(-14)), 
            aes(x=lon,y=lat, group=as.factor(device_id), color=as.factor(device_id)))
ggsave(path = paste0(usrdir,savedir,"PLOTS/HUPE/"), filename = "Map_Dec4_2023.png",dpi=300)

ggplot()+
  geom_path(data=Birds%>%filter(lon>-100)%>%filter(lat<(-14)), 
            aes(x=lon,y=lat, group=as.factor(device_id), color=as.factor(device_id)))+
  facet_wrap(~device_id, scales="free")
ggsave(path = paste0(usrdir,savedir,"PLOTS/HUPE/"), filename = "Map_Dec4_2023_byBird.png",dpi=300)

Birds$ext_temperature_C[Birds$ext_temperature_C>40]<-NA
Birds$ext_temperature_C[Birds$ext_temperature_C<3]<-NA
ggplot()+
  geom_point(data=Birds%>%filter(datetime>"2023-11-18 00:00:00")%>%filter(depth_m<100), 
            aes(x=datetime,y=(-depth_m), group=as.factor(device_id), color=ext_temperature_C), size=.03)
ggsave(path = paste0(usrdir,savedir,"PLOTS/HUPE/"), filename = "Dives_Dec4_2023.png",dpi=300)

ggplot()+
  geom_point(data=Birds%>%filter(datetime>"2023-11-18 00:00:00")%>%filter(depth_m<100), 
             aes(x=datetime,y=(-depth_m), group=as.factor(device_id), color=ext_temperature_C), size=.03)+
  facet_wrap(~device_id, nrow=8)
ggsave(path = paste0(usrdir,savedir,"PLOTS/HUPE/"), filename = "Dives_Dec4_2023_byBird.png",dpi=300)


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
  datadir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/data/ornitela_ftp_data'
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

# Find files with specific logger ID
Files2<-list.files(datadir, pattern="224983", full.names = TRUE)
filenames<-list.files(datadir, pattern="224983", full.names = TRUE)

# Find file names with data -------------------------------------------
my_files <- fileSnapshot(path=datadir)
Files1<-rownames(my_files$info[1])[which(my_files$info[1] < 309)] #selects files with >309 bytes (1 header row)

Files<-Files2[Files2 %in% Files1 == FALSE] #I think this should remove empty files written within last two weeks


# Cycles through selected data files ----------------------------------------------
Bird<-NULL 
for (i in 1:length(Files)){
  
  dat<-read.csv(file = (Files[i])) #could switch to fread to be quicker...
  dat$Foid<-1:nrow(dat)
  Bird<-rbind(Bird,dat)
  
}  
  
Bird<-rename(Bird,lat="Latitude")
Bird<-rename(Bird,lon="Longitude")
Bird[is.na(Bird)==TRUE]<-NA
Bird$datetime<-ymd_hms(Bird$UTC_timestamp)
Bird$datetimeNZST<-with_tz(Bird$datetime,"NZ")

Bird$depth_m[Bird$lat<0]<-0 #only when there is a non-zero location

write.csv(Bird,paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_DeploymentData.csv"))


# identify dives ----------------------------------------------------------
Birds_dpth<-Bird%>%filter(is.na(depth_m)==FALSE)
Birds_dpth$tdiff_sec <-difftime(Birds_dpth$datetimeNZST, lag(Birds_dpth$datetimeNZST, 1),units = "secs")

names(Birds_dpth)
id_num <- which(colnames(Birds_dpth) == "device_id") 
dt_num <- which(colnames(Birds_dpth) == "datetimeNZST") 
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
write.csv(Birds_dpth,paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_DiveAnnotated.csv"))


B_sum<-Birds_dpth%>%group_by(ID)%>%
  summarise(minDt_NZST=min(datetime),
            maxDt_NZST=max(datetime),
            maxDepth=max(depth,na.rm=TRUE),
            n=n(),
            uDepth=round(mean(depth,na.rm=TRUE),2),
            sdDepth=round(sd(depth,na.rm=TRUE),2))%>%
  mutate(dur=round(maxDt_NZST-minDt_NZST,2)) 
write.csv(B_sum,paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_SumBird.csv"))

daily_sum<-Birds_dpth%>%group_by(ID, date)%>%
  summarise(minDt_NZST=min(datetime),
            maxDt_NZST=max(datetime),
            maxDepth=max(depth,na.rm=TRUE),
            nDives=n_distinct(dive_id),
            uDepth=round(mean(depth,na.rm=TRUE),2),
            sdDepth=round(sd(depth,na.rm=TRUE),2))
write.csv(daily_sum,paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_SumByDay.csv"))

dive_sum<-Birds_dpth%>%group_by(ID, date,dive_id)%>%
  summarise(minDt_NZST=min(datetime),
            maxDt_NZST=max(datetime),
            maxDepth=max(depth,na.rm=TRUE),
            nDives=n_distinct(dive_id),
            uDepth=round(mean(depth,na.rm=TRUE),2),
            sdDepth=round(sd(depth,na.rm=TRUE),2))%>%
  mutate(Div_dur=round(maxDt_NZST-minDt_NZST,2)) 
dive_sum<-dive_sum%>%filter(is.na(dive_id)==FALSE)
write.csv(dive_sum,paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_SumByDive.csv"))


ggplot()+
  geom_point(data=dive_sum, aes(y=Div_dur/60,x=date, color=maxDepth))+
  labs(title="Spotted Shag: 224983")+
  ylab("Dive Duration (min)")+
  xlab("Full Tagging Record (NZST)")+
  #theme(legend.title=element_blank())+
  #guides(colour = guide_legend(override.aes = list(size=3)))+
  NULL
ggsave(paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_DiveDuration_date.png"), dpi=300)

ggplot()+
  geom_point(data=dive_sum, aes(y=Div_dur/60,x=minDt_NZST, color=maxDepth))+
  labs(title="Spotted Shag: 224983")+
  ylab("Dive Duration (min)")+
  xlab("Full Tagging Record (NZST)")+
  #theme(legend.title=element_blank())+
  #guides(colour = guide_legend(override.aes = list(size=3)))+
  NULL
ggsave(paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_DiveDuration_datetime.png"), dpi=300)

event<-Birds_dpth%>%filter(datetime > ymd_hms("2023-10-18 22:15:00")&  datetime < ymd_hms("2023-10-18 22:40:00"))
event_zm<-Birds_dpth%>%filter(datetime > ymd_hms("2023-10-18 22:28:00") &  datetime < ymd_hms("2023-10-18 22:35:00"))
event_zm_zm<-Birds_dpth%>%filter(datetime > ymd_hms("2023-10-18 22:28:30") &  datetime < ymd_hms("2023-10-18 22:35:00"))
event_zm_before<-Birds_dpth%>%filter(datetime > ymd_hms("2023-10-18 22:25:00") &  datetime < ymd_hms("2023-10-18 22:29:30"))
day_event<-Birds_dpth%>%filter(datetime > ymd_hms("2023-10-12 00:00:00")&  datetime < ymd_hms("2023-10-19 4:40:00"))

ggplot()+
  geom_point(data=day_event%>%filter(is.na(dive_id)==FALSE),
             aes(x=datetime,y=-depth, color=as.factor(dive_id)))+
  geom_point(data=day_event%>%filter(depth==0),
             aes(x=datetime,y=-depth), color="black")+
  geom_path(data=event,
             aes(x=datetime,y=-depth, group=as.factor(dive_id), color=as.factor(dive_id)))+
  scale_color_manual(values=met.brewer("Tam", 1781))+
  labs(title="Spotted Shag: 224983")+
  ylab("Depth (m)")+
  xlab("Diving Activity the Previous Week (NZST)")+
  theme(legend.position="none")+
  NULL
ggsave(paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_DiveDepth_WeekBefore.png"), dpi=300)

ggplot()+
  geom_point(data=event%>%filter(is.na(dive_id)==FALSE),
             aes(x=datetime,y=-depth, color=as.factor(dive_id)))+
  geom_point(data=event%>%filter(depth==0),
             aes(x=datetime,y=-depth), color="black")+
  geom_path(data=event%>%filter(is.na(dive_id)==FALSE),
            aes(x=datetime,y=-depth, group=as.factor(dive_id), color=as.factor(dive_id)))+
  scale_color_manual(values=met.brewer("Tam", 27))+
  labs(title="Spotted Shag: 224983")+
  ylab("Depth (m)")+
  xlab("October 19, 2023 (NZST)")+
  theme(legend.position="none")+
  NULL
ggsave(paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_DiveDepth_Oct192023event.png"), dpi=300)

ggplot()+
  geom_point(data=event_zm,
             aes(x=datetime,y=-depth, color=as.factor(dive_id)))+
  geom_point(data=event_zm%>%filter(depth==0),
             aes(x=datetime,y=-depth), color="black")+
  geom_path(data=event_zm,
            aes(x=datetime,y=-depth),linetype=2)+
  scale_color_manual(values=met.brewer("Tam", 27,direction = -1))+
  labs(title="Spotted Shag: 224983")+
  ylab("Depth (m)")+
  xlab("October 19, 2023 (NZST)")+
  theme(legend.position="none")+
  NULL
ggsave(paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_DiveDepth_Oct192023eventZoom.png"), dpi=300)

ggplot()+
  geom_point(data=event_zm_before,
             aes(x=datetime,y=-depth, color=as.factor(dive_id)))+
  geom_point(data=event_zm_before%>%filter(depth==0),
             aes(x=datetime,y=-depth), color="black")+
  geom_path(data=event_zm_before,
            aes(x=datetime,y=-depth),linetype=2)+
  scale_color_manual(values=met.brewer("Tam", 27,direction = -1))+
  labs(title="Spotted Shag: 224983")+
  ylab("Depth (m)")+
  xlab("October 19, 2023 (NZST)")+
  theme(legend.position="none")+
  NULL
ggsave(paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_DiveDepth_Oct192023eventZoomBefore.png"), dpi=300)

ggplot()+
  geom_point(data=event_zm_zm,
             aes(x=datetime,y=-depth, color=as.factor(dive_id)))+
  geom_point(data=event_zm_zm%>%filter(depth==0),
             aes(x=datetime,y=-depth), color="black")+
  geom_path(data=event_zm_zm,
            aes(x=datetime,y=-depth),linetype=2)+
  scale_color_manual(values=met.brewer("Tam", 27,direction = -1))+
  labs(title="Spotted Shag: 224983")+
  ylab("Depth (m)")+
  xlab("October 19, 2023 (NZST)")+
  theme(legend.position="none")+
  NULL
ggsave(paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_DiveDepth_Oct192023eventZoom_fromlastSurface.png"), dpi=300, width=12,height=5)



# map ---------------------------------------------------------------------
library(ggmap)
library(osmdata)
library(ggpubr)

locs<-Bird%>%filter(is.na(lat)==FALSE)%>%filter(UTC_date>"2023-08-22")%>%filter(lat!=0)
y_min<-min(locs$lat)-.4
y_max<-max(locs$lat)+.4

x_min<-min(locs$lon)-.5
x_max<-max(locs$lon)+.5

(map <- get_map(c(left = x_min, bottom = y_min, right = x_max, top = y_max),source="stamen"))

pl<-
  #geom_polygon(data=w2hr,aes(long,lat,group=group),fill="grey70",color="grey60",linewidth=0.1)+
  ggmap(map)+
  geom_path(data=locs,aes(x=lon,y=lat, group=device_id))+
  geom_point(data=locs,aes(x=lon,y=lat, color=device_id), size=.5)+
  xlab("Longitude")+
  ylab("Latitude")+
  coord_fixed(ratio=1.7,xlim = c(x_min,x_max),ylim=c(y_min,y_max))+
  #labs(title=tit)+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave(pl,path = paste0(usrdir,savedir,"PLOTS/NZ_FisheriesMortality/NZEHGSP923_22498_mortality_Map.png"), dpi=300)

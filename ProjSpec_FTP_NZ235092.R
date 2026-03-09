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



  usrdir<-"/Users/alexa/Desktop/235092/"
  datadir<-'/Users/alexa/Desktop/235092/ornitela_ftp_data'
  savedir<-'/Users/alexa/Desktop/235092/DataViz/'
  deplymatrix<-'/Users/alexa/Desktop/235092/Deployment_Field_Data.csv'
  source('/Users/alexa/git_repos/CormOcean_QuickViz/MakeDive.R')




#  Pulls in deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(deplymatrix)
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
deploy_matrix<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC)%>%
  filter(is.na(TagSerialNumber)==FALSE)

# Find files with specific logger ID
Files2<-list.files(datadir, pattern="235092", full.names = TRUE)
filenames<-list.files(datadir, pattern="235092", full.names = TRUE)

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

write.csv(Bird,paste0(savedir,"NZEHGSP23_235092_mortality_DeploymentData.csv"))


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
write.csv(Birds_dpth,paste0(savedir,"NZEHGSP23_235092_mortality_DiveAnnotated.csv"))


B_sum<-Birds_dpth%>%group_by(ID)%>%
  summarise(minDt_NZST=min(datetime),
            maxDt_NZST=max(datetime),
            maxDepth=max(depth,na.rm=TRUE),
            n=n(),
            uDepth=round(mean(depth,na.rm=TRUE),2),
            sdDepth=round(sd(depth,na.rm=TRUE),2))%>%
  mutate(dur=round(maxDt_NZST-minDt_NZST,2)) 
write.csv(B_sum,paste0(savedir,"NZEHGSP23_235092_mortality_SumBird.csv"))

daily_sum<-Birds_dpth%>%group_by(ID, date)%>%
  summarise(minDt_NZST=min(datetime),
            maxDt_NZST=max(datetime),
            maxDepth=max(depth,na.rm=TRUE),
            nDives=n_distinct(dive_id),
            uDepth=round(mean(depth,na.rm=TRUE),2),
            sdDepth=round(sd(depth,na.rm=TRUE),2))
write.csv(daily_sum,paste0(savedir,"NZEHGSP23_235092_mortality_SumByDay.csv"))

dive_sum<-Birds_dpth%>%group_by(ID, date,dive_id)%>%
  summarise(minDt_NZST=min(datetime),
            maxDt_NZST=max(datetime),
            maxDepth=max(depth,na.rm=TRUE),
            nDives=n_distinct(dive_id),
            uDepth=round(mean(depth,na.rm=TRUE),2),
            sdDepth=round(sd(depth,na.rm=TRUE),2))%>%
  mutate(Div_dur=round(maxDt_NZST-minDt_NZST,2)) 
dive_sum<-dive_sum%>%filter(is.na(dive_id)==FALSE)
write.csv(dive_sum,paste0(savedir,"NZEHGSP23_235092_mortality_SumByDive.csv"))


ggplot()+
  geom_point(data=dive_sum, aes(y=Div_dur/60,x=date, color=maxDepth))+
  labs(title="Spotted Shag: 235092")+
  ylab("Dive Duration (min)")+
  xlab("Full Tagging Record (NZST)")+
  #theme(legend.title=element_blank())+
  #guides(colour = guide_legend(override.aes = list(size=3)))+
  NULL
ggsave(paste0(savedir,"NZEHGSP23_235092_mortality_DiveDuration_date.png"), dpi=300)

ggplot()+
  geom_point(data=dive_sum, aes(y=Div_dur/60,x=minDt_NZST, color=maxDepth))+
  labs(title="Spotted Shag: 235092")+
  ylab("Dive Duration (min)")+
  xlab("Full Tagging Record (NZST)")+
  #theme(legend.title=element_blank())+
  #guides(colour = guide_legend(override.aes = list(size=3)))+
  NULL
ggsave(paste0(savedir,"NZEHGSP23_235092_mortality_DiveDuration_datetime.png"), dpi=300)

event<-Birds_dpth%>%filter(datetime > ymd_hms("2026-01-06 00:36:03")&  datetime < ymd_hms("2026-01-07 22:35:55"))
event_zm<-Birds_dpth%>%filter(datetime > ymd_hms("2026-01-07 00:36:06") &  datetime < ymd_hms("2026-01-07 22:35:55"))
event_zm_zm<-Birds_dpth%>%filter(datetime > ymd_hms("2026-01-06 21:40:36") &  datetime < ymd_hms("2026-01-06 21:40:50"))
event_zm_before<-Birds_dpth%>%filter(datetime > ymd_hms("2026-01-01 07:09:51") &  datetime < ymd_hms("2026-01-07 22:35:55"))
day_event<-Birds_dpth%>%filter(datetime > ymd_hms("2026-01-01 7:09:51")&  datetime < ymd_hms("2026-01-07 22:35:55"))
#event 1/7/2026  3:44:54 PM  - 1/7/2026  3:45:02 PM
#1/1/2026  7:09:51 AM - 1/7/2026  10:35:55 PM

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
  scale_color_manual(values=met.brewer("Hokusai2", 1930))+
  labs(title="Spotted Shag: 235092")+
  ylab("Depth (m)")+
  xlab("Diving Activity the Previous Week (NZST)")+
  theme(legend.position="none")+
  NULL
ggsave(paste0(savedir,"NZEHGSP23_235092_mortality_DiveDepth_WeekBefore.png"), dpi=300)

ggplot()+
  geom_point(data=event%>%filter(is.na(dive_id)==FALSE),
             aes(x=datetime,y=-depth, color=as.factor(dive_id)))+
  geom_point(data=event%>%filter(depth==0),
             aes(x=datetime,y=-depth), color="black")+
  geom_path(data=event%>%filter(is.na(dive_id)==FALSE),
            aes(x=datetime,y=-depth, group=as.factor(dive_id), color=as.factor(dive_id)))+
  scale_color_manual(values=met.brewer("Hokusai2", 234))+
  labs(title="Spotted Shag: 235092")+
  ylab("Depth (m)")+
  xlab("Jan")+
  theme(legend.position="none")+
  NULL
ggsave(paste0(savedir,"235092_mortality_DiveDepth_last24.png"), dpi=300)

ggplot()+
  geom_point(data=event_zm,
             aes(x=datetime,y=-depth, color=as.factor(dive_id)))+
  geom_point(data=event_zm%>%filter(depth==0),
             aes(x=datetime,y=-depth), color="black")+
  geom_path(data=event_zm,
            aes(x=datetime,y=-depth),linetype=2)+
  scale_color_manual(values=met.brewer("Tam", 27,direction = -1))+
  labs(title="Spotted Shag: 235092")+
  ylab("Depth (m)")+
  xlab("January 7, 2026 (NZST)")+
  theme(legend.position="none")+
  NULL
ggsave(paste0(savedir,"235092_last_DiveDepth_Jan072026eventZoom.png"), dpi=300)


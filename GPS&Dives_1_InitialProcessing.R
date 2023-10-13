library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table) #rename
library(stringr)
library(R.utils)
library(tidyr)

if(require("argosfilter")==FALSE) install.packages("argosfilter")
library(argosfilter)

# Link to local Box Sync folder ---- 
#To find user/computer specific username use: Sys.getenv("LOGNAME")

if(Sys.info()[4]=="benthos") {
  args = commandArgs(trailingOnly=TRUE)
  datadir<-args[1] #/home/DASHCAMS/data/ornitela_ftp_data/
  deplymatrix<-args[2] #/home/DASHCAMS/data_raw/metadata/DASHCAMS_Deployment_Field_data.csv
  savedir<-args[3] #/home/DASHCAMS/zTagStatus/
  source('/home/DASHCAMS/git/CormOcean/MakeDive.R') 
}

#if(Sys.info()[4]=="benthos") {
#  datadir<-'/home/DASHCAMS/data_raw/ornitela_ftp_data/'
#  savedir<-'/home/DASHCAMS/data_processed/zTagStatus/'
#  deplymatrix<-'/home/DASHCAMS/data_raw/metadata/DASHCAMS_Deployment_Field_data.csv'
#}

if(Sys.info()[7]=="rachaelorben") {
  datadir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/data/ornitela_ftp_data/'
  savedir<-'/Users/rachaelorben/zTagStatus/'
  deplymatrix<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

#  Pulls in deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(deplymatrix)
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
deploy_matrix<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC)%>%
  filter(is.na(TagSerialNumber)==FALSE)
unique(deploy_matrix$Project_ID)

(dm<-deploy_matrix%>%filter(Project_ID=="NZEHGSP23"))
(dm<-deploy_matrix%>%filter(Project_ID=="SOUDICA22"))

#  Pulls downloaded data files ---------------------------------------------
Files<-list.files("/Users/rachaelorben/Desktop/NZ", full.names = TRUE)
filenames<-list.files("/Users/rachaelorben/Desktop/NZ")

Files<-list.files("/Users/rachaelorben/Desktop/SA", full.names = TRUE)
filenames<-list.files("/Users/rachaelorben/Desktop/SA")


locs<-NULL
for (i in 1:length(Files)){
  
    tagID<-sapply(strsplit(filenames[i], split='_', fixed=TRUE), function(x) (x[1]))
    
    deply_sel<-dm[dm$TagSerialNumber==tagID[1],]
    
    dat <- read.csv(Files[i], header=TRUE, nrows = 0,  skipNul=TRUE)
    
    unique(dat$datatype)
    dat<-dat%>%filter(datatype!="SEN_ACC_5Hz_START")%>% #removes surface accelerometry
      filter(datatype!="SEN_ACC_5Hz")%>%
      filter(datatype!="SEN_ACC_5Hz_ENDINT")%>%
      filter(datatype!="SEN_ACC_5Hz_END")
    
    dat<-rename(dat,lat="Latitude")
    dat<-rename(dat,lon="Longitude")
    dat$datetime<-ymd_hms(dat$UTC_timestamp)
    
    dat$Project_ID<-deply_sel$Project_ID
    dat$tagID<-tagID
    
    dat<-dat%>%filter(UTC_timestamp>deply_sel$DeploymentStartDatetime & UTC_timestamp<deply_sel$DeploymentEndDatetime_UTC)
    dat$DeployEndShort<-deply_sel$Deployment_End_Short
    
    dat[is.na(dat)==TRUE]<-NA
    dat$Foid<-1:nrow(dat)
    
    dat_gps<-dat%>%filter(!is.na(lat))
    dat_gps$PointDur <- NA
    
    # force difftime in secs & finds surface drifts (diff of <2 for more than 5 seconds)
    dat_gps$PointDur <- abs(as.numeric(difftime(time1 =  dat_gps$datetime,
                                                time2 = lead(dat_gps$datetime),
                                                units = "secs")))
    
    out <- data.frame(unclass(rle(dat_gps$PointDur<=2)))
    out$pos <- head(cumsum(c(1, out$lengths)), -1)
    out.s<-out[out$lengths>=5  & out$values,c("pos", "lengths")]
    
    dat$GPS_surfacedrifts<-0 #annotates GPS surface drift bursts
    for (j in 1:nrow(out.s)){
      if (nrow(out.s)==0) next
      info<-out.s[j,]
      idx1<-dat_gps$Foid[info$pos]
      idx2<-dat_gps$Foid[info$pos+info$lengths]
      dat$GPS_surfacedrifts[idx1:idx2]<-1
    }
    
    locs<-rbind(locs,dat)
}

#remove duplicate GPS points 
#locs2<-locs%>%group_by(device_id,datetime)%>%
#  distinct(device_id,datetime, .keep_all = TRUE)%>%
#  arrange(datetime) #arranges by time, could scramble data >1HZ a little bit

Birds_gps<-locs%>%filter(lat!=0)%>%filter(is.na(lat)==FALSE)%>%filter(lon!=0)
(birds<-unique(Birds_gps$tagID))

#rough speed filter 
Birds_gps_sp<-NULL
for (j in 1:length(birds)){
  Locs1<-Birds_gps%>%filter(device_id==birds[j])
  try(mfilter<-vmask(lat=Locs1$lat, lon=Locs1$lon, dtime=Locs1$datetime, vmax=25), silent=FALSE)
  #if mfilter isn't made this makes one that selects all points
  if (exists("mfilter")==FALSE) mfilter<-rep("not", nrow(Locs1))
  Locs1$mfilter<-mfilter
  #Locs<-Locs1%>%filter(mfilter!="removed")
  Birds_gps_sp<-rbind(Birds_gps_sp,Locs1)
}


# identify dives ----------------------------------------------------------
Birds_dpth<-locs%>%filter(is.na(depth_m)==FALSE)
Birds_dpth$tdiff_sec <-difftime(Birds_dpth$datetime, lag(Birds_dpth$datetime, 1),units = "secs")
head(Birds_dpth)

id_num <- which(colnames(Birds_dpth) == "tagID") 
dt_num <- which(colnames(Birds_dpth) == "datetime") 
dp_num <- which(colnames(Birds_dpth) == "depth_m") 
td_num <- which(colnames(Birds_dpth) == "tdiff_sec") 

Birds_dpth_MD<-MakeDive(Birds_dpth,idCol=id_num, #column index with unique ID
                     dtCol=dt_num, #column index with datetime
                     depthCol=dp_num, #column index with depth
                     tdiffCol=td_num, #column index with time difference in seconds
                     DepthCutOff=1, #depth that dives happen below (meters)
                     DiveDepthYes=1.5, #dives need to reach 3 meters to be considered a dive event
                     TimeDiffAllowed_sec=2, #consecutive points need to have a time difference <2 to be in the same event
                     NumLocCut=2) #dives need to contain three points to be considered a dive, could change this to a duration

Birds_dpth_MD$date<-date(Birds_dpth$datetime)
Birds_dpth_MD$datatype<-Birds_dpth$datatype

dive_sum<-Birds_dpth_MD%>%
  group_by(ID,divedatYN,)%>% #individual dive summaries
  dplyr::summarise(minDt=min(datetime),
            maxDt=max(datetime),
            maxDepth=max(depth,na.rm=TRUE),
            uDepth=round(mean(depth,na.rm=TRUE),2))%>%
  mutate(dur=maxDt-minDt,
         date=date(minDt))


#matches dive with GPS points 
IDS<-unique(dive_sum$ID)
dive_sum_gps<-NULL
for (i in 1:length(IDS)){
  birdy<-Birds_gps%>%filter(tagID==IDS[i])
  birdy$gps_oid<-1:nrow(birdy)
  bdive<-dive_sum%>%filter(ID==IDS[i])
  
  dt<-unique(bdive$date)
  bdive_all<-NULL
for (k in 1:length(dt)){
    birdy_dt<-birdy%>%filter(UTC_date==dt[k])
    bdive_dt<-bdive%>%filter(date==dt[k])

    bdive_dt$gps_tdiff<-NA
    bdive_dt$lat<-NA
    bdive_dt$lon<-NA
    bdive_dt$gps_datatype<-NA
    bdive_dt$gps_oid<-NA
    bdive_dt$gps_time<-NA
for (j in 1:nrow(bdive_dt)){
    birdy_dt$gpstd<-(abs(birdy_dt$datetime-bdive_dt$maxDt[j]))
    sm<-min(abs(birdy_dt$gpstd))
    idx<-which(abs(birdy_dt$gpstd)==sm)
  
    bdive_dt$gps_tdiff[j]<-birdy_dt$datetime[idx]-bdive_dt$maxDt[j]
    bdive_dt$lat[j]<-birdy_dt$lat[idx]
    bdive_dt$lon[j]<-birdy_dt$lon[idx]
    bdive_dt$gps_datatype[j]<-birdy_dt$datatype[idx]
    bdive_dt$gps_oid[j]<-birdy_dt$gps_oid[idx]
    bdive_dt$gps_time[j]<-birdy_dt$datetime[idx]
}
    bdive_all<-rbind(bdive_all,bdive_dt)     
}

dive_sum_gps<-rbind(dive_sum_gps,bdive_all)  
}

dsum<-Birds_dpth%>%group_by(ID,date)%>% #daily dive summaries
  summarise(n=n_distinct(divedatYN),
            minDt=min(datetime),
            maxDt=max(datetime),
            maxDepth=max(depth,na.rm=TRUE),
            uDepth=round(mean(depth,na.rm=TRUE),2))

# quick summary of the bird data
sumDat<-locs%>%group_by(tagID)%>%
  summarise(minDt=min(datetime),
            maxDt=max(datetime),
            maxDepth=max(depth_m,na.rm=TRUE),
            uDepth=round(mean(depth_m,na.rm=TRUE),2),
            n_GPS=n_distinct(lat),
            uBat=round(mean(U_bat_mV,na.rm=TRUE)),
            uTemp=round(mean(ext_temperature_C,na.rm=TRUE),2),
            uCond=round(mean(conductivity_mS.cm,na.rm=TRUE),2),
            GPS_surfacedrift_pts=sum(GPS_surfacedrifts))%>%
  mutate(dur=round(maxDt-minDt,2)) 


#write.csv(x=dive_sum, paste0("/Users/rachaelorben/Desktop/NZ","/DiveSum.csv"))
write.csv(x=dive_sum, paste0("/Users/rachaelorben/Desktop/SA","/DiveSum.csv"))


w2hr<-map_data('world')

y_min<-min(Birds_gps$lat)-.25
y_max<-max(Birds_gps$lat)+.25

x_min<-min(Birds_gps$lon)-.25
x_max<-max(Birds_gps$lon)+.25

quartz(width=5,height=4)
ggplot()+
  geom_polygon(data=w2hr,aes(long,lat,group=group),fill="grey70",color="grey60",linewidth=0.1)+
  geom_path(data=Birds_gps,aes(x=lon,y=lat, group=device_id))+
  geom_point(data=Birds_gps,aes(x=lon,y=lat, color=datatype))+
  xlab("Longitude")+
  ylab("Latitude")+
  coord_fixed(ratio=1.7,xlim = c(x_min,x_max),ylim=c(y_min,y_max))+
  theme_bw()
ggsave(filename = paste0("/Users/rachaelorben/Desktop/SA/GPSplot.jpg"))


names(Birds_dpth_MD)
Birds_gps$Pt<-1


Birds_dpth_MD<-cbind(Birds_dpth_MD,ext_temperature_C=Birds_dpth$ext_temperature_C)
quartz(width=12,height=3)
ggplot()+
  geom_point(data=Birds_gps,aes(x=datetime,y=Pt, color=datatype))+
  geom_path(data=Birds_dpth_MD%>%filter(is.na(divedatYN)==FALSE),
            aes(x=datetime,y=-depth, group=divedatYN))+
  geom_point(data=Birds_dpth_MD%>%filter(is.na(divedatYN)==FALSE),
             aes(x=datetime,y=-depth, fill=ext_temperature_C, group=divedatYN), shape = 21)+
  theme_bw()+
  ylab("Depth (m)")+
  xlab("")+
  NULL
ggsave(filename = paste0("/Users/rachaelorben/Desktop/SA/DiveData.jpg"))

names(Birds_dpth_MD)
quartz(width=12,height=3)
ggplot()+
  geom_point(data=Birds_gps%>%filter(UTC_date=="2022-12-05"),
             aes(x=datetime,y=Pt, color=datatype))+
  geom_path(data=Birds_dpth_MD%>%filter(is.na(divedatYN)==FALSE)%>%filter(date=="2022-12-05"),
            aes(x=datetime,y=-depth, group=divedatYN))+
  geom_point(data=Birds_dpth_MD%>%filter(is.na(divedatYN)==FALSE)%>%filter(date=="2022-12-05"),
             aes(x=datetime,y=-depth, fill=ext_temperature_C, group=divedatYN), shape = 21)+
  theme_bw()+
  ylab("Depth (m)")+
  xlab("")+
  NULL
ggsave(filename = paste0("/Users/rachaelorben/Desktop/SA/DiveData_12-5.jpg"))

quartz(width=12,height=3)
ggplot()+
  geom_point(data=Birds_gps%>%filter(UTC_date=="2022-12-06"),
             aes(x=datetime,y=Pt, color=datatype))+
  geom_path(data=Birds_dpth_MD%>%filter(is.na(divedatYN)==FALSE)%>%filter(date=="2022-12-06"),
            aes(x=datetime,y=-depth, group=divedatYN))+
  geom_point(data=Birds_dpth_MD%>%filter(is.na(divedatYN)==FALSE)%>%filter(date=="2022-12-06"),
             aes(x=datetime,y=-depth, fill=ext_temperature_C, group=divedatYN), shape = 21)+
  theme_bw()+
  ylab("Depth (m)")+
  xlab("")+
  NULL
ggsave(filename = paste0("/Users/rachaelorben/Desktop/SA/DiveData_12-6.jpg"))


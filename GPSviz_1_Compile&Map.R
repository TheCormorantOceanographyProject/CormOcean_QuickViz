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
  datadir<-'/data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

#  Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
dm<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC,TagManufacture)%>%
  filter(is.na(TagSerialNumber)==FALSE)

prjt<-unique(dm$Project_ID)
prjt<-prjt[prjt!="USACRBRDO14"]

# Loop through each project -----------------------------------------------

# Find Project Data Files -------------------------------------------------
# eventually change to pull in gps only files


for (i in 27:length(prjt)){
  
  Files<-list.files(paste0(usrdir,datadir,prjt[i],"/gps_sensors_v2"), full.names = TRUE)
  filenames<-list.files(paste0(usrdir,datadir,prjt[i],"/gps_sensors_v2"))
  
  if (length(Files)==0) next
# Loads Data ------------------------------------------------
birds<-NULL
for (j in 1:length(Files)){
  
  tagID<-sapply(strsplit(filenames[j], split='_', fixed=TRUE), function(x) (x[1]))
  dm_p<-dm%>%filter(Project_ID==prjt[i])
  deply_sel<-dm_p[dm_p$TagSerialNumber==tagID[1],]
  deply_sel<-deply_sel[1,]
  
  dat <- read.csv(Files[j], header=TRUE, nrows = 0,  skipNul=TRUE)
  if(ncol(dat)==1) next
  
  unique(dat$datatype)
  dat<-rename(dat,lat="Latitude")
  dat<-rename(dat,lon="Longitude")
  dat<-dat%>%filter(is.na(lat)==FALSE)
  dat$datetime<-ymd_hms(dat$UTC_timestamp)
  
  dat$Project_ID<-prjt[i]
  
  if(is.na(deply_sel$DeploymentEndDatetime_UTC)==TRUE) {dat<-dat%>%filter(UTC_timestamp>deply_sel$DeploymentStartDatetime)}
  if(is.na(deply_sel$DeploymentEndDatetime_UTC)==FALSE) {dat<-dat%>%filter(UTC_timestamp>deply_sel$DeploymentStartDatetime & UTC_timestamp<deply_sel$DeploymentEndDatetime_UTC)}
  if(nrow(dat)==0) next #skips data that was collected after a tag fell off the bird / bird died
  
  dat$DeployEndShort<-deply_sel$Deployment_End_Short
  
  dat[is.na(dat)==TRUE]<-NA
  dat$Foid<-1:nrow(dat)
  
  #remove columns not relavent for GPS data
  dat<-dat%>%select(-Reserved,-depth_m,-altimeter_m,-conductivity_mS.cm,-ext_temperature_C,-mag_x,-mag_y,-mag_z,-acc_x,-acc_y,-acc_z)%>%
    filter(lat!=0) #removes 0,0 GPS values
  birds<-rbind(birds,dat)
}

  if(nrow(birds)==0) next

  # Speed Filter ------------------------------------------------------------
birds$Uni_ID<-paste0(birds$Project_ID,"_",birds$device_id)
IDs<-unique(birds$Uni_ID)

vmax_val=20 #(72 km/hr)

locs<-NULL
for (j in 1:length(IDs)){
  Locs1<-birds%>%filter(Uni_ID==IDs[j])
  Locs1<-Locs1%>%group_by(datetime)%>%
    distinct(datetime, .keep_all = TRUE)%>%
    arrange(datetime) #arranges by time
  try(mfilter<-vmask(lat=Locs1$lat, lon=Locs1$lon, dtime=Locs1$datetime, vmax=vmax_val), silent=FALSE)
  #if mfilter isn't made this makes one that selects all points
  if (exists("mfilter")==FALSE) mfilter<-rep("not", nrow(Locs1))
  Locs1$mfilter<-mfilter
  Locs<-Locs1%>%filter(mfilter!="removed")
  locs<-rbind(locs,Locs)
}

#identify and sequentially number GPS Dive bursts (typically 2 GPS fixes)
locs$tdiff_sec <-round(difftime(locs$datetime, lag(locs$datetime, 1),units = "secs"),2)

# Find consecutively recorded GPS data
locs<- locs %>% group_by(device_id)%>%
  mutate(gap_time=tdiff_sec>2, # find times when there is a gap > 2sec
         gap_time=ifelse(is.na(gap_time),0,gap_time), #fill NAs
         gpsDiveburstID=(cumsum(gap_time)))#, # gap_time is T/F so cumsum is adding 1

#sequentially numbers each row in each burst
locs<-locs%>%group_by(device_id,gpsDiveburstID)%>%
  mutate(gpsNum=row_number())

head(locs)
saveRDS(locs, paste0(usrdir,savedir,"Processed_Deployment_Data/",prjt[i],"_GPS_SpeedFiltered.rds"))

# Map Check --------------------------------------------------------------------
# Pulls in saved data and plots

for (i in 1:length(prjt)){
  
locs<-readRDS(paste0(usrdir,savedir,"Processed_Deployment_Data/",prjt[i],"_GPS_SpeedFiltered.rds"))
dm_prj<-dm%>%filter(Project_ID==prjt[i])

w2hr<-map_data('world')

locs_wgs84<-st_as_sf(locs,coords=c('lon','lat'),remove = F,crs = 4326)

y_min<-min(locs$lat)-.25
y_max<-max(locs$lat)+.25
x_min<-min(locs$lon)-.25
x_max<-max(locs$lon)+.25

dt<-Sys.Date()
ids<-unique(locs$Uni_ID)

#quartz()
ggplot()+
    geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray50",color="grey5",linewidth=0.1)+
    geom_sf(data = locs_wgs84, aes(color=Uni_ID), size=.01)+
    scale_color_manual(values=met.brewer("Tam", length(ids)))+
    coord_sf(xlim = c(x_min,x_max),ylim=c(y_min,y_max))+
  xlab("Longitude")+
  ylab("Latitude")+
    theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave(paste0(usrdir,savedir,"PLOTS/DeploymentMaps/",prjt[i],"_",dt,"_Map.png"), dpi=300)

# All Bird Data Coverage --------------------------------------------------
locs$date<-date(locs$UTC_datetime)
names(locs)
dm_prj_O<-dm_prj%>%filter(TagManufacture=="Ornitela")
dm_prj_O$start<-date(dm_prj_O$DeploymentStartDatetime)
dm_prj_O$end<-date(dm_prj_O$DeploymentEndDatetime_UTC)
dm_prj_O<-dm_prj_O%>%rename("device_id"="TagSerialNumber")
dm_prj_O<-dm_prj_O%>%select(device_id,start,end)
dm_prj<-dm_prj_O %>% 
  pivot_longer(-device_id, names_to = "d_info", values_to = "date")


ggplot()+
  geom_point(data=locs, aes(y=as.factor(device_id),x=date), size=0.05)+
  geom_point(data=dm_prj, aes(y=as.factor(device_id),x=date,color=d_info, fill=d_info))+
  ylab("") 
ggsave(paste0(usrdir,savedir,"PLOTS/DeploymentCoverage/",prjt[i],"_",dt,"_TimeFrame.png"), dpi=300)

}


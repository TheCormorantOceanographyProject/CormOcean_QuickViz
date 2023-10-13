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

prjt<-c("USACRBRPE19","USAFIBR21","USACRBR22","USAPRBR23")

#  Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
deploy_matrix<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC)%>%
  filter(is.na(TagSerialNumber)==FALSE)

IDx<-which(deploy_matrix$Project_ID %in% prjt)
dm<-deploy_matrix[IDx,] #selects the file names with IDs that are currently deployed


# Find Project Data Files -------------------------------------------------

Files<-NULL
filenames<-NULL
for (i in 2:length(prjt)){
  
  Files.fu<-list.files(paste0(usrdir,datadir,prjt[i],"/gps_sensors_v2"), full.names = TRUE)
  files.sh<-list.files(paste0(usrdir,datadir,prjt[i],"/gps_sensors_v2"))
  
  Files<-c(Files,Files.fu)
  filenames<-c(filenames,files.sh)


# Loads Data ------------------------------------------------
birds<-NULL
for (j in 1:length(Files)){
  
  tagID<-sapply(strsplit(filenames[j], split='_', fixed=TRUE), function(x) (x[1]))
  deply_sel<-dm[dm$TagSerialNumber==tagID[1],]
  
  dat <- read.csv(Files[j], header=TRUE, nrows = 0,  skipNul=TRUE)
  if(ncol(dat)==1) next
  
  unique(dat$datatype)
  dat<-rename(dat,lat="Latitude")
  dat<-rename(dat,lon="Longitude")
  dat<-dat%>%filter(is.na(lat)==FALSE | is.na(depth_m)==FALSE) #removes axy only rows
  dat$datetime<-ymd_hms(dat$UTC_timestamp)
  #dat$datetime<-ymd_hms(paste0(dat$UTC_datetime[80],".",dat$milliseconds))
  
  dat$Project_ID<-deply_sel$Project_ID
  
  if(is.na(deply_sel$DeploymentEndDatetime_UTC)==TRUE) {dat<-dat%>%filter(UTC_timestamp>deply_sel$DeploymentStartDatetime)}
  if(is.na(deply_sel$DeploymentEndDatetime_UTC)==FALSE) {dat<-dat%>%filter(UTC_timestamp>deply_sel$DeploymentStartDatetime & UTC_timestamp<deply_sel$DeploymentEndDatetime_UTC)}
  if(nrow(dat)==0) next #skips data that was collected after a tag fell off the bird / bird died
  
  dat[is.na(dat)==TRUE]<-NA
  dat$Foid<-1:nrow(dat)
  
  #remove a few columns
  #names(dat)
  dat<-dat%>%select(-Reserved,-int_temperature_C)
  birds<-rbind(birds,dat)
}
  saveRDS(birds, paste0(usrdir,savedir,"Processed_Deployment_Data/",prjt[i],"_BRAC_GPSDive.rds"))
}

# Speed Filter ------------------------------------------------------------
birds_gps<-birds%>%filter(is.na(lat)==FALSE)%>%filter(lat!=0) #removes 0,0 GPS values
birds_gps$Uni_ID<-paste0(birds_gps$Project_ID,"_",birds_gps$device_id)
IDs<-unique(birds_gps$Uni_ID)

vmax_val=20 #(72 km/hr)

locs<-NULL
for (j in 1:length(IDs)){
  Locs1<-birds_gps%>%filter(Uni_ID==IDs[j])
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


# Dive Coverage -----------------------------------------------------------
birds_depth<-birds%>%filter(is.na(depth_m)==FALSE)
birds_depth$date<-date(birds_depth$datetime)

ggplot()+
  geom_point(data=birds_depth, aes(y=as.factor(device_id),x=date), size=0.05)+
  geom_point(data=dm_prj, aes(y=as.factor(device_id),x=date,color=d_info, fill=d_info))+
  ylab("") 
#ggsave(paste0(usrdir,savedir,"PLOTS/DeploymentCoverage/",prjt[i],"_",dt,"_TimeFrame.png"), dpi=300)


# Map Check --------------------------------------------------------------------
w2hr<-map_data('world')

locs_wgs84<-st_as_sf(locs,coords=c('lon','lat'),remove = F,crs = 4326)

y_min_U<-min(locs$lat)-.25
y_max_U<-30.5
x_min_U<-45
x_max_U<-max(locs$lon)+.25


#quartz()
ggplot()+
    geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray25",color="grey60",linewidth=0.1)+
    geom_sf(data = locs_wgs84, aes(color=Uni_ID), size=.01)+
    scale_color_manual(values=met.brewer("Tam", 37))+
    coord_sf(xlim = c(47,x_max_U),ylim=c(y_min_U,y_max_U))+
    theme_bw()+
    theme(legend.position ="none",axis.title = element_blank(),
          axis.text = element_text(size = 8))
ggsave(paste0(usrdir,savedir,"Map_BRAC_Cormorants.png"), dpi=300)

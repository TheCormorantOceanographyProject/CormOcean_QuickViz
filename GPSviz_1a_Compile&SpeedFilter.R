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

if(Sys.info()[7]=="alexa") {
  usrdir<-"/Users/alexa/Box Sync/DASHCAMS/"
  datadir<-'data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  #savedir<-'Research Workspace/Project Metadata/Bounding Coordinates'
  deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/alexa/git_repos/CormOcean_QuickViz/MakeDive.R')
}

#  Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
dm<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC,TagManufacture)%>%
  filter(is.na(TagSerialNumber)==FALSE)

#all project names
prjt_all<-unique(dm$Project_ID)

#projects with tags currently deployed
tags_current<-dm%>%filter(is.na(DeploymentEndDatetime_UTC))
prjt_current<-unique(tags_current$Project_ID)

#projects with all end dates complete
prjt_complete<-prjt_all[!(prjt_all %in% prjt_current)]

#projects to process: change as needed
prjt<-prjt_current
#prjt<-prjt_all
prjt<-prjt[prjt!="USACRBRDO14"] #removes non-Ornitela Projects

# Loop through each project -----------------------------------------------

# Find Project Data Files -------------------------------------------------
# eventually change to pull in gps only files
for (i in 25:length(prjt)){
  
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

#archive_dat<-readRDS(paste0(usrdir,savedir,"Processed_GPS_Deployment_Data/",prjt[i],"_GPS_SpeedFiltered.rds"))

#if(nrow(archive_dat)==nrow(locs)) next #only saves new data if the files are not the same
#if(nrow(archive_dat) > nrow(locs)) next #only saves new data if there are more new rows

saveRDS(locs, paste0(usrdir,savedir,"Processed_GPS_Deployment_Data/",prjt[i],"_GPS_SpeedFiltered.rds"))

}

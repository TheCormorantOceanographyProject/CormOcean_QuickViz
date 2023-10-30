library(dplyr)
library(lubridate)
library(data.table) #rename
library(stringr)
library(R.utils)
library(tidyr)

if(Sys.info()[7]=="rachaelorben") {
  usrdir<-"/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/"
  FTPdatadir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/data/ornitela_ftp_data/'
  ATNdatadir<-'data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

options(dplyr.summarise.inform = FALSE) #supresses group warning
options(warn =1)

ptm <- proc.time() #start timer

Files<-list.files(FTPdatadir, full.names = TRUE) #26986 files and counting
filenames<-list.files(FTPdatadir, full.names = FALSE) #26986 files and counting


FTP_datsum<-NULL

for (i in 1:length(Files)){
  
  if (file.size(Files[i]) > 0){
    dat<-read.csv(Files[i])
  }
  
  if (file.size(Files[i]) == 0) next
  
  dat$datetime1<-ymd_hms(dat$UTC_datetime)
  dat$datetime2<-mdy_hm(dat$UTC_datetime)
  
  dat$date1<-date(dat$datetime1)
  dat$hour1<-hour(dat$datetime1)
  
  dat$date2<-date(dat$datetime2)
  dat$hour2<-hour(dat$datetime2)
  
  dat<-dat %>% mutate(date = coalesce(date1,date2)) 
  dat<-dat %>% mutate(hour = coalesce(hour1,hour2)) 
  
  datsum<-dat%>%
    group_by(device_id,date,hour)%>%
    summarise(FTPn=n())
  datsum$File<-filenames[i]
  
  if (nrow(datsum)<1){ #if file has data, but datsum doesn't work
    datsum<-data.frame(device_id=99999,date=ymd("2020-01-01"),hour=25,n=0,File=filenames[i])
  }
  FTP_datsum<-rbind(FTP_datsum, datsum)
}

proc.time() - ptm #stops timer
  
dt<-Sys.Date()
write.csv(FTP_datsum,paste0(usrdir,savedir,"FTPDataCheck_DayHr_",dt,".csv"))
FTP_datsum%>%filter(hour==25)

# Trim FTP datasum by deployment matrix & add projects

# ATN_data ----------------------------------------------------------------
#  Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
dm<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC,TagManufacture)%>%
  filter(is.na(TagSerialNumber)==FALSE)

#all project names
prjt_all<-unique(dm$Project_ID)
prjt<-prjt_all[prjt_all!="USACRBRDO14"] #removes non-Ornitela Projects

ATN_datsum<-NULL
for (i in 1:length(prjt)){
  
Files<-list.files(paste0(usrdir,ATNdatadir,prjt[i],"/gps_sensors_v2"), full.names = TRUE)
filenames<-list.files(paste0(usrdir,ATNdatadir,prjt[i],"/gps_sensors_v2"))

for (j in 1:length(Files)){
  
  if (file.size(Files[j]) > 0){
    dat<-read.csv(Files[j])
  }
  
  if (file.size(Files[j]) == 0) next
  if (ncol(dat) == 1) {datsum<-data.frame(device_id=99999,date=ymd("2020-01-01"),hour=25,ATNn=0,File=filenames[j],prjt=prjt[i])
  }
  if (ncol(dat) == 1) {ATN_datsum<-rbind(ATN_datsum, datsum)}
  if (ncol(dat) == 1) next
  
  dat$datetime1<-ymd_hms(dat$UTC_datetime)
  dat$datetime2<-mdy_hm(dat$UTC_datetime)
  
  dat$date1<-date(dat$datetime1)
  dat$hour1<-hour(dat$datetime1)
  
  dat$date2<-date(dat$datetime2)
  dat$hour2<-hour(dat$datetime2)
  
  dat<-dat %>% mutate(date = coalesce(date1,date2)) 
  dat<-dat %>% mutate(hour = coalesce(hour1,hour2)) 
  
  datsum<-dat%>%
    group_by(device_id,date,hour)%>%
    summarise(ATNn=n())
  datsum$File<-filenames[j]
  datsum$prjt<-prjt[i]
  
  if (nrow(datsum)<1){ #if file has data, but datsum doesn't work
    datsum<-data.frame(device_id=99999,date=ymd("2020-01-01"),hour=25,ATNn=0,File=filenames[j],prjt=prjt[i])}
  ATN_datsum<-rbind(ATN_datsum, datsum)
}
}


proc.time() - ptm #stops timer
write.csv(ATN_datsum,paste0(usrdir,savedir,"ATNDataCheck_DayHr_",dt,".csv"))

qfiles<-ATN_datsum%>%filter(device_id==99999)%>%
  group_by(prjt,File)%>%summarise(n=n())
write.csv(qfiles,paste0(usrdir,savedir,"ATNDataCheck_DayHr_",dt,"_Emptyfiles.csv"))

names(ATN_datsum)
ATN_datsum%>%group_by(prjt)%>%
  summarise(n=n_distinct(prjt))

head(ATN_datsum)
head(FTP_datsum)

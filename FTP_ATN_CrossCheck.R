library(dplyr)
library(lubridate)
library(data.table) #rename
library(stringr)
library(R.utils)
library(tidyr)

if(Sys.info()[7]=="rachaelorben") {
  usrdir<-"/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/"
  FTPdatadir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/data/ornitela_ftp_data/'
  datadir<-'/data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

options(dplyr.summarise.inform = FALSE) #supresses group warning
options(warn =1)

ptm <- proc.time() #start timer

Files<-list.files(FTPdatadir, full.names = TRUE) #26986 files and counting
filenames<-list.files(FTPdatadir, full.names = FALSE) #26986 files and counting

my_function <- function(x, y){
  tryCatch(
    #try to do this
    {
      dat$datetime<-ymd_hms(dat$UTC_datetime)
    },
    #if an error occurs, tell me the error
    error=function(e) {
      message('An Error Occurred')
      print(e)
    },
    #if a warning occurs, tell me the warning
    warning=function(w) {
      message('A Warning Occurred')
      print(w)
      return(NA)
    }
  )
}


FTP_datsum<-NULL
#i=2150
for (i in 1:length(Files)){
  if (file.size(Files[i]) > 0){
    dat<-read.csv(Files[i])
  }
  
  funs <- c("ymd_hms","mdy_hm")
  
  # vector to store results
  dates <- as.POSIXct(rep(NA,length(dat)))
  
  # we try everything lubridate has. There will be some warnings
  # e.g. because mdy cannot translate everything. You can ignore this.
  for ( f in funs ){
    dates[is.na(dates)] <- do.call(f,list(dat$UTC_datetime[is.na(dates)]))  
  }
  dates
  
  
  dat$datetime<-ymd_hms(dat$UTC_datetime)
  dat$datetime<-mdy_hm(dat$UTC_datetime)
  
  dat$date<-date(dat$datetime)
  dat$hour<-hour(dat$datetime)
  
  datsum<-dat%>%
    group_by(device_id,date,hour)%>%
    summarise(n=n())
  datsum$File<-filenames[i]
  FTP_datsum<-rbind(FTP_datsum, datsum)
}

proc.time() - ptm #stops timer
  
  
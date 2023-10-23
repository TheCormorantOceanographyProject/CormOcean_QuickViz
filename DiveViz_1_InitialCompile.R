library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table) #rename
library(stringr)
library(R.utils)
library(tidyr)

if(Sys.info()[7]=="rachaelorben") {
  usrdir<-"/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/"
  datadir<-'/data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

op <- options(digits.secs=3)

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
#prjt<-prjt_current
prjt<-prjt_all
prjt<-prjt[prjt!="USACRBRDO14"]

# Loop through each project -----------------------------------------------

# Find Project Data Files -------------------------------------------------
# eventually change to pull in gps only files
for (i in 11:length(prjt)){
  
  Files<-list.files(paste0(usrdir,datadir,prjt[i],"/gps_sensors_v2"), full.names = TRUE)
  filenames<-list.files(paste0(usrdir,datadir,prjt[i],"/gps_sensors_v2"))
  
  if (length(Files)==0) next

 a<-seq(1,length(Files),50) 
 if (length(Files)>50) {b<-c(seq(50,length(Files),50),length(Files))}

 if (length(Files)>50) {sets<-data.frame(st=a,end=b)}
 if (length(Files)<51) {sets<-data.frame(st=1,end=length(Files))}
   
# Loads Data ------------------------------------------------
for(k in 1:nrow(sets)){
  s<-sets[k,]
  
  Birds_dpth<-NULL
  for (j in as.numeric(s[1]):as.numeric(s[2])){
    
    tagID<-sapply(strsplit(filenames[j], split='_', fixed=TRUE), function(x) (x[1]))
    dm_p<-dm%>%filter(Project_ID==prjt[i])
    deply_sel<-dm_p[dm_p$TagSerialNumber==tagID[1],]
    deply_sel<-deply_sel[1,]
    
    dat <- read.csv(Files[j], header=TRUE, nrows = 0,  skipNul=TRUE)
    if(ncol(dat)==1) next
    
    #moves diving annotations to the datatype column
    dat$datatype[dat$satcount=="DIVING_SENSOR_OFF"]<-"DIVING_SENSOR_OFF"
    dat$datatype[dat$satcount=="DIVING_SENSOR_ON"]<-"DIVING_SENSOR__ON"
    
    dat<-dat%>%filter(is.na(depth_m)==FALSE | datatype=="NOTE_DIVING") #keeps Diving switch notes
    dat$Rtime<-dat$milliseconds/1000
    
    dat$UTC_time_ms <- strftime(strptime(dat$UTC_time,format="%H:%M:%OS")+(dat$Rtime %% 1),format="%H:%M:%OS2")
    dat$datetime<-ymd_hms(paste(dat$UTC_date,dat$UTC_time_ms))
    dat[is.na(dat)==TRUE]<-NA
    
    if(is.na(deply_sel$DeploymentEndDatetime_UTC)==TRUE) {dat<-dat%>%filter(UTC_timestamp>deply_sel$DeploymentStartDatetime)}
    if(is.na(deply_sel$DeploymentEndDatetime_UTC)==FALSE) {dat<-dat%>%filter(UTC_timestamp>deply_sel$DeploymentStartDatetime & UTC_timestamp<deply_sel$DeploymentEndDatetime_UTC)}
    if(nrow(dat)==0) next #skips data that was collected after a tag fell off the bird / bird died
    
    #remove columns not relevant for dive data
    dat<-dat%>%select(-satcount,-hdop,-Latitude,-Longitude,-MSL_altitude_m,-Reserved,
                      -U_bat_mV, -bat_soc_pct,-solar_I_mA,-speed_km.h,-altimeter_m,-milliseconds,
                      -direction_deg,-UTC_timestamp,-UTC_datetime,-UTC_date,-UTC_time,-UTC_time_ms,-Rtime,
                      -acc_x,-acc_y,-acc_z,-mag_x,-mag_y,-mag_z,-int_temperature_C)
    Birds_dpth<-rbind(Birds_dpth,dat)
  }
  saveRDS(Birds_dpth, paste0(usrdir,savedir,"Processed_Dive_Deployment_Data/",prjt[i],"_file_",k,"_DiveOnly.rds"))
  
  #if(nrow(Birds_dpth)==0) next
  rm(dat,Birds_dpth)
}
}


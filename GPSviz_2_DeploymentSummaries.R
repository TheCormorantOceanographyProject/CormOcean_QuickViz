library(dplyr)
library(lubridate)
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


# Project info ------------------------------------------------------------
prj_info<-read.csv(paste0(usrdir,"/data/Field Data/Project Titles and IDs.csv"))

#  Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
dm<-deploy_matrix%>%select(Bird_ID,Capture_Site,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC,TagManufacture)%>%
  filter(is.na(TagSerialNumber)==FALSE)
dm$DeployEnd<-1 #1 is still Tx
dm$DeployEnd[is.na(dm$DeploymentEndDatetime_UTC)==FALSE]<-0 #0 is finished

#all project names
prjt<-unique(deploy_matrix$Project_ID)
rm(deploy_matrix)

prjt<-prjt[prjt!="USACRBRDO14"]

indiSUM<-NULL
projSUM<-NULL

for (i in 1:length(prjt)){
  locs<-readRDS(paste0(usrdir,savedir,"Processed_GPS_Deployment_Data/",prjt[i],"_GPS_SpeedFiltered.rds"))
  names(locs)
  
  dm_pjt<-dm%>%filter(Project_ID==prjt[i])%>%filter(TagManufacture=="Ornitela")
  dm_pjt$TagSerialNumber<-as.numeric(dm_pjt$TagSerialNumber)
  
  (iS<-locs%>%group_by(Project_ID,device_id)%>%
    summarise(minDt=min(datetime),
              maxDt=max(datetime),
              nGPS=n())%>%
    mutate(dur=round(difftime(maxDt, minDt, units="days")), 
           durD=as.numeric(dur)))
  iS<-left_join(iS,dm_pjt%>%select(-Project_ID),by=c("device_id"="TagSerialNumber"))
  
  (pS<-iS%>%group_by(Project_ID)%>%
    summarise(nBirds=n_distinct(device_id),
              year=year(min(minDt)),
              minDt=min(date(minDt)),
              maxDt=max(date(maxDt)),
              uDur=round(mean(durD)),
              minDur=min(durD),
              maxDur=max(durD),
              GPSlocTotal=sum(nGPS),
              DayTotal=sum(durD),
              Tx=sum(DeployEnd)))
  
  indiSUM<-rbind(indiSUM, iS)
  projSUM<-rbind(projSUM, pS)
}

projSUM<-left_join(projSUM,prj_info,by="Project_ID")

write.csv(indiSUM, paste0(usrdir,savedir,"GPSdata_individualbirdSummary.csv"))
write.csv(projSUM, paste0(usrdir,savedir,"GPSdata_ProjectSummary.csv"))

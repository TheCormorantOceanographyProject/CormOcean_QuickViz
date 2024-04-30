# Save GPS only tag data to box

library(dplyr)
library(lubridate)
library(data.table) #rename
library(stringr)
library(R.utils)
library(tidyr)
library(ggplot2)
library(scales)


if(Sys.info()[7]=="alexa") {
  usrdir<-"/Users/alexa/Box Sync/DASHCAMS/"
  FTPdatadir<-'/Users/alexa/Box Sync/DASHCAMS/data/ornitela_ftp_data/'
  ATNdatadir<-'data/ornitela_for_ATN/'
  datadir<-'data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/alexa/git_repos/CormOcean_QuickViz/MakeDive.R')
}


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
prjt<-prjt[prjt!="USACRBRDO14"] #removes non-Ornitela Projects


for (i in 1:length(prjt)){
  
  Files<-list.files(paste0(usrdir,datadir,prjt[i],"/gps"), full.names = TRUE) 
  filenames<-list.files(paste0(usrdir,datadir,prjt[i],"/gps"))
  
  if (length(Files)==0) next
}

Files

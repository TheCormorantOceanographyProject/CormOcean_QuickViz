# Save GPS only tag data to box

library(tidyverse)
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
library(conflicted)
library(magrittr) 

conflicted::conflicts_prefer(dplyr::filter)


if(Sys.info()[7]=="alexa") {
  usrdir<-"/Users/alexa/Box Sync/DASHCAMS/"
  datadir<-'Analysis/DataViz/'
  savedir<-"/Users/alexa/Box Sync/Test/"
  deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/alexa/git_repos/CormOcean_QuickViz/MakeDive.R')
}


if(Sys.info()[7]=="rachaelorben") {
  usrdir<-"/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/"
  datadir<-'Analysis/DataViz/'
  savedir<-'/data/ornitela_for_ATN/'
  deplymatrix<-'/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

#  Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
dm<-deploy_matrix%>%dplyr::select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC,TagManufacture)%>%
  dplyr::filter(is.na(TagSerialNumber)==FALSE)
names(deploy_matrix)

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

#<<<<<<< HEAD
#HEAD
for (i in 1:length(prjt)){

  #Files<-list.files("/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/Analysis/DataViz/Processed_GPS_Deployment_Data", full.names = TRUE)
  Files<-list.files("/Users/alexa/Box Sync/DASHCAMS/Analysis/DataViz/Processed_GPS_Deployment_Data", full.names = TRUE)
}
#=======
#HEAD
#for (i in 1:length(prjt)){

#Files<-list.files(paste0(usrdir,datadir,"Processed_GPS_Deployment_Data"), full.names = TRUE)
#}
#>>>>>>> ed5f921c00c66b7b8d6d733c3c1c6a156aa61dc9

Files

#create project folders

for (i in 1:length(Files)){
  
  dat<-readRDS(Files[i])
  birdID<-unique(dat$device_id) # gets unique device ID as it loops through
  projID<-dat$Project_ID[1]  # gets the project ID 
  
  #this needs to be put into an if statement so it doesn't make the folder if it is already there. 
  
  for (j in length(projID)){
    if (!dir.exists(savedir)==projID[j]){
      dir.create(paste0(savedir,projID[j]))# create project folders
    } else {
      next
    }  

  for (k in 1:length(birdID)){
    birdy<-dat%>%filter(device_id==birdID[k])
    
    #missing userdir  
    write.csv(birdy,paste0(usrdir, savedir,"projID_ make project folder addresses here/", birdID[k],"_GPS.csv")) #need to fix this and pull the project ID to file correctly
    }
  }
}






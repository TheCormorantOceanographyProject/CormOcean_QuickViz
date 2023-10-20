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
Files<-list.files(paste0(usrdir,savedir,"Processed_Dive_Deployment_Data/"), full.names = TRUE)
filenames<-list.files(paste0(usrdir,savedir,"Processed_Dive_Deployment_Data/"))

for (i in 1:length(Files)){
  
Birds_dpth<-readRDS(Files[i])

# identify dives ----------------------------------------------------------
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
Birds_dpth_MD$ext_temperature_C<-Birds_dpth$ext_temperature_C
Birds_dpth_MD$conductivity_mS.cm<-Birds_dpth$conductivity_mS.cm

saveRDS(Birds_dpth_MD, paste0(usrdir,savedir,"Processed_DiveID_Deployment_Data/",prjt[i],"_DiveOnlyID.rds"))
}


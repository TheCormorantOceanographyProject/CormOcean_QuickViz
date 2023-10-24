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

#NO DIVE NOTES: i =1:6
#"USACRBRPE19"  "USAMIPE20"    "UAEBUSO20"    "UAESISO20"    "LITCUGR21"    "USAFIBR21" 
#DIVE NOTES: i =7
#BAHHASO21, USACRBR22 - possibly started midway through also NOTE_DIVING in datatype - not on and off

# Loop through each project -----------------------------------------------
for (i in 10:length(prjt)){
  
# Find Project Data Files -------------------------------------------------
Files<-list.files(paste0(usrdir,savedir,"Processed_Dive_Deployment_Data/"), pattern = prjt[i],full.names = TRUE)
filenames<-list.files(paste0(usrdir,savedir,"Processed_Dive_Deployment_Data/"),pattern = prjt[i],)

Birds_dpth<-NULL
for (k in 1:length(Files)){
  birdy_d<-readRDS(paste0(usrdir,savedir,"Processed_Dive_Deployment_Data/",prjt[i],"_file_",k,"_DiveOnly.rds"))
  Birds_dpth<-rbind(Birds_dpth,birdy_d)
}
rm(birdy_d)

prjt[i]
unique(Birds_dpth$datatype)

# identify dives (by individual) ----------------------------------------------------------
IDS<-unique(Birds_dpth$device_id)
  #191996 too big? (j=7), nope! 11668747 lines processed OK
for (j in 1:length(IDS)){
  birdy_d<-Birds_dpth%>%filter(device_id==IDS[j])
  unique(birdy_d$datatype) 
  
  birdy_d$tdiff_sec <-difftime(birdy_d$datetime, lag(birdy_d$datetime, 1),units = "secs")

id_num <- which(colnames(birdy_d) == "device_id") 
dt_num <- which(colnames(birdy_d) == "datetime") 
dp_num <- which(colnames(birdy_d) == "depth_m") 
td_num <- which(colnames(birdy_d) == "tdiff_sec") 

birdy_d_MD<-MakeDive(birdy_d,idCol=id_num, #column index with unique ID
                        dtCol=dt_num, #column index with datetime
                        depthCol=dp_num, #column index with depth
                        tdiffCol=td_num, #column index with time difference in seconds
                        DepthCutOff=1, #depth that dives happen below (meters)
                        DiveDepthYes=1.2, #dives need to reach 1.2 meters to be considered a dive event
                        TimeDiffAllowed_sec=2, #consecutive points need to have a time difference <2 to be in the same event
                        NumLocCut=2) #dives need to contain three points to be considered a dive, could change this to a duration

#names(Birds_dpth)
birdy_d_MD$date<-date(birdy_d$datetime)
birdy_d_MD$datatype<-birdy_d$datatype
birdy_d_MD$ext_temperature_C<-birdy_d$ext_temperature_C
birdy_d_MD$conductivity_mS.cm<-birdy_d$conductivity_mS.cm

saveRDS(birdy_d_MD, paste0(usrdir,savedir,"Processed_DiveID_Deployment_Data/",prjt[i],"_",IDS[j],"_DiveOnlyID_DiveNote.rds"))
}
}


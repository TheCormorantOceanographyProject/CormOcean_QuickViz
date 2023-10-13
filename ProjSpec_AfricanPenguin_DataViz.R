library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table) #rename
library(stringr)
#library(argosfilter)
library(R.utils)
library(tidyr)

# Link to local Box Sync folder ---- 
#To find user/computer specific username use: Sys.getenv("LOGNAME")

if(Sys.info()[4]=="benthos") {
  args = commandArgs(trailingOnly=TRUE)
  datadir<-args[1] #/home/DASHCAMS/data/ornitela_ftp_data/
  deplymatrix<-args[2] #/home/DASHCAMS/data_raw/metadata/DASHCAMS_Deployment_Field_data.csv
  savedir<-args[3] #/home/DASHCAMS/zTagStatus/
  source('/home/DASHCAMS/git/CormOcean/MakeDive.R') 
}

#if(Sys.info()[4]=="benthos") {
#  datadir<-'/home/DASHCAMS/data_raw/ornitela_ftp_data/'
#  savedir<-'/home/DASHCAMS/data_processed/zTagStatus/'
#  deplymatrix<-'/home/DASHCAMS/data_raw/metadata/DASHCAMS_Deployment_Field_data.csv'
#}

if(Sys.info()[7]=="rachaelorben") {
  datadir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/data/ornitela_ftp_data/'
  savedir<-'/Users/rachaelorben/zTagStatus/'
  deplymatrix<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}


#  Pulls in deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(deplymatrix)
#str(deploy_matrix)
deploy_matrix<-deploy_matrix%>%filter(TagManufacture=="Ornitela")%>%
  select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short)%>%
  filter(is.na(TagSerialNumber)==FALSE)
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)


# finds files from deployment specified -----------------------------------
dm<-deploy_matrix%>%filter(Project_ID=="SOUDIAP23")

tag_ids<-dm$TagSerialNumber
Files<-list.files(datadir,pattern = ".csv",full.names = FALSE)

my_files <- fileSnapshot(path=datadir)
Files1<-rownames(my_files$info[1])[which(my_files$info[1] > 309)] #selects files with >309 bytes (1 header row)

Tags <- sapply(strsplit(Files, split='_', fixed=TRUE), function(x) (x[1]))

sel_files<-Files1[Tags %in%  tag_ids]

# Cycles through selected data files ----------------------------------------------
Birds<-NULL 
for (i in 1:length(sel_files)){
  
  fileN<-sel_files[i]
  tagID<-sapply(strsplit(sel_files[i], split='_', fixed=TRUE), function(x) (x[1]))
  
  dat<-read.csv(file = paste0(datadir,sel_files[i]),sep = ",") #could switch to fread to be quicker...
  
  dat<-rename(dat,lat="Latitude")
  dat<-rename(dat,lon="Longitude")
  #dat<-rename(dat,UTC_timestamp="UTC_datetime")
  dat[is.na(dat)==TRUE]<-NA
  
  dat$datetime<-ymd_hms(dat$UTC_timestamp)
  dat$Foid<-1:nrow(dat)
  Birds<-rbind(Birds,dat)
}


#remove duplicate GPS points 
Birds<-Birds%>%group_by(device_id)%>%
  distinct(device_id,datetime, .keep_all = TRUE)%>%
  arrange(datetime) #arranges by time, could scramble data >1HZ a little bit


# identify dives ----------------------------------------------------------
Birds_dpth<-Birds%>%filter(is.na(depth_m)==FALSE)
Birds_dpth$tdiff_sec <-difftime(Birds_dpth$datetime, lag(Birds_dpth$datetime, 1),units = "secs")

id_num <- which(colnames(Birds_dpth) == "device_id") 
dt_num <- which(colnames(Birds_dpth) == "datetime") 
dp_num <- which(colnames(Birds_dpth) == "depth_m") 
td_num <- which(colnames(Birds_dpth) == "tdiff_sec") 

Birds_dpth<-MakeDive(Birds_dpth,idCol=id_num, #column index with unique ID
                     dtCol=dt_num, #column index with datetime
                     depthCol=dp_num, #column index with depth
                     tdiffCol=td_num, #column index with time difference in seconds
                     DepthCutOff=1, #depth that dives happen below (meters)
                     DiveDepthYes=1, #dives need to reach 3 meters to be considered a dive event
                     TimeDiffAllowed_sec=2, #consecutive points need to have a time difference <2 to be in the same event
                     NumLocCut=3) #dives need to contain three points to be considered a dive, could change this to a duration



# plots -------------------------------------------------------------------
names(Birds_dpth)
unique(Birds_dpth$ID)
ggplot()+
  geom_point(data=Birds_dpth%>%
               filter(ID==225642)%>%
               filter(datetime>"2023-08-29 12:00:00"),
             aes(x=datetime,y=-depth, color=as.factor(divedatYN)))+
  geom_path(data=Birds_dpth%>%
               filter(ID==225642)%>%
               filter(datetime>"2023-08-29 12:00:00"),
             aes(x=datetime,y=-depth, group=divedatYN,color=as.factor(divedatYN)))+
  geom_hline(yintercept = -1)+
  geom_hline(yintercept = -2)+
  geom_hline(yintercept = -3)+
  theme(legend.position = "none")

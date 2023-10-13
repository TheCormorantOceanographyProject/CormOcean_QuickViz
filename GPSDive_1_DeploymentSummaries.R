library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table) #rename
library(stringr)
#library(argosfilter)
library(R.utils)
library(tidyr)

# pulls in all .csv files from Ornitela
# annotates birds, deployments, and studies 
#    1. trims to deployment 
#    2. removes failed sensor data

# Filters GPS data from each bird
#   1. removes 0,0 locations first
#   2. speed filters GPS data - 80km/hr?

# Finds and annotates dives on a monthly basis
#   1. dives noted as mo_diveID

# Save .rda files for each study


# SetUp -------------------------------------------------------------------
# Link to local Box Sync folder ---- 
#To find user/computer specific username use: Sys.getenv("LOGNAME")

if(Sys.info()[4]=="benthos") {
  args = commandArgs(trailingOnly=TRUE)
  datadir<-args[1] #/home/DASHCAMS/data/ornitela_ftp_data/
  deplymatrix<-args[2] #/home/DASHCAMS/data_raw/metadata/DASHCAMS_Deployment_Field_data.csv
  savedir<-args[3] #/home/DASHCAMS/zTagStatus/
  source('/home/DASHCAMS/git/CormOcean/MakeDive.R') 
}

if(Sys.info()[7]=="rachaelorben") {
  datadir<-'/Users/rachaelorben/Box/DASHCAMS/data/ornitela_ftp_data/'
  savedir<-'/Users/rachaelorben/Box/DASHCAMS/data/Processed/'
  deplymatrix<-'/Users/rachaelorben/Box/DASHCAMS/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}


#  Pulls in deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(deplymatrix)
#str(deploy_matrix)
deploy_matrix<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short)%>%
  filter(is.na(TagSerialNumber)==FALSE)
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)

# Find file names with data -------------------------------------------

my_files <- fileSnapshot(path=datadir)
Files<-rownames(my_files$info[1])
Files1<-rownames(my_files$info[1])[which(my_files$info[1] < 309)] #selects files with >309 bytes (1 header row)
new_files<-Files[Files %in% Files1 == FALSE] #I think this should remove empty files written within last two weeks

injest_files<-readRDS(paste0(savedir,"Ornitela_GPSDive_allbirds_Files.rds"))
sel_files1<-new_files[new_files %in% injest_files == FALSE] 
all_files1<-new_files[new_files %in% injest_files == TRUE] 


#saveRDS(sel_files,paste0(savedir,"Ornitela_GPSDive_allbirds_Files.rds"))



# Pulls in ALL data -------------------------------------------------------
#Birds<-NULL 
#Birds<-readRDS(paste0(savedir,"Ornitela_GPSDive_allbirds.rds"))

for (i in 1:length(sel_files)){
  
  fileN<-sel_files[i]
  tagID<-sapply(strsplit(sel_files[i], split='_', fixed=TRUE), function(x) (x[1]))
  
  deply_sel<-deploy_matrix[deploy_matrix$TagSerialNumber==tagID[1],]
  deply_sel<-deply_sel%>%filter(is.na(DeploymentStartDatetime)==FALSE)
  n<-nrow(deply_sel)
  
  if(n==0) next #if the tag isn't in the deployment matrix is will be skipped - important for skipping testing tags etc. 
  
  dat<-read.csv(file = paste0(datadir,sel_files[i]),sep = ",") #could switch to fread to be quicker...

  dat<-rename(dat,lat="Latitude")
  dat<-rename(dat,lon="Longitude")
  dat[is.na(dat)==TRUE]<-NA
  
  dat<-dat[, !names(dat) %in% c("mag_x", "mag_y", "mag_z", "acc_x", "acc_y", "acc_z")]
  
  if("depth_m" %in% names(dat)==FALSE) dat$depth_m<-NA
  dat<-dat[!with(dat,is.na(lat)& is.na(depth_m)),]
  dat<-dat[!with(dat,lat==0 & lon==0 & is.na(depth_m)),]
  dat<-janitor::remove_empty(dat, which = "rows")#removes rows with all NAs
  if(nrow(dat)==0) next
  
  dat$datetime<-ymd_hms(paste(dat$UTC_date, dat$UTC_time))
 
  #tries to associate tracking data with the correct deployment of the tag
  bid_num<-unique(deply_sel$Bird_ID)
  if(length(bid_num)>1) deply_sel<-deply_sel%>%filter(DeploymentStartDatetime<dat$UTC_datetime[1])
  bid_num<-unique(deply_sel$Bird_ID)
  if(length(bid_num)>1) deply_sel$tag_time=dat$datetime[1]; 
  if(length(bid_num)>1) deply_sel$tdiff<-abs(as.numeric(difftime(time1 =  deply_sel$tag_time,time2 = deply_sel$DeploymentStartDatetime,units = "days"))) 
  if(length(bid_num)==2) deply_sel%>%filter(tdiff>0)
  if(length(bid_num)==1) deply_sel<-deply_sel[1,]
  if(length(bid_num)==2) deply_sel<-deply_sel[2,]
  if(length(bid_num)==3) deply_sel<-deply_sel[3,]
  bid_num<-unique(deply_sel$Bird_ID)
  if(length(bid_num)>1) break
  
  dat$Project_ID<-deply_sel$Project_ID
  dat$tagID<-tagID
  dat$DeployEndShort<-deply_sel$Deployment_End_Short
  
  dat$Foid<-1:nrow(dat)
  
  dat_gps<-dat%>%filter(!is.na(lat))
  
  if(nrow(dat_gps)==0) dat$GPS_surfacedrifts<-0 #adds the annotation column for surface drift points
  if(nrow(dat_gps)==0) Birds<-bind_rows(Birds,dat) #if there is no GPS data in the file this adds it to Birds 
  if(nrow(dat_gps)==0) next # and skips to the next iteration
  
  dat_gps$PointDur <- NA
  
  # force difftime in secs & finds surface drifts (diff of <2 for more than 5 seconds)
  dat_gps$PointDur <- abs(as.numeric(difftime(time1 =  dat_gps$datetime,
                                              time2 = lead(dat_gps$datetime),
                                              units = "secs")))
  
  out <- data.frame(unclass(rle(dat_gps$PointDur<=2)))
  out$pos <- head(cumsum(c(1, out$lengths)), -1)
  out.s<-out[out$lengths>=5  & out$values,c("pos", "lengths")]
  
  dat$GPS_surfacedrifts<-0
  for (j in 1:nrow(out.s)){
    if (nrow(out.s)==0) next
    info<-out.s[j,]
    idx1<-dat_gps$Foid[info$pos]
    idx2<-dat_gps$Foid[info$pos+info$lengths]
    dat$GPS_surfacedrifts[idx1:idx2]<-1
  }
  Birds<-bind_rows(Birds,dat)
}
saveRDS(Birds, paste0(savedir,"Ornitela_GPSDive_allbirds.rds"))

#remove duplicate GPS points 
Birds<-Birds%>%group_by(device_id,datetime)%>%
  distinct(device_id,datetime, .keep_all = TRUE)%>%
  arrange(datetime) #arranges by time, could scramble data >1HZ a little bit

Birds%>%group_by(Project_ID)%>%
  summarize(n=n_distinct(tagID))

# identify dives ----------------------------------------------------------
Birds<-readRDS(paste0(savedir,"Ornitela_GPSDive_allbirds.rds"))
Prjs<-unique(Birds$Project_ID)
#Birds<-Birds%>%filter(Project_ID!=Prjs[i])

for (i in 1:length(Prjs)){
  birdies<-Birds%>%filter(Project_ID==Prjs[i])

  Birds_Nodpth<-birdies%>%filter(is.na(depth_m)==TRUE)
  Birds_dpth<-birdies%>%filter(is.na(depth_m)==FALSE)
  Birds_dpth$tdiff_sec <-difftime(Birds_dpth$datetime, lag(Birds_dpth$datetime, 1),units = "secs")

id_num <- which(colnames(Birds_dpth) == "tagID") 
dt_num <- which(colnames(Birds_dpth) == "datetime") 
dp_num <- which(colnames(Birds_dpth) == "depth_m") 
td_num <- which(colnames(Birds_dpth) == "tdiff_sec") 

Birds_dpth<-MakeDive(Birds_dpth,idCol=id_num, #column index with unique ID
                     dtCol=dt_num, #column index with datetime
                     depthCol=dp_num, #column index with depth
                     tdiffCol=td_num, #column index with time difference in seconds
                     DepthCutOff=1, #depth that dives happen below (meters)
                     DiveDepthYes=3, #dives need to reach 3 meters to be considered a dive event
                     TimeDiffAllowed_sec=2, #consecutive points need to have a time difference <2 to be in the same event
                     NumLocCut=3) #dives need to contain three points to be considered a dive, could change this to a duration

Birds_dpth$date<-date(Birds_dpth$datetime)
Birds_dpth$Project_ID<-Prjs[i]
saveRDS(Birds_dpth, paste0(savedir,"annotated_study/",Prjs[i],"_annotatedDives.rds"))

# quick summary of the bird data
sumdives<-Birds_dpth%>%group_by(Project_ID,ID)%>%
  summarise(nDepth=n_distinct(divedatYN))
write.table(sumdives,paste0(savedir,"annotated_study/AnnotatedDives.csv"),sep = ',',append = TRUE)
Birds<-Birds%>%filter(Project_ID!=Prjs[i])
}

dsum<-Birds_dpth%>%group_by(ID,date)%>%
  summarise(n=n_distinct(divedatYN))

dsum_weekly<-dsum%>%group_by(ID)%>%
  summarize(udives_day=floor(mean(n)))

# quick summary of the bird data
PROJ_dat%>%group_by(ID)%>%
  summarise(nDepth=n_distinct(divedatYN))
            
sumDat<-PROJ_dat%>%group_by(Project_ID,tagID,device_id)%>%
  summarise(minDt=min(datetime),
            maxDt=max(datetime),
            maxDepth=max(depth_m,na.rm=TRUE),
            uDepth=round(mean(depth_m,na.rm=TRUE),2),
            nDepth=n_distinct(divedatYN),
            n_GPS=n_distinct(lat),
            uBat=round(mean(U_bat_mV,na.rm=TRUE)),
            uTemp=round(mean(ext_temperature_C,na.rm=TRUE),2),
            uCond=round(mean(conductivity_mS.cm,na.rm=TRUE),2),
            GPS_surfacedrift_pts=sum(GPS_surfacedrifts))%>%
  mutate(dur=round(maxDt-minDt,2)) 

sumDat<-left_join(sumDat,dsum_weekly,by=c("tagID"="ID"))


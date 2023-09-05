library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table) #rename
library(stringr)
library(R.utils)
library(tidyr)

#sandbox for real-time CTD profile processing

#DATA: FTP data from Ornitela

if(Sys.info()[7]=="rachaelorben") {
  datadir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/data/ornitela_ftp_data/'
  savedir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box//DASHCAMS/Analysis/realtime_CTD_ATNtoGTS/profiles/'
  deplymatrix<-'/Users/rachaelorben/Library/CloudStorage/Box-Box//DASHCAMS/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

# Find file names with data within last 12-hr-------------------------------------------
my_files <- fileSnapshot(path=datadir)
Files1<-rownames(my_files$info[1])[which(my_files$info[1] < 309)] #selects files with >309 bytes (1 header row)

now<-Sys.time()
tz_str<-Sys.timezone() #adds in system timezone
hr6<-now-(43200) #12hr in seconds
dates<-my_files$info[5]
dates$datetime<-ymd_hms(dates$ctime,tz = tz_str)

IDx<-which(dates$datetime>hr6)
Files2<-rownames(my_files$info[5])[IDx]#removes all files not written in last 6hr
Files<-Files2[Files2 %in% Files1 == FALSE] #I think this should remove empty files written within last 6hr

# Cycles through data files to find data written in last 6 hr------------
sel_files<-NULL
for (i in 1:length(Files)){
  nL <- countLines(paste0(datadir,Files[i]))
  dfh <- read.csv(paste0(datadir,Files[i]), header=TRUE, nrows = 0,  skipNul=TRUE)
  df<-dfh[nL-1,]
  #df <- read.csv(paste0(datadir,Files[i]), header=FALSE, skip=nL-1)
  names(df)<-names(dfh)
  info<-str_locate(df$UTC_datetime[1], "/")
  if(is.na(info[1])==FALSE){df$UTC_datetime<-mdy_hm(df$UTC_datetime)}
  if(is.na(info[1])==TRUE){df$UTC_datetime<-ymd_hms(df$UTC_datetime)}
  today<-.POSIXct(Sys.time(), "UTC")
  if(df$UTC_datetime[1]>today-43200){sel_files<-c(sel_files,Files[i])} #selects files with a last date within 7 days of today
}

sel_files_IDs<-as.numeric(sapply(strsplit(sel_files, split='_', fixed=TRUE), function(x) (x[1])))

#  Pulls in deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(deplymatrix)
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
deploy_matrix<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC)%>%
  filter(is.na(TagSerialNumber)==FALSE)
deploy_matrix_now<-deploy_matrix%>%filter(is.na(DeploymentEndDatetime_UTC==TRUE))


# deployment check --------------------------------------------------------
 #deployment end dates need to be up-to-date
 #figure out better system for recaptured tags

IDx<-which(sel_files_IDs %in% deploy_matrix_now$TagSerialNumber)
sel_files_dply<-sel_files[IDx] #selects the file names with IDs that are currently deployed

#224066 Sri Lanka
#227351 & 227357 Korea

# Cycles through selected data files ----------------------------------------------
#lots of extra stuff in here that needs to be cleaned out
Birds<-NULL 
for (i in 1:length(sel_files_dply)){
  
  fileN<-sel_files[i]
  tagID<-sapply(strsplit(sel_files_dply[i], split='_', fixed=TRUE), function(x) (x[1]))
  
  deply_sel<-deploy_matrix[deploy_matrix$TagSerialNumber==tagID[1],]
  
  n<-nrow(deply_sel)
  #if(n==0) next #if the tag isn't in the deployment matrix is will be skipped - important for skipping testing tags etc. 
  
  deply_sel<-deply_sel[n,] #picks the most recent deployment of that tag
  dat<-read.csv(file = paste0(datadir,sel_files[i]),sep = ",") #could switch to fread to be quicker...
  
  #deply_sel[deply_sel$DeploymentEndDatetime_UTC<deply_sel$DeploymentEndDatetime_UTC,]
  
  #if(is.na(deply_sel$DeploymentEndDatetime_UTC)==FALSE) {deply_sel<-deply_sel[deply_sel$DeploymentEndDatetime_UTC<deply_sel$DeploymentEndDatetime_UTC,]}
  #if(nrow(deply_sel)==0) next
  
  dat$Project_ID<-deply_sel$Project_ID
  dat$tagID<-tagID
  dat$DeployEndShort<-deply_sel$Deployment_End_Short
  
  dat<-rename(dat,lat="Latitude")
  dat<-rename(dat,lon="Longitude")
  dat[is.na(dat)==TRUE]<-NA
  
  dat$datetime<-ymd_hms(dat$UTC_timestamp)
  dat$Foid<-1:nrow(dat)
  
  # dat_gps<-dat%>%filter(!is.na(lat))
  # if (nrow(dat_gps)==0)   {today<-Sys.time(); dat$GPS_surfacedrifts<-NA
  # dat_sel<-dat[dat$datetime>(today-604800),] #trims to last week of data
  # Birds<-rbind(Birds,dat_sel)}
  # if (nrow(dat_gps)==0) next
  # 
  # dat_gps$PointDur <- NA
  # 
  # # force difftime in secs & finds surface drifts (diff of <2 for more than 5 seconds)
  # dat_gps$PointDur <- abs(as.numeric(difftime(time1 =  dat_gps$datetime,
  #                                             time2 = lead(dat_gps$datetime),
  #                                             units = "secs")))
  # 
  # out <- data.frame(unclass(rle(dat_gps$PointDur<=2)))
  # out$pos <- head(cumsum(c(1, out$lengths)), -1)
  # out.s<-out[out$lengths>=5  & out$values,c("pos", "lengths")]
  # 
  # dat$GPS_surfacedrifts<-0
  # for (j in 1:nrow(out.s)){
  #   if (nrow(out.s)==0) next
  #   info<-out.s[j,]
  #   idx1<-dat_gps$Foid[info$pos]
  #   idx2<-dat_gps$Foid[info$pos+info$lengths]
  #   dat$GPS_surfacedrifts[idx1:idx2]<-1
  # }
  
  today<-Sys.time()
  dat_sel<-dat[dat$datetime>(today-604800),] #trims to last week of data
  Birds<-rbind(Birds,dat_sel)
}
names(Birds)

#finds birds with dive data: filter(depth_m>0)
birdsWdives<-Birds%>%group_by(device_id)%>%filter(depth_m>0)%>%summarise(n=n())
#selects just birds with dive data via device ID
birdies<-Birds[which(Birds$device_id %in% birdsWdives$device_id),]

# identify dives ----------------------------------------------------------
Birds_dpth<-birdies%>%filter(is.na(depth_m)==FALSE)
Birds_dpth$tdiff_sec <-difftime(Birds_dpth$datetime, lag(Birds_dpth$datetime, 1),units = "secs")

id_num <- which(colnames(Birds_dpth) == "tagID") 
dt_num <- which(colnames(Birds_dpth) == "datetime") 
dp_num <- which(colnames(Birds_dpth) == "depth_m") 
td_num <- which(colnames(Birds_dpth) == "tdiff_sec") 

Birds_dpth_ids<-MakeDive(Birds_dpth,idCol=id_num, #column index with unique ID
                     dtCol=dt_num, #column index with datetime
                     depthCol=dp_num, #column index with depth
                     tdiffCol=td_num, #column index with time difference in seconds
                     DepthCutOff=1, #depth that dives happen below (meters)
                     DiveDepthYes=2, #dives need to reach 1 meters to be considered a dive event
                     TimeDiffAllowed_sec=2, #consecutive points need to have a time difference <2 to be in the same event
                     NumLocCut=3) #dives need to contain three points to be considered a dive, could change this to a duration

Birds_dpth_ids$date<-date(Birds_dpth_ids$datetime)

(dsum<-Birds_dpth_ids%>%group_by(ID,date)%>%
  summarise(n=n_distinct(divedatYN)))


# GPS clean ---------------------------------------------------------------

gps<-Birds%>%
  filter(!is.na(lat))%>% #removes NA
  filter(lat!=0 & lon!=0)%>% #removes (0,0)
  filter(datatype=="GPSD") #finds all post dive GPS locations
gps$tdiff_sec <-round(difftime(gps$datetime, lag(gps$datetime, 1),units = "secs"),2)

names(gps)

#identify and sequentially number GPS Dive bursts (typically 2 GPS fixes)
# Find consecutively recorded dive data
gps_burstID<- gps %>% group_by(device_id)%>%
  mutate(gap_time=tdiff_sec>2, # find times when there is a gap > 2sec
         gap_time=ifelse(is.na(gap_time),0,gap_time), #fill NAs
         gpsDiveburstID=(cumsum(gap_time)))#, # gap_time is T/F so cumsum is adding 1

#sequentially numbers each row in each burst
gps_burstID<-gps_burstID%>%group_by(device_id,gpsDiveburstID)%>%
  mutate(gpsNum=row_number())



# find max depth to surface segments --------------------------------------
names(Birds_dpth_ids)
S<-Birds_dpth_ids%>%group_by(ID, divedatYN)%>%filter(!is.na(divedatYN))%>%
  summarise(max=max(depth),max_oid=which.max(depth),startT=min(datetime),endT=max(datetime))%>%
  mutate(duration=endT-startT)


# conversion to ATN csv format (platformQC)--------------------------------------------
  # this is an attempt to join the ATN workflow between the logger and QC stage. 
  # The Ornitela logger data would be annoying to convert to SMRU GPS data format and the QCd data from these tags is closer to the Ornitela format. 
  # Also, some of the QC for argos data & cormorants might be different. 

Tonly<-read.csv("/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/Analysis/realtime_CTD_ATNtoGTS/SMRU GPS/platformQC-gp26-416C-20.csv")
CTD<-read.csv("/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/Analysis/realtime_CTD_ATNtoGTS/SMRU CTD/platformQC-ft32-TSL-727-22.csv")

names(Tonly)
names(CTD)



library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table) #rename
library(stringr)
library(R.utils)
library(tidyr)
library(zoo)#dive phase
library(pracma)#matlab style gradient function

#sandbox for real-time CTD profile processing

#DATA: FTP data from Ornitela

if(Sys.info()[7]=="rachaelorben") {
  datadir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/data/ornitela_ftp_data/'
  savedir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box//DASHCAMS/Analysis/realtime_CTD_ATNtoGTS/profiles/'
  deplymatrix<-'/Users/rachaelorben/Library/CloudStorage/Box-Box//DASHCAMS/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

op=options(digits.secs=3) # option for decimals in date time variables

# Find file names with data within last 24-hr-------------------------------------------
my_files <- fileSnapshot(path=datadir)
Files1<-rownames(my_files$info[1])[which(my_files$info[1] < 309)] #selects files with >309 bytes (1 header row)

now<-Sys.time()
tz_str<-Sys.timezone() #adds in system timezone
hr6<-now-(86400) #24hr in seconds
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
  if(df$UTC_datetime[1]>today-86400){sel_files<-c(sel_files,Files[i])} #selects files with a last date within 24hr of today
}

sel_files_IDs<-as.numeric(sapply(strsplit(sel_files, split='_', fixed=TRUE), function(x) (x[1])))

#  Pulls in deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(deplymatrix)
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
deploy_matrix<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC)%>%
  filter(is.na(TagSerialNumber)==FALSE)
deploy_matrix_now<-deploy_matrix%>%filter(is.na(DeploymentEndDatetime_UTC==TRUE))


# Deployment check --------------------------------------------------------

IDx<-which(sel_files_IDs %in% deploy_matrix_now$TagSerialNumber)
sel_files_dply<-sel_files[IDx] #selects the file names with IDs that are currently deployed

#224066 Sri Lanka
#227351 & 227357 Korea

# Cycles through selected data files ----------------------------------------------

Birds<-NULL 
for (i in 1:length(sel_files_dply)){
  
  tagID<-sapply(strsplit(sel_files_dply[i], split='_', fixed=TRUE), function(x) (x[1]))
  deply_sel<-deploy_matrix[deploy_matrix$TagSerialNumber==tagID,]
  n<-nrow(deply_sel)
  if(nrow(deply_sel)==0) next #if the tag isn't in the deployment matrix is will be skipped - important for skipping testing tags etc. 
  
  deply_sel<-deply_sel[n,] #picks the most recent deployment of that tag
  dat<-read.csv(file = paste0(datadir,sel_files[i]),sep = ",") #could switch to fread to be quicker...
  
  dat$Project_ID<-deply_sel$Project_ID
  dat$DeployEndShort<-deply_sel$Deployment_End_Short
  
  dat<-rename(dat,lat="Latitude")
  dat<-rename(dat,lon="Longitude")
  dat[is.na(dat)==TRUE]<-NA
  
  dat$datetime<-ymd_hms(dat$UTC_timestamp)
  
  today<-Sys.time()
  dat_sel<-dat[dat$datetime>(today-86400),] #trims to last 24hr of data
  Birds<-rbind(Birds,dat_sel)
}
names(Birds)

#finds birds with dive data: filter(depth_m>0)
birdsWdives<-Birds%>%group_by(device_id)%>%filter(depth_m>0)%>%summarise(n=n())

#selects just birds with dive data via device ID
birdies<-Birds[which(Birds$device_id %in% birdsWdives$device_id),]
unique(birdies$device_id)

# Identify dives (deeper than 2m, longer than 3 sec)----------------------------------------------------------
Birds_dpth<-birdies%>%filter(is.na(depth_m)==FALSE)
Birds_dpth$tdiff_sec <-as.numeric(round(difftime(Birds_dpth$datetime, lag(Birds_dpth$datetime),units = "secs"),2))
Birds_dpth$Doid<-1:nrow(Birds_dpth)

id_num <- which(colnames(Birds_dpth) == "device_id") 
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

#adds temp and conductivity data back into dive dataset
Birds_dpth_ids<-cbind(Birds_dpth_ids,Birds_dpth%>%
                        select(Doid,conductivity_mS.cm,ext_temperature_C))

(dsum<-Birds_dpth_ids%>%group_by(ID)%>%
  summarise(n=n_distinct(divedatYN)))



# Label Dive Phases (gradient threshold of acc) --------------------------------------
#thresholds
vmax = 0.4;
amax = 0.5;
dmin = 2;

Birds_dpth<-Birds_dpth_ids%>%filter(is.na(divedatYN)==FALSE) #filters out depth values not assigned to a dive
nrow(Birds_dpth_ids)-nrow(Birds_dpth) #depth data removed

Birds_dpth$DiveID<-paste0(Birds_dpth$divedatYN,"_",Birds_dpth$ID)

diveIDs<-unique(Birds_dpth$DiveID)

DIVE<-data.frame()
for (i in 1:length(diveIDs)){

  dive<-Birds_dpth%>%filter(DiveID==diveIDs[i])
  #dive<-Birds_dpth%>%filter(DiveID=="23_231684")

  dive$Doid<-1:nrow(dive)
  
  if(is.na(dive$tdiff[1])==TRUE){dive$tdiff[1]<-0} #first dives get NA and this switches that to 0

  dive$time<-cumsum(dive$tdiff)-dive$tdiff[1]

  dt = gradient(dive$time)
  dive$spd = abs(gradient(dive$depth)/dt);
  dive$acc = abs(gradient(dive$spd)/dt);

  # Find segments within thresholds
  mask = dive$spd < vmax & dive$acc < amax & dive$depth >= dmin;
  dive$bottom<-mask

  dive<-dive%>% mutate(depth_diff=diff(as.zoo(depth), lag=1, na.pad=T))
  dive$depth_diff<-as.numeric(dive$depth_diff) #gets rid of zoo object formatting right away
  
  #rough identification of phases
  dive$phase<-"descent" #all records start as descent
  dive$phase[dive$bottom==TRUE]<-"bottom"
  dive$phase[dive$bottom==FALSE & dive$depth_diff<0]<-"profile" 
  
  #for dives without all the phases
  if(length(unique(dive$phase))<3){DIVE<-rbind(DIVE,dive)} 
  if(length(unique(dive$phase))<3) next

  db<-dive[dive$bottom==TRUE,]
  mi<-which(dive$oid==min(db$oid))
  ma<-which(dive$oid==max(db$oid))
  dive$phase[mi:ma]<-"bottom"
  
  dive%>%select(depth,time,phase)
  
  #assigns bottom point to profile if it is deeper than following depth
  #this process repeats itself until the depth is shallower
  mptB <- dive %>%
    group_by(phase) %>%
    filter(time == max(time)) %>%
    filter(phase=="bottom")
  
  while(dive$depth[mptB$Doid]>dive$depth[mptB$Doid+1]){
  #finds indexes of max time value for each phase
  mptB <- dive %>%
    group_by(phase) %>%
    filter(time == max(time)) %>%
    filter(phase=="bottom")
  if(nrow(mptB)==0) break
  if(is.na(dive$phase[mptB$Doid])==TRUE) break
  if(dive$depth[mptB$Doid]>dive$depth[mptB$Doid+1]){dive$phase[mptB$Doid]<-"profile"}
  }
  
  DIVE<-rbind(DIVE,dive)
}

# dive phase plots --------------------------------------------------------
unique(DIVE$ID)
ggplot()+
  geom_point(data=DIVE%>%filter(ID=="231684"),
             aes(x=datetime,y=-depth,color=phase))+
  facet_wrap(~DiveID, scales="free")

#TO DO - pagenate and save these plots as a pdf. 

# conversion to ATN csv format --------------------------------------------
  # this is an attempt to join the ATN workflow between the logger and QC stage. 
  # The Ornitela logger data would be annoying to convert to SMRU GPS data format and the QCd data from these tags is closer to the Ornitela format. 
  # Also, some of the QC for argos data & cormorants might be different. 

Tonly<-read.csv("/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/Analysis/realtime_CTD_ATNtoGTS/SMRU GPS/platformQC-gp26-416C-20.csv")
CTD<-read.csv("/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/Analysis/realtime_CTD_ATNtoGTS/SMRU CTD/platformQC-ft32-TSL-727-22.csv")

Tonly<-Tonly%>%select(-max_dbar,-cnt,-num,-n_temp,-n_photo,-n_cond,-n_sal,-n_fluoro,-n_oxy)

names(CTD)
head(Tonly, 20)
Asc<-DIVE%>%filter(phase=="ascent")%>%
  select(-oid,-tdiff,-divedatYN,-time,-phase,-spd,-acc,-bottom,-depth_diff,-Doid)
Asc<-Asc %>% 
  dplyr::group_by(DiveID) %>%
  dplyr::mutate(datetime = max(datetime, na.rm=T))

head(Asc)
names(Asc)
Asc<-rename(Asc,platform_id=ID)
Asc<-rename(Asc,profile=DiveID)
Asc<-rename(Asc,ext_temperature_C=temperature)
Asc$ptt<-Asc$platform_id

# GPS clean ---------------------------------------------------------------
#TO DO: GPS speed filter goes here
gps<-Birds%>%
  filter(!is.na(lat))%>% #removes NA
  filter(lat!=0 & lon!=0)%>% #removes (0,0)
  filter(datatype=="GPSD") #finds all post dive GPS locations
gps$tdiff_sec <-round(difftime(gps$datetime, lag(gps$datetime, 1),units = "secs"),2)

names(gps)

#identify and sequentially number GPS Dive bursts (typically 2 GPS fixes)
# Find consecutively recorded GPS data
gps_burstID<- gps %>% group_by(device_id)%>%
  mutate(gap_time=tdiff_sec>2, # find times when there is a gap > 2sec
         gap_time=ifelse(is.na(gap_time),0,gap_time), #fill NAs
         gpsDiveburstID=(cumsum(gap_time)))#, # gap_time is T/F so cumsum is adding 1

#sequentially numbers each row in each burst
gps_burstID<-gps_burstID%>%group_by(device_id,gpsDiveburstID)%>%
  mutate(gpsNum=row_number())



# find GPS point for each profile -----------------------------------------
gps_burstID_sel<-gps_burstID%>%
       ungroup()%>%
       select(device_id,datatype,satcount,lat,lon,hdop, datetime)%>%
  filter(satcount>3)

head(Asc)

IDs<-unique(Asc$platform_id)
asc_gps<-NULL
for (i in 1:length(IDs)){
  gps<-gps_burstID_sel%>%filter(device_id==IDs[i])
  if(nrow(gps)==0) next
  
  asc<-Asc%>%filter(platform_id==IDs[i])%>%group_by(platform_id,profile)%>%
    summarise(dt=max(datetime))
  asc$gpsID<-NA
  asc$lat<-NA
  asc$lon<-NA
  asc$gpstdiff<-NA
  asc$dt_gps<-ymd_hms("2000-01-01 01:01:01")
  
  for(j in 1:nrow(asc)){
    gps$Pdiff<-abs(difftime(gps$datetime, asc$dt[j],units = "secs"))
    asc$gpsID[j]<-which(gps$Pdiff==min(gps$Pdiff))
    asc$gpstdiff[j]<-min(gps$Pdiff)
    asc$lat[j]<-gps$lat[which(gps$Pdiff==min(gps$Pdiff))]
    asc$lon[j]<-gps$lon[which(gps$Pdiff==min(gps$Pdiff))]
    asc$dt_gps[j]<-gps$datetime[which(gps$Pdiff==min(gps$Pdiff))]
}
asc_gps<-rbind(asc_gps,asc)
}

asc_gps<-asc_gps%>%group_by(platform_id)%>%arrange(dt)
head(Asc)
head(asc_gps)
left_join(Asc,asc_gps,)
    
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
  deplymatrix<-'/data/Field Data/Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

if(Sys.info()[7]=="alexa") {
  usrdir<-"/Users/alexa/Box Sync/DASHCAMS/"
  datadir<-'data/ornitela_for_ATN/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'data/Field Data/Deployment_Field_Data.csv'
  source('/Users/alexa/git_repos/CormOcean_QuickViz/MakeDive.R')
}

acc<-readRDS(paste0(usrdir,savedir,"Accelerometer_Bursts.rds"))
unique(acc$datatype)

acc.g<-acc%>%filter(datatype=="GPS" | datatype=="GPSS"  | datatype=="GPSD" | datatype=="GPSF")

acc<-acc%>%filter(datatype=="SEN_ACC_5Hz" | datatype=="SEN_ACC_5Hz_START" | 
                    datatype=="SEN_ACC_5Hz_ENDINT" | datatype=="SEN_ACC_5Hz_END")

#identify and sequentially number GPS Dive bursts (typically 2 GPS fixes)
acc$tdiff_sec <-round(difftime(acc$datetime, lag(acc$datetime, 1),units = "secs"),2)

#annotate with a burst ID for each acc segment >10sec and <20 sec
# Find consecutively recorded accelerometer data
acc<- acc %>% select(project, device_id, datetime, tdiff_sec,acc_x,acc_y,acc_z)%>%
  group_by(device_id)%>%
  mutate(gap_time=tdiff_sec>0.3, # find times when there is a gap > 0.3 sec
         gap_time=ifelse(is.na(gap_time),0,gap_time), #fill NAs
         accBurstID=(cumsum(gap_time)))#, # gap_time is T/F so cumsum is adding 1

#sequentially numbers each row in each burst
acc<-acc%>%group_by(device_id,accBurstID)%>%
  mutate(accNum=row_number())
head(acc)

#filter out burst

i=15
ggplot()+
  geom_line(data=acc%>%filter(accBurstID==i),
            aes(x=datetime,y=acc_x), color="green")+
  geom_line(data=acc%>%filter(accBurstID==i),
            aes(x=datetime,y=acc_y), color="orange")+
  geom_line(data=acc%>%filter(accBurstID==i),
            aes(x=datetime,y=acc_z), color="blue")
  

#find the closest GPS location for the beginning of each segment
names(acc.g)
acc.g<-acc.g%>%select(project,device_id,datetime,Latitude,Longitude)

head(acc.g)
head(acc)


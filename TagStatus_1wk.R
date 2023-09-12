library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table) #rename
library(stringr)
library(R.utils)
library(tidyr)

#if(require("argosfilter")==FALSE) install.packages("argosfilter")
#library(argosfilter)

# Link to local Box Sync folder ---- 
#To find user/computer specific username use: Sys.getenv("LOGNAME")

# if(Sys.info()[4]=="benthos") {
# args = commandArgs(trailingOnly=TRUE)
# datadir<-args[1] #/home/DASHCAMS/data/ornitela_ftp_data/
# deplymatrix<-args[2] #/home/DASHCAMS/data_raw/metadata/DASHCAMS_Deployment_Field_data.csv
# savedir<-args[3] #/home/DASHCAMS/zTagStatus/
# source('/home/DASHCAMS/git/CormOcean/MakeDive.R')
# }

if(Sys.info()[4]=="benthos") {
datadir<-'/home/DASHCAMS/data_raw/ornitela_ftp/'
savedir<-'/home/DASHCAMS/data_processed/zTagStatus/'
deplymatrix<-'/home/DASHCAMS/data_raw/metadata/DASHCAMS_Deployment_Field_Data.csv'
source('/home/DASHCAMS/git/CormOcean/MakeDive.R')
}

if(Sys.info()[7]=="rachaelorben") {
  datadir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/data/ornitela_ftp_data/'
  savedir<-'/Users/rachaelorben/zTagStatus/'
  deplymatrix<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

#  Pulls in deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(deplymatrix)
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
deploy_matrix<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC)%>%
  filter(is.na(TagSerialNumber)==FALSE)

# Find file names with data -------------------------------------------
my_files <- fileSnapshot(path=datadir)
Files1<-rownames(my_files$info[1])[which(my_files$info[1] < 309)] #selects files with >309 bytes (1 header row)

today<-Sys.time()
wk2<-today-(604800*2)
dates<-my_files$info[5]
IDx<-which(dates$ctime>wk2)
Files2<-rownames(my_files$info[5])[IDx]#removes all files not written in last two weeks
Files<-Files2[Files2 %in% Files1 == FALSE] #I think this should remove empty files written within last two weeks

# Cycles through data files to find data written in last week------------
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
  if(df$UTC_datetime[1]>today-604800){sel_files<-c(sel_files,Files[i])} #selects files with a last date within 7 days of today
}

# Cycles through selected data files ----------------------------------------------
Birds<-NULL 
for (i in 1:length(sel_files)){
  
  fileN<-sel_files[i]
  tagID<-sapply(strsplit(sel_files[i], split='_', fixed=TRUE), function(x) (x[1]))
  
  deply_sel<-deploy_matrix[deploy_matrix$TagSerialNumber==tagID[1],]
  
  n<-nrow(deply_sel)
  if(n==0) next #if the tag isn't in the deployment matrix is will be skipped - important for skipping testing tags etc. 

  deply_sel<-deply_sel[n,] #picks the most recent deployment of that tag
  dat<-read.csv(file = paste0(datadir,sel_files[i]),sep = ",") #could switch to fread to be quicker...
  
  #deply_sel[deply_sel$DeploymentEndDatetime_UTC<deply_sel$DeploymentEndDatetime_UTC,]
 
  #if(is.na(deply_sel$DeploymentEndDatetime_UTC)==FALSE) {deply_sel<-deply_sel[deply_sel$DeploymentEndDatetime_UTC<deply_sel$DeploymentEndDatetime_UTC,]}
  if(nrow(deply_sel)==0) next
  
  dat$Project_ID<-deply_sel$Project_ID
  dat$tagID<-tagID
  dat$DeployEndShort<-deply_sel$Deployment_End_Short
  
  dat<-rename(dat,lat="Latitude")
  dat<-rename(dat,lon="Longitude")
  dat[is.na(dat)==TRUE]<-NA
  
  dat$datetime<-ymd_hms(dat$UTC_timestamp)
  dat$Foid<-1:nrow(dat)
  
  dat_gps<-dat%>%filter(!is.na(lat))
  if (nrow(dat_gps)==0)   {today<-Sys.time(); dat$GPS_surfacedrifts<-NA
  dat_sel<-dat[dat$datetime>(today-604800),] #trims to last week of data
  Birds<-rbind(Birds,dat_sel)}
  if (nrow(dat_gps)==0) next
    
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
  
  today<-Sys.time()
  dat_sel<-dat[dat$datetime>(today-604800),] #trims to last week of data
  Birds<-rbind(Birds,dat_sel)
}


#remove duplicate GPS points 
Birds<-Birds%>%group_by(device_id,datetime)%>%
  distinct(device_id,datetime, .keep_all = TRUE)%>%
  arrange(datetime) #arranges by time, could scramble data >1HZ a little bit


# identify dives ----------------------------------------------------------
Birds_dpth<-Birds%>%filter(is.na(depth_m)==FALSE)
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
                   DiveDepthYes=2, #dives need to reach 3 meters to be considered a dive event
                   TimeDiffAllowed_sec=2, #consecutive points need to have a time difference <2 to be in the same event
                   NumLocCut=3) #dives need to contain three points to be considered a dive, could change this to a duration
  
Birds_dpth$date<-date(Birds_dpth$datetime)

dsum<-Birds_dpth%>%group_by(ID,date)%>%
  summarise(n=n_distinct(divedatYN))

dsum_weekly<-dsum%>%group_by(ID)%>%
  summarize(udives_day=floor(mean(n)),
            totalDives=sum(n))

# quick summary of the bird data
sumDat<-Birds%>%group_by(Project_ID,tagID,device_id)%>%
  summarise(minDt=min(datetime),
            maxDt=max(datetime),
            maxDepth=max(depth_m,na.rm=TRUE),
            uDepth=round(mean(depth_m,na.rm=TRUE),2),
            n_GPS=n_distinct(lat),
            uBat=round(mean(U_bat_mV,na.rm=TRUE)),
            uTemp=round(mean(ext_temperature_C,na.rm=TRUE),2),
            uCond=round(mean(conductivity_mS.cm,na.rm=TRUE),2),
            GPS_surfacedrift_pts=sum(GPS_surfacedrifts))%>%
  mutate(dur=round(maxDt-minDt,2)) 

sumDat<-left_join(sumDat,dsum_weekly,by=c("tagID"="ID"))

dat_info<-Birds%>%
  group_by(tagID,datatype, DeployEndShort) %>%
  summarise(no_rows = length(datatype))

wide_dat<-dat_info%>%pivot_wider(names_from = datatype, values_from = no_rows)
SUMDAT<-left_join(sumDat,wide_dat,by="tagID")

SUMDAT[SUMDAT==-Inf]<-NA
SUMDAT <- SUMDAT %>%
  select(DeployEndShort, everything())

write.csv(x=SUMDAT,file = paste0(savedir,"1WeekStats_",date(today),".csv"))

# error checking plots ----------------------------------------------------
dt<-Sys.time()

#vectoral_sum for calibration
Birds$vectoral_sum<-((Birds$acc_x^2+Birds$acc_y^2+Birds$acc_z^2)^0.5)*.1 #should be .5 but 0.5 plots better

dsum$datetime<-ymd_hms(paste(dsum$date,"23:59:00"))

IDs<-unique(Birds$device_id)
for (i in 1:length(IDs)){
  
  birdy<-Birds[Birds$device_id==IDs[i],]
  Mdt<-min(birdy$datetime)
  
  labs <- data.frame(variable = c("a", "b","bb","c","d","e","f","g"), 
                     title_wd = c("Depth", "Temp","Con","Lat","Drift","Bat","Solar","VecSum"), 
                     y = c(-5,10,25,40,50,80,110, 125),
                     dt=c(rep(Mdt-10000,8))) # vertical position for labels
  
  temp_plot<-ggplot()+
    #depth=blue
    geom_point(data=birdy%>%filter(is.na(lat)==FALSE),aes(x=datetime,y=vectoral_sum),color="darkturquoise",size=.01)+
    geom_point(data=birdy%>%filter(is.na(depth_m)==FALSE)%>%filter(depth_m!=0),
               aes(x=datetime,y=-depth_m),size=.01, color="blue")+
    geom_point(data=dsum%>%filter(ID==IDs[i]),aes(x=datetime,y=n), size=3,alpha=.5, color="blue")+
    #lat=black
    geom_point(data=birdy,aes(x=datetime,y=abs(lat)),size=.01, color="black")+
    geom_point(data=birdy%>%filter(GPS_surfacedrifts==1),aes(x=datetime,y=abs(lat)+5),size=.02, color="darkgreen")+
    #temperature=purple
    geom_point(data=birdy%>%filter(ext_temperature_C<100)%>%filter(ext_temperature_C>0),
               aes(x=datetime,y=ext_temperature_C),size=.01,color="purple")+
    #conductivity=
    geom_point(data=birdy%>%filter(is.na(conductivity_mS.cm)==FALSE),
               aes(x=datetime,y=(conductivity_mS.cm)),size=.01,color="orange")+
    #battery=red
    geom_path(data=birdy%>%filter(is.na(lat)==FALSE),aes(x=datetime,y=bat_soc_pct),color="grey")+
    geom_point(data=birdy%>%filter(is.na(lat)==FALSE),aes(x=datetime,y=bat_soc_pct),color="red",size=.01)+
    #solar=yellow
    geom_point(data=birdy%>%filter(is.na(lat)==FALSE),aes(x=datetime,y=solar_I_mA+101),color="goldenrod2",size=.01)+
    scale_x_datetime(date_labels = "%b %d") +
    geom_text(data = labs, angle = 45, size=4,# add rotated text near y-axis
              aes(x = dt, y = y, label = title_wd, color = title_wd)) +
    scale_color_manual(values=c("red","orange","blue" ,"darkgreen","black","goldenrod2","purple","darkturquoise")) +
    ylab("")+ # hide default y-axis label
    theme(legend.position = "none")+
    guides(color = guide_legend(override.aes = list(size = 5)))
  ggsave(temp_plot,filename = paste0(savedir,"1wks_",birdy$Project_ID[1],"_",birdy$DeployEndShort[1],"_",IDs[i],"_AllDataStreams.png"),
         height=4,width=8,device = "png")
}

#map location, color by date
w2hr<-map_data('world')

Birds_gps<-Birds%>%filter(lat!=0)%>%filter(is.na(lat)==FALSE)%>%filter(lon!=0)
Birds_gps$device_id<-as.factor(Birds_gps$device_id)

IDs<-unique(Birds_gps$Project_ID)
for (i in 1:length(IDs)){
  birdies<-Birds_gps[Birds_gps$Project_ID==IDs[i],]
  birds<-droplevels(unique(birdies$device_id))
  
  locs<-birdies
  # locs<-NULL
  # for (j in 1:length(birds)){
  #   Locs1<-birdies%>%filter(device_id==birds[j])
  #   try(mfilter<-vmask(lat=Locs1$lat, lon=Locs1$lon, dtime=Locs1$datetime, vmax=25), silent=FALSE)
  #   #if mfilter isn't made this makes one that selects all points 
  #   if (exists("mfilter")==FALSE) mfilter<-rep("not", nrow(Locs1))
  #   Locs1$mfilter<-mfilter
  #   Locs<-Locs1%>%filter(mfilter!="removed")
  #   locs<-rbind(locs,Locs)
  # }
  
  y_min<-min(locs$lat)-.25
  y_max<-max(locs$lat)+.25
  
  x_min<-min(locs$lon)-.25
  x_max<-max(locs$lon)+.25
  
  temp_plot<-ggplot()+
    geom_polygon(data=w2hr,aes(long,lat,group=group),fill="grey70",color="grey60",linewidth=0.1)+
    geom_path(data=locs,aes(x=lon,y=lat, group=device_id))+
    geom_point(data=locs,aes(x=lon,y=lat, color=device_id), size=.1)+
    xlab("Longitude")+
    ylab("Latitude")+
    coord_fixed(ratio=1.7,xlim = c(x_min,x_max),ylim=c(y_min,y_max))+
    theme_bw()
    #theme(legend.position = "none",
    #      text = element_text(size = 6),
    #      axis.text = element_text(size = 4))+
    #theme(legend.title = element_blank(),
    #     legend.text = element_text(size=5))+
    #guides(color = guide_legend(override.aes = list(size = 2)))
    #facet_wrap(~device_id)
  ggsave(temp_plot,filename = paste0(savedir,"/1wk_map_",IDs[i],".png"),height=4,width=8,device = "png")
}



# google map plots --------------------------------------------------------
# Once Benthos is ready rewrite for server and move libraries to the top of code. 
# This should just fail during runs

library(ggmap)
library(osmdata)
library(argosfilter)
library(ggpubr)

info<-read.csv("/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/data/Field Data/Project Titles and IDs.csv")

unique(Birds_gps$DeployEndShort)
Birds_gps<-Birds%>%filter(lat!=0)%>%
  filter(is.na(lat)==FALSE)%>%
  filter(lon!=0)%>%
  filter(DeployEndShort!="stationary")

Birds_gps$device_id<-as.factor(Birds_gps$device_id)

birdNum<-length(unique(Birds_gps$device_id))

IDs<-unique(Birds_gps$Project_ID)
temp_plot<-list()
diveSUM<-NA

for (i in 1:length(IDs)){
  birdies<-Birds_gps[Birds_gps$Project_ID==IDs[i],]
  birds<-droplevels(unique(birdies$device_id))
  sum.dat<-SUMDAT%>%filter(Project_ID==IDs[i])
  diveSUM[i]<-sum(sum.dat$totalDives, na.rm=TRUE)
  Pinfo<-info%>%filter(Project_ID==IDs[i])
  tit<-paste0(Pinfo$Country,"-",Pinfo$Species_Short, ": ",diveSUM[i], " dives")
  
  
  
  locs<-birdies
  # locs<-NULL
  # for (j in 1:length(birds)){
  #   Locs1<-birdies%>%filter(device_id==birds[j])
  #   try(mfilter<-vmask(lat=Locs1$lat, lon=Locs1$lon, dtime=Locs1$datetime, vmax=25), silent=FALSE)
  #   #if mfilter isn't made this makes one that selects all points
  #   if (exists("mfilter")==FALSE) mfilter<-rep("not", nrow(Locs1))
  #   Locs1$mfilter<-mfilter
  #   Locs<-Locs1%>%filter(mfilter!="removed")
  #   locs<-rbind(locs,Locs)
  # }
  # 
  y_min<-min(locs$lat)-.4
  y_max<-max(locs$lat)+.4
  
  x_min<-min(locs$lon)-.5
  x_max<-max(locs$lon)+.5
  
  (map <- get_map(c(left = x_min, bottom = y_min, right = x_max, top = y_max),source="stamen"))
  
  temp_plot[[i]]<-
    #geom_polygon(data=w2hr,aes(long,lat,group=group),fill="grey70",color="grey60",linewidth=0.1)+
    ggmap(map)+
    geom_path(data=locs,aes(x=lon,y=lat, group=device_id))+
    geom_point(data=locs,aes(x=lon,y=lat, color=device_id), size=.5)+
    xlab("Longitude")+
    ylab("Latitude")+
    coord_fixed(ratio=1.7,xlim = c(x_min,x_max),ylim=c(y_min,y_max))+
    labs(title=tit)+
    theme_bw()+
    theme(legend.title=element_blank(),
          legend.text = element_blank())+
    guides(colour = guide_legend(override.aes = list(size=3)))
  ggsave(temp_plot[[i]],filename = paste0(savedir,"/street_1wk_map_",IDs[i],".png"),height=4,width=8,device = "png")
}


IDS_label<-paste0(IDs, ": ",diveSUM, " dives")

length(temp_plot[[i]])
A<-date(min(Birds_gps$datetime))
B<-date(max(Birds_gps$datetime))

figure <- ggarrange(temp_plot[[1]],temp_plot[[2]],temp_plot[[3]],temp_plot[[4]],
                    temp_plot[[5]],temp_plot[[6]],temp_plot[[7]],temp_plot[[8]],
                    temp_plot[[9]],temp_plot[[10]],temp_plot[[11]],
                    ncol = 4, nrow = 3)

figure<-annotate_figure(figure, top = text_grob(paste("Cormorant Oceanography Project Data:", A,"-", B,"with", birdNum, "birds &",sum(diveSUM),"total dives"),
                                      color = "black", face = "bold", size = 14))
figure
ggsave(figure,filename = paste0(savedir,"/CormOcean_1wk_",A,"_ALL.png"),height=12,width=16,device = "png",bg = "white")

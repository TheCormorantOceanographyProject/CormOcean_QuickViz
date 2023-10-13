library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table) #rename
library(stringr)
library(R.utils)
library(tidyr)
library(argosfilter)
library(sf)
library(MetBrewer)
library(cowplot)

wrap360 = function(lon) {
  lon360<-ifelse(lon<0,lon+360,lon)
  return(lon360)
}

if(Sys.info()[7]=="rachaelorben") {
  userdir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  #source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

#adjust with your info
if(Sys.info()[7]=="Jessica") { 
  userdir<-'/Users/jessica/Library/CloudStorage/Box-Box/DASHCAMS/'
  savedir<-'Analysis/DataViz/'
  deplymatrix<-'data/Field Data/DASHCAMS_Deployment_Field_Data.csv'  
  #source('/Users/jessica/git_repos/CormOcean/MakeDive.R')
}

# Pulls in deployment matrix ---------------------------------------------
# only birds in the deployment matrix will be plotted
deploy_matrix<-read.csv(paste0(userdir,deplymatrix))
names(deploy_matrix)
deploy_matrix<-deploy_matrix%>%
  select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC)%>%
  filter(is.na(TagSerialNumber)==FALSE)
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
head(deploy_matrix)


# Pulls in GPS data only --------------------------------------------------
#should be from Box, but the 13GB file in the processed folder prohibits selective syncing of that folder. 
#dat<-read.csv(paste0(userdir,"processed/ornitela_gps.csv"))
dat<-read.csv("/Users/rachaelorben/Desktop/ornitela_gps.csv") 
dat<-dat%>%filter(lon<180)%>%filter(lat!=0) #removes one crazy lon value + 23k of 0,0 GPS values
dat$datetime<-dmy_hms(dat$time)
head(dat)

IDS<-unique(dat$id)
DAT<-NULL
for (i in 1:length(IDS)){

deply_sel<-deploy_matrix[deploy_matrix$TagSerialNumber==IDS[i],]
deply_sel<-distinct(.data = deply_sel, Project_ID, .keep_all = TRUE)

birdy<-dat%>%filter(id==IDS[i])
if(is.na(deply_sel$DeploymentStartDatetime[1])==TRUE) next
birdy<-birdy%>%filter(datetime>deply_sel$DeploymentStartDatetime[1])
if(nrow(deply_sel)==1 & is.na(deply_sel$DeploymentEndDatetime_UTC[1])==FALSE) {birdy<-birdy%>%filter(datetime<deply_sel$DeploymentEndDatetime_UTC[1])}
if(nrow(birdy)==0) next

bid_num<-unique(deply_sel$Bird_ID)
if(length(bid_num)==1) {birdy$Project_ID<-deply_sel$Project_ID}
if(length(bid_num)==1) {birdy$Bird_ID<-deply_sel$Bird_ID}
if(length(bid_num)==1) {DAT<-rbind(DAT,birdy)}
if(length(bid_num)==1) next
  
b1<-birdy%>%filter(datetime>deply_sel$DeploymentStartDatetime[1])%>%
  filter(datetime<deply_sel$DeploymentEndDatetime_UTC[1])
b2<-birdy%>%filter(datetime>deply_sel$DeploymentStartDatetime[2])
if(is.na(deply_sel$DeploymentEndDatetime_UTC[2])==FALSE) {b2<-b2%>%filter(datetime<deply_sel$DeploymentEndDatetime_UTC[2])}

b1$Project_ID<-deply_sel$Project_ID[1]; b1$Bird_ID<-deply_sel$Bird_ID[1]
b2$Project_ID<-deply_sel$Project_ID[2]; b2$Bird_ID<-deply_sel$Bird_ID[2]

if(length(bid_num)==3) {b2<-b2%>%filter(datetime>deply_sel$DeploymentStartDatetime[2])%>%
  filter(datetime<deply_sel$DeploymentEndDatetime_UTC[2])}
if(length(bid_num)==3) {b3<-birdy%>%filter(datetime>deply_sel$DeploymentStartDatetime[3])}
if(length(bid_num)==3) {b3$Project_ID<-deply_sel$Project_ID[3]}
if(length(bid_num)==3) {b3$Bird_ID<-deply_sel$Bird_ID[3]}
  
DAT<-rbind(DAT,b1,b2)
if(length(bid_num)==3) {DAT<-rbind(DAT,b3)}

if(length(bid_num)>3) {break} #we don't have any tags that have been on four birds yet...
}

unique(DAT$Project_ID)

# weak speed filter ------------------------------------------------------------
DAT$Uni_ID<-paste0(DAT$Project_ID,"_",DAT$Bird_ID)

birds<-unique(DAT$Uni_ID)
locs<-NULL
for (j in 1:length(birds)){
  Locs1<-DAT%>%filter(Uni_ID==birds[j])
  Locs1<-Locs1%>%group_by(datetime)%>%
    distinct(datetime, .keep_all = TRUE)%>%
    arrange(datetime) #arranges by time
  try(mfilter<-vmask(lat=Locs1$lat, lon=Locs1$lon, dtime=Locs1$datetime, vmax=25), silent=FALSE)
  #if mfilter isn't made this makes one that selects all points
  if (exists("mfilter")==FALSE) mfilter<-rep("not", nrow(Locs1))
  Locs1$mfilter<-mfilter
  Locs<-Locs1%>%filter(mfilter!="removed")
  locs<-rbind(locs,Locs)
}

head(locs)
#saveRDS(locs, paste0(userdir,savedir,"GPS_SpeedFiltered.rds"))

unique(locs$Project_ID)


# Maps --------------------------------------------------------------------
w2hr<-map_data('world')

locs_wgs84<-st_as_sf(locs,coords=c('lon','lat'),remove = F,crs = 4326)
dt=Sys.Date()
#quartz()
ggplot()+
  geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray60",color="grey25",size=0.1)+
  geom_sf(data = locs_wgs84, aes(color=Project_ID), size=.3)+
  scale_color_manual(values=met.brewer("Johnson", 18))+
  theme_bw()+
  theme(legend.position ="none",axis.title = element_blank())
ggsave(paste0(userdir,savedir,"WorldCormorants_",dt,".png"), dpi=300)

#the rest of the code is under constuction...
WORLD<-ggplot()+
  geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray25",color="grey60",size=0.1)+
  geom_sf(data = locs_wgs84, color="orange", size=.3)+
  scale_color_manual(values=met.brewer("Johnson", 18))+
  theme_bw()+
  theme(legend.position ="none",axis.title = element_blank())
ggsave("/Users/rachaelorben/Desktop/WorldCormorants_orange.png", dpi=300)

data_sel=locs%>%filter(Project_ID=="UAESISO20" | Project_ID=="BAHHASO21" |Project_ID=="BAHHASO22" | Project_ID=="UAESISO20" | Project_ID=="UAEBUSO20")
UAE_sel_wgs84<-st_as_sf(data_sel,coords=c('lon','lat'),remove = F,crs = 4326)
y_min_U<-min(data_sel$lat)-.25
y_max_U<-30.5
x_min_U<-45
x_max_U<-max(data_sel$lon)+.25
unique(UAE_sel_wgs84$Uni_ID)

#quartz()
UAE<-ggplot()+
  geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray25",color="grey60",size=0.1)+
  geom_sf(data = UAE_sel_wgs84, aes(color=Uni_ID), size=.01)+
  scale_color_manual(values=met.brewer("Tam", 37))+
  coord_sf(xlim = c(47,x_max_U),ylim=c(y_min_U,y_max_U))+
  theme_bw()+
  theme(legend.position ="none",axis.title = element_blank(),
        axis.text = element_text(size = 8))
ggsave("/Users/rachaelorben/Desktop/ArabianGulf.png", dpi=300)

unique(locs$Project_ID)
Peru_sel=locs%>%filter(Project_ID=="PERIPGU22_CZ" | Project_ID=="PERIPGU22_SC")
length(unique(Peru_sel$Uni_ID))
Peru_sel_wgs84<-st_as_sf(Peru_sel,coords=c('lon','lat'),remove = F,crs = 4326)

y_min_P<-min(Peru_sel$lat)-.25
y_max_P<-max(Peru_sel$lat)-.25
x_min_P<-min(Peru_sel$lon)+.30
x_max_P<-max(Peru_sel$lon)+.25

PERU<-ggplot()+
  geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray25",color="grey60",size=0.1)+
  geom_sf(data = Peru_sel_wgs84, aes(color=Uni_ID), size=.01)+
  scale_color_manual(values=met.brewer("Tam", 12))+
  coord_sf(xlim = c(x_min_P,x_max_P),ylim=c(-5,-25))+
  theme_bw()+
  theme(legend.position ="none",axis.title = element_blank(),
        axis.text = element_text(size = 4))
ggsave("/Users/rachaelorben/Desktop/Peru.png", dpi=300)

unique(locs$Project_ID)
USA_sel=locs%>%filter(Project_ID=="USACRBRPE19" | Project_ID=="USAFIBR21"| Project_ID=="USAPRBR23")
length(unique(USA_sel$Uni_ID))
USA_sel_wgs84<-st_as_sf(USA_sel,coords=c('lon','lat'),remove = F,crs = 4326)

y_min<-min(USA_sel$lat)-.25
y_max<-max(USA_sel$lat)-.25
x_min<-min(USA_sel$lon)+.25
x_max<-max(USA_sel$lon)+.25

USA<-ggplot()+
  geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray25",color="grey60",size=0.1)+
  geom_sf(data = USA_sel_wgs84, aes(color=Uni_ID), size=.01)+
  scale_color_manual(values=met.brewer("OKeeffe2", 19))+
  coord_sf(xlim = c(-128,-118),ylim=c(33,50))+
  theme_bw()+
  theme(legend.position ="none",axis.title = element_blank(),
        axis.text = element_text(size = 4))
ggsave("/Users/rachaelorben/Desktop/USA.png", dpi=300)


p1<-plot_grid(USA, width=2)
p2<-plot_grid(PERU, width=2)
p1<-plot_grid(USA, PERU,rel_widths = c(1,1),
              labels = c('C','D'), label_size = 12)
p2<-plot_grid(WORLD, UAE, nrow = 2,rel_heights = c(1,1),
              labels = c('A', 'B'), label_size = 12)

P<-plot_grid(p2,p1,rel_widths = c(.6, 0.4))
ggsave2("/Users/rachaelorben/Desktop/Allareas.png",P, 
        dpi=300,width = 8, height=5, units="in")


p3<-plot_grid(WORLD, UAE, ncol = 2,rel_heights = c(1,1),
              labels = c('A', 'B'), label_size = 12)
ggsave2("/Users/rachaelorben/Desktop/World_AG.png",p3, 
        dpi=300,width = 8, height=3, units="in")

unique(locs$UniI)

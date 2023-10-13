library(sf)
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(dplyr)
library(lubridate)
library(sp)
library(adehabitatHR)
library(scales) # Helps make polygons partly transparent using the alpha argument below
library(cowplot)

usr<-"/Users/rachaelorben/Box/DASHCAMS/"

#  Pulls in deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usr,"/data/Field Data/DASHCAMS_Deployment_Field_Data.csv"))
#str(deploy_matrix)
deploy_matrix<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short)%>%
  filter(is.na(TagSerialNumber)==FALSE)
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)
dm<-deploy_matrix%>%filter(Project_ID=="USAMIPE20")


# filters birds for the Middleton Deployment ------------------------------
peco<-read.csv(paste0(usr,"/data/Processed/ornitela_dives.csv"))
head(peco)
peco$datetime<-dmy_hms(peco$time)
peco$year<-year(peco$datetime)
peco$month<-month(peco$datetime)
peco<-peco%>%filter(tag %in% dm$TagSerialNumber)%>%filter(year==2020)%>%
  filter(month==7 | month==8)%>%filter(gps_lat_post!=0)%>%filter(gps_lon_post<(-138))
peco$species<-"PECO"

ggplot()+
  geom_point(data=peco,aes(x=gps_lon_post,y=gps_lat_post, group=tag, color=as.factor(tag)))

# makes minimum convex polygons from all birds (and individual bir --------
peco.sp <- peco[, c("tag", "gps_lon_post", "gps_lat_post")] 
coordinates(peco.sp) <- c("gps_lon_post", "gps_lat_post")
proj4string(peco.sp) <- CRS( "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs" )
peco.mcp <- mcp(peco.sp, percent = 100)

plot(peco.sp, col = as.factor(peco.sp@data$tag), pch = 16)
plot(peco.mcp, col = alpha(1:5, 0.5), add = TRUE)

peco.all <- peco[, c("species", "gps_lon_post", "gps_lat_post")] 
coordinates(peco.all) <- c("gps_lon_post", "gps_lat_post")
proj4string(peco.all) <- CRS( "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs" )
peco.all.mcp <- mcp(peco.all, percent = 100)

plot(peco.all, col = as.factor(peco.sp@data$tag), pch = 16)
plot(peco.all.mcp, col = alpha(1:5, 0.5), add = TRUE)

peco.all.mcp.df <- fortify(peco.all.mcp)

# pulls in GEBCO souce info (TID) -----------------------------------------
TID_sh<-read.csv(paste0(usr,"/data/gebco_2021_sub_ice_topo/TID_GEBCO_2022.csv"))
names(TID_sh)
TID_sh.sel<-TID_sh%>%dplyr::select("TID","TID_short","Measurement")

nc_data<-nc_open(paste0(usr,"/data/gebco_2021_sub_ice_topo/GEBCO_12_Aug_2022_95c7216ac683_AK/gebco_2022_tid_n61.4093_s54.7581_w-153.0473_e-139.4114.nc"))
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "tid")
nc_close(nc_data) 

r <- raster(t(t), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
plot(r)

r_df <- as.data.frame(r, xy = TRUE)

r_df<-left_join(r_df,TID_sh.sel,by=c("layer"="TID"))
head(r_df)
MI<-data.frame(location="Middleton Island",x=(-146.336545), y=59.424123)

# crop raster to polygon --------------------------------------------------
bang_man <- raster::crop(r,peco.all.mcp)
TID_peco <- mask(bang_man, peco.all.mcp)
plot(TID_peco, axes = FALSE)
TID_peco.df<-as.data.frame(TID_peco, xy = TRUE)
TID_peco.df<-TID_peco.df%>%filter(is.na(layer)==FALSE)

names(TID_peco.df)
TID_peco.df%>%group_by(layer)%>%
  summarise(n=n())

TID_peco.df<-left_join(TID_peco.df,TID_sh.sel,by=c("layer"="TID"))
E<-ggplot()+
  geom_bar(data=TID_peco.df%>%filter(is.na(layer)==FALSE),aes(x=TID_short, fill=TID_short))+
  theme(legend.position = "none")
Fa<-ggplot()+
  geom_bar(data=TID_peco.df%>%filter(is.na(layer)==FALSE),aes(x=Measurement, fill=Measurement))+
  theme(legend.position = "none")

quartz(height=4,width=8)
plot_grid(E,Fa, labels = c('A', 'B'))
ggsave(paste0(usr,"/Analysis/gebco_comparisons/plots/Alaska_TID_BARplot.jpg"))


# Data Viz ----------------------------------------------------------------
quartz()
ggplot()+
  geom_raster(data=r_df, aes(x = x, y = y, fill = as.factor(TID_short))) + 
  geom_point(data=MI, aes(x = x, y = y )) + 
  coord_quickmap()
ggsave(paste0(usr,"/Analysis/gebco_comparisons/plots/Alaska_TID.jpg"))

quartz()
ggplot()+
  geom_raster(data=r_df, aes(x = x, y = y, fill = as.factor(Measurement))) + 
  geom_point(data=MI, aes(x = x, y = y )) + 
  coord_quickmap()
ggsave(paste0(usr,"/Analysis/gebco_comparisons/plots/Alaska_type.jpg"))

quartz()
ggplot()+
  geom_raster(data=r_df, aes(x = x, y = y, fill = as.factor(TID_short))) + 
  geom_point(data=MI, aes(x = x, y = y )) + 
  geom_path(data=peco.all.mcp.df, aes(x=long,y=lat), color="black")+
  coord_fixed(ratio=1.7,xlim = c(-147,-145.5),ylim=c(59,59.75))
ggsave(paste0(usr,"/Analysis/gebco_comparisons/plots/Alaska_MiddletonZoom_TID.jpg"))


ggplot()+
  geom_raster(data=TID_peco.df, aes(x = x, y = y, fill = as.factor(TID_short))) + 
  geom_point(data=MI, aes(x = x, y = y )) + 
  coord_quickmap()
ggsave(paste0(usr,"/Analysis/gebco_comparisons/plots/Alaksa_Middleton_MCP_peco.jpg"))

ggplot()+
  geom_raster(data=TID_peco.df, aes(x = x, y = y, fill = as.factor(layer))) + 
  geom_point(data=MI, aes(x = x, y = y )) + 
  coord_quickmap()
ggsave(paste0(usr,"/Analysis/gebco_comparisons/plots/Alaksa_Middleton_codes_MCP_peco.jpg"))

quartz()
ggplot()+
  geom_raster(data=TID_peco.df, aes(x = x, y = y, fill = as.factor(Measurement))) + 
  geom_point(data=peco, aes(x = gps_lon_post, y = gps_lat_post), size=0.5) + 
  coord_quickmap()
ggsave(paste0(usr,"/Analysis/gebco_comparisons/plots/Alaksa_Middleton_MCP_peco_dives.jpg"))


quartz(height=4,width=10)
plot_grid(A,B, labels = c('A', 'B'),rel_widths = .6)

quartz()
plot_grid(C,D, labels = c('C', 'D'))

quartz()
plot_grid(E,Fa, labels = c('E', 'F'))



# Arabian Gulf ------------------------------------------------------------

#  Pulls in deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usr,"/data/Field Data/DASHCAMS_Deployment_Field_Data.csv"))
#str(deploy_matrix)
deploy_matrix<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short)%>%
  filter(is.na(TagSerialNumber)==FALSE)
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)
dm<-deploy_matrix%>%filter(Project_ID=="UAEBUSO20" |Project_ID=="BAHHASO21" )


# filters birds for the Middleton Deployment ------------------------------
scco<-read.csv(paste0(usr,"/data/Processed/ornitela_dives.csv"))
head(scco)
scco$datetime<-dmy_hms(scco$time)
scco$year<-year(scco$datetime)
scco$month<-month(scco$datetime)
scco<-scco%>%filter(tag %in% dm$TagSerialNumber)%>%filter(gps_lat_post!=0)
scco$species<-"scco"

ggplot()+
  geom_point(data=scco,aes(x=gps_lon_post,y=gps_lat_post, group=tag, color=as.factor(tag)))

# makes minimum convex polygons from all birds (and individual bir --------
scco.sp <- scco[, c("tag", "gps_lon_post", "gps_lat_post")] 
coordinates(scco.sp) <- c("gps_lon_post", "gps_lat_post")
proj4string(scco.sp) <- CRS( "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs" )
scco.mcp <- mcp(scco.sp, percent = 100)

plot(scco.sp, col = as.factor(scco.sp@data$tag), pch = 16)
plot(scco.mcp, col = alpha(1:5, 0.5), add = TRUE)

scco.all <- scco[, c("species", "gps_lon_post", "gps_lat_post")] 
coordinates(scco.all) <- c("gps_lon_post", "gps_lat_post")
proj4string(scco.all) <- CRS( "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs" )
scco.all.mcp <- mcp(scco.all, percent = 100)

plot(scco.all, col = as.factor(scco.sp@data$tag), pch = 16)
plot(scco.all.mcp, col = alpha(1:5, 0.5), add = TRUE)

scco.all.mcp.df <- fortify(scco.all.mcp)

# pulls in GEBCO souce info (TID) -----------------------------------------
TID_sh<-read.csv(paste0(usr,"/data/gebco_2021_sub_ice_topo/TID_GEBCO_2022.csv"))
TID_sh.sel<-TID_sh%>%select(TID,TID_short,Measurement)

nc_data<-nc_open(paste0(usr,"/data/gebco_2021_sub_ice_topo/GEBCO_12_Aug_2022_5758a269b4c3_UAE/gebco_2022_tid_n36.1849_s23.5071_w41.5627_e58.4554.nc"))
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
t <- ncvar_get(nc_data, "tid")
nc_close(nc_data) 

r <- raster(t(t), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
plot(r)


r_df <- as.data.frame(r, xy = TRUE)

r_df<-left_join(r_df,TID_sh.sel,by=c("layer"="TID"))
head(r_df)


# crop raster to polygon --------------------------------------------------
bang_man <- raster::crop(r,scco.all.mcp)
TID_scco <- mask(bang_man, scco.all.mcp)
plot(TID_scco, axes = FALSE)
TID_scco.df<-as.data.frame(TID_scco, xy = TRUE)
TID_scco.df<-TID_scco.df%>%filter(is.na(layer)==FALSE)

names(TID_scco.df)
TID_scco.df%>%group_by(layer)%>%
  summarise(n=n())

TID_scco.df<-left_join(TID_scco.df,TID_sh.sel,by=c("layer"="TID"))
E<-ggplot()+
  geom_bar(data=TID_scco.df%>%filter(is.na(layer)==FALSE),aes(x=TID_short, fill=TID_short))+
  theme(legend.position = "none")+
  theme(axis.text.x=element_text(angle=45, hjust=1))
Fa<-ggplot()+
  geom_bar(data=TID_scco.df%>%filter(is.na(layer)==FALSE),aes(x=Measurement, fill=Measurement))+
  theme(legend.position = "none")+
  theme(axis.text.x=element_text(angle=45, hjust=1))

quartz(height=4,width=8)
plot_grid(E,Fa, labels = c('A', 'B'))
ggsave(paste0(usr,"/Analysis/gebco_comparisons/plots/ArabianGulf_barplot.jpg"))


# Data Viz ----------------------------------------------------------------
r_df<-r_df%>%filter(x>47)%>%filter(y<31)
quartz()
ggplot()+
  geom_raster(data=r_df, aes(x = x, y = y, fill = as.factor(TID_short))) + 
  coord_quickmap()
ggsave(paste0(usr,"/Analysis/gebco_comparisons/plots/ArabianGulf_TID.jpg"))

quartz()
ggplot()+
  geom_raster(data=r_df, aes(x = x, y = y, fill = as.factor(Measurement))) + 
  coord_quickmap()
ggsave(paste0(usr,"/Analysis/gebco_comparisons/plots/ArabianGulf_meastype.jpg"))

quartz()
ggplot()+
  geom_raster(data=r_df, aes(x = x, y = y, fill = as.factor(TID_short))) + 
  geom_path(data=scco.all.mcp.df, aes(x=long,y=lat), color="black")
ggsave(paste0(usr,"/Analysis/gebco_comparisons/plots/ArabianGulf_SCCO_mcp.jpg"))


ggplot()+
  geom_raster(data=TID_scco.df, aes(x = x, y = y, fill = as.factor(TID_short))) + 
  coord_quickmap()
ggsave(paste0(usr,"/Analysis/gebco_comparisons/plots/ArabianGulf_scco_clip.jpg"))

ggplot()+
  geom_raster(data=TID_scco.df, aes(x = x, y = y, fill = as.factor(layer))) + 
  coord_quickmap()
ggsave(paste0(usr,"/Analysis/gebco_comparisons/plots/ArabianGulf_codes_scco_clip.jpg"))

ggplot()+
  geom_raster(data=TID_scco.df, aes(x = x, y = y, fill = as.factor(Measurement))) + 
  coord_quickmap()
ggsave(paste0(usr,"/Analysis/gebco_comparisons/plots/ArabianGulf_type_scco_clip.jpg"))


quartz()
ggplot()+
  geom_raster(data=TID_scco.df, aes(x = x, y = y, fill = as.factor(Measurement))) + 
  geom_point(data=scco, aes(x = gps_lon_post, y = gps_lat_post), size=0.5) + 
  coord_quickmap()
ggsave(paste0(usr,"/Analysis/gebco_comparisons/plots/ArabianGulf_scco_clip_dives.jpg"))


quartz(height=4,width=10)
plot_grid(A,B, labels = c('A', 'B'),rel_widths = .6)

quartz()
plot_grid(C,D, labels = c('C', 'D'))

quartz()
plot_grid(E,Fa, labels = c('E', 'F'))


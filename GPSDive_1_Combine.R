
#matches dive with GPS points 
IDS<-unique(dive_sum$ID)
dive_sum_gps<-NULL
for (i in 1:length(IDS)){
  birdy<-Birds_gps%>%filter(tagID==IDS[i])
  birdy$gps_oid<-1:nrow(birdy)
  bdive<-dive_sum%>%filter(ID==IDS[i])
  
  dt<-unique(bdive$date)
  bdive_all<-NULL
  for (k in 1:length(dt)){
    birdy_dt<-birdy%>%filter(UTC_date==dt[k])
    bdive_dt<-bdive%>%filter(date==dt[k])
    
    bdive_dt$gps_tdiff<-NA
    bdive_dt$lat<-NA
    bdive_dt$lon<-NA
    bdive_dt$gps_datatype<-NA
    bdive_dt$gps_oid<-NA
    bdive_dt$gps_time<-NA
    for (j in 1:nrow(bdive_dt)){
      birdy_dt$gpstd<-(abs(birdy_dt$datetime-bdive_dt$maxDt[j]))
      sm<-min(abs(birdy_dt$gpstd))
      idx<-which(abs(birdy_dt$gpstd)==sm)
      
      bdive_dt$gps_tdiff[j]<-birdy_dt$datetime[idx]-bdive_dt$maxDt[j]
      bdive_dt$lat[j]<-birdy_dt$lat[idx]
      bdive_dt$lon[j]<-birdy_dt$lon[idx]
      bdive_dt$gps_datatype[j]<-birdy_dt$datatype[idx]
      bdive_dt$gps_oid[j]<-birdy_dt$gps_oid[idx]
      bdive_dt$gps_time[j]<-birdy_dt$datetime[idx]
    }
    bdive_all<-rbind(bdive_all,bdive_dt)     
  }
  
  dive_sum_gps<-rbind(dive_sum_gps,bdive_all)  
}

dsum<-Birds_dpth%>%group_by(ID,date)%>% #daily dive summaries
  summarise(n=n_distinct(divedatYN),
            minDt=min(datetime),
            maxDt=max(datetime),
            maxDepth=max(depth,na.rm=TRUE),
            uDepth=round(mean(depth,na.rm=TRUE),2))

# quick summary of the bird data
sumDat<-locs%>%group_by(tagID)%>%
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


#write.csv(x=dive_sum, paste0("/Users/rachaelorben/Desktop/NZ","/DiveSum.csv"))
write.csv(x=dive_sum, paste0("/Users/rachaelorben/Desktop/SA","/DiveSum.csv"))


w2hr<-map_data('world')

y_min<-min(Birds_gps$lat)-.25
y_max<-max(Birds_gps$lat)+.25

x_min<-min(Birds_gps$lon)-.25
x_max<-max(Birds_gps$lon)+.25

quartz(width=5,height=4)
ggplot()+
  geom_polygon(data=w2hr,aes(long,lat,group=group),fill="grey70",color="grey60",linewidth=0.1)+
  geom_path(data=Birds_gps,aes(x=lon,y=lat, group=device_id))+
  geom_point(data=Birds_gps,aes(x=lon,y=lat, color=datatype))+
  xlab("Longitude")+
  ylab("Latitude")+
  coord_fixed(ratio=1.7,xlim = c(x_min,x_max),ylim=c(y_min,y_max))+
  theme_bw()
ggsave(filename = paste0("/Users/rachaelorben/Desktop/SA/GPSplot.jpg"))


names(Birds_dpth_MD)
Birds_gps$Pt<-1


Birds_dpth_MD<-cbind(Birds_dpth_MD,ext_temperature_C=Birds_dpth$ext_temperature_C)
quartz(width=12,height=3)
ggplot()+
  geom_point(data=Birds_gps,aes(x=datetime,y=Pt, color=datatype))+
  geom_path(data=Birds_dpth_MD%>%filter(is.na(divedatYN)==FALSE),
            aes(x=datetime,y=-depth, group=divedatYN))+
  geom_point(data=Birds_dpth_MD%>%filter(is.na(divedatYN)==FALSE),
             aes(x=datetime,y=-depth, fill=ext_temperature_C, group=divedatYN), shape = 21)+
  theme_bw()+
  ylab("Depth (m)")+
  xlab("")+
  NULL
ggsave(filename = paste0("/Users/rachaelorben/Desktop/SA/DiveData.jpg"))

names(Birds_dpth_MD)
quartz(width=12,height=3)
ggplot()+
  geom_point(data=Birds_gps%>%filter(UTC_date=="2022-12-05"),
             aes(x=datetime,y=Pt, color=datatype))+
  geom_path(data=Birds_dpth_MD%>%filter(is.na(divedatYN)==FALSE)%>%filter(date=="2022-12-05"),
            aes(x=datetime,y=-depth, group=divedatYN))+
  geom_point(data=Birds_dpth_MD%>%filter(is.na(divedatYN)==FALSE)%>%filter(date=="2022-12-05"),
             aes(x=datetime,y=-depth, fill=ext_temperature_C, group=divedatYN), shape = 21)+
  theme_bw()+
  ylab("Depth (m)")+
  xlab("")+
  NULL
ggsave(filename = paste0("/Users/rachaelorben/Desktop/SA/DiveData_12-5.jpg"))

quartz(width=12,height=3)
ggplot()+
  geom_point(data=Birds_gps%>%filter(UTC_date=="2022-12-06"),
             aes(x=datetime,y=Pt, color=datatype))+
  geom_path(data=Birds_dpth_MD%>%filter(is.na(divedatYN)==FALSE)%>%filter(date=="2022-12-06"),
            aes(x=datetime,y=-depth, group=divedatYN))+
  geom_point(data=Birds_dpth_MD%>%filter(is.na(divedatYN)==FALSE)%>%filter(date=="2022-12-06"),
             aes(x=datetime,y=-depth, fill=ext_temperature_C, group=divedatYN), shape = 21)+
  theme_bw()+
  ylab("Depth (m)")+
  xlab("")+
  NULL
ggsave(filename = paste0("/Users/rachaelorben/Desktop/SA/DiveData_12-6.jpg"))


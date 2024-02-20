

usrdir2<-"/Users/alexa/Box Sync/DASHCAMS/Analysis/DataViz/Processed_Dive_Deployment_Data"

KOR_TC<-readRDS(paste0(usrdir2,"/KORYMTC23_file_1_DiveOnly.rds"))
KOR_GR<-readRDS(paste0(usrdir2,"/KORGUGR23_file_1_DiveOnly.rds"))



head(KOR_TC)

D_sum<-KOR_TC%>%group_by(device_id)%>%
  summarise(minDt=min(datetime),
            maxDt=max(datetime),
            maxDepth=max(depth_m,na.rm=TRUE),
            meanDepth=mean(depth_m, na.rm = TRUE),
            n=n(),
            uDepth=round(mean(depth_m,na.rm=TRUE),2),
            sdDepth=round(sd(depth_m,na.rm=TRUE),2))%>%
  mutate(dur=round(maxDt-minDt,2)) 

D_sum_KOR_GR<-KOR_GR%>%group_by(device_id)%>%
  summarise(minDt=min(datetime),
            maxDt=max(datetime),
            maxDepth=max(depth_m,na.rm=TRUE),
            meanDepth=mean(depth_m, na.rm = TRUE),
            n=n(),
            uDepth=round(mean(depth_m,na.rm=TRUE),2),
            sdDepth=round(sd(depth_m,na.rm=TRUE),2))%>%
  mutate(dur=round(maxDt-minDt,2)) 


str(KOR_TC)
KOR_TC$device_id<-as.character(KOR_TC$device_id)
KOR_GR$device_id<-as.character(KOR_GR$device_id)

ggplot(KOR_TC, aes(x=device_id, y=depth_m, fill=device_id)) + 
  geom_boxplot()

ggplot(KOR_GR, aes(x=device_id, y=depth_m, fill=device_id,theme_USGS_box())) + 
  geom_boxplot()

theme_USGS_box <- function(base_family = "serif", ...){
  theme_bw(base_family = base_family, ...) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 8),
      axis.ticks.length = unit(-0.05, "in"),
      axis.text.y = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm")),
      axis.text.x = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm")),
      axis.ticks.x = element_blank(),
      aspect.ratio = 1,
      legend.background = element_rect(color = "black", fill = "white")
    )
}

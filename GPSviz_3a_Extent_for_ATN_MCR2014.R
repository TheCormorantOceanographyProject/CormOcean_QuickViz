library(data.table) #fread
library(dplyr)
library(ggplot2)


if(Sys.info()[7]=="rachaelorben") {
  datadir<-'/Users/rachaelorben/Library/CloudStorage/Box-Box/DASHCAMS/data/'
}

Files<-list.files(paste0(datadir,"/2014_MCR/2014/GPS_TDR_DATA_SYNC/"),
                  pattern = ".csv",full.names = TRUE)

DAT<-NULL
for (j in 1:length(Files)){
  dat<-read.csv(file=Files[j],stringsAsFactors=FALSE,sep = ",",fill=TRUE) 
  #dat<-dat%>%filter(is.na(LatDegree)==FALSE)
  dat<-dat%>%select(BirdID,LatDegree,LongDegree,ObsDepth,CalDepth)
  DAT<-rbind(DAT,dat)
}
DAT$species<-sapply(strsplit(DAT$BirdID, split='_', fixed=TRUE), function(x) (x[1]))

min(DAT$LatDegree, na.rm=TRUE)
max(DAT$LatDegree, na.rm=TRUE)

min(DAT$LongDegree, na.rm=TRUE)
max(DAT$LongDegree, na.rm=TRUE)

max(abs(DAT$CalDepth), na.rm =TRUE)

ggplot()+
  geom_path(data=DAT%>%filter(is.na(LatDegree)==FALSE), 
            aes(x=LongDegree,y=LatDegree, group=BirdID, color=species))+
  #facet_wrap(~species)+
  NULL



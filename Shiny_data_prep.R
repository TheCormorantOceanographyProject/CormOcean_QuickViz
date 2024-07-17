##### PREP DATA
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
conflicts_prefer(dplyr::filter)

if(Sys.info()[7]=="alexa") {
  usrdir<-"/Users/alexa/Box Sync/"
  datadir<-'DASHCAMS/Analysis/DataViz/'
  savedir<-'Test1/'
  deplymatrix<-'DASHCAMS/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
}

#  Load in Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))

# change date from chr to POSIXct
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)

deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC) 
dm1<-deploy_matrix%>%select(Bird_ID,Species,TagSerialNumber,Project_ID,DeploymentStartDatetime,DeploymentEndDatetime_UTC,TagManufacture)%>%
  dplyr::filter(is.na(TagSerialNumber)==FALSE)

list(unique(dm1$Species))

# bring in project titles data

titles<-read_csv(paste0(usrdir,"data/Field Data/Project Titles and IDs.csv"))
tagreg<-titles%>%select(Project_ID,Country)
dm<-merge(dm1,tagreg, by="Project_ID")

#unique project names
prjt<-unique(deploy_matrix$Project_ID)
rm(deploy_matrix)
prjt<-prjt[prjt!="USACRBRDO14"]


locs <- data.frame()
for (i in 1:length(prjt)) {
  loc_files<-list.files("/Users/alexa/Box Sync/DASHCAMS/Analysis/DataViz/Processed_GPS_Deployment_Data", full.names = TRUE)

  temp <- data.frame()
  #prjx <- grep(prjt[i],loc_files)
  for(j in 1:length(loc_files)){
    temp <- rbind(temp, as.data.frame(readRDS(loc_files[j])))
}

temp <- temp %>%
  as.data.frame() %>%
  mutate(
    Project = prjt[i],
    Tag_ID = device_id,
  )%>%
  select(Project,Tag_ID,lat,lon,)
head(temp)
  locs<- rbind(locs,temp)
  }

saveRDS(locs,(paste0(usrdir,savedir, "project_locations.RDS"))) 


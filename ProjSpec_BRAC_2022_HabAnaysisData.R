library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table) #rename
library(stringr)
#library(argosfilter)
library(R.utils)
library(tidyr)
library(stringr)

# Link to local Box Sync folder ---- 
#To find user/computer specific username use: Sys.getenv("LOGNAME")

if(Sys.info()[4]=="benthos") {
  args = commandArgs(trailingOnly=TRUE)
  datadir<-args[1] #/home/DASHCAMS/data/ornitela_ftp_data/
  deplymatrix<-args[2] #/home/DASHCAMS/data_raw/metadata/DASHCAMS_Deployment_Field_data.csv
  savedir<-args[3] #/home/DASHCAMS/zTagStatus/
  source('/home/DASHCAMS/git/CormOcean/MakeDive.R') 
}

#if(Sys.info()[4]=="benthos") {
#  datadir<-'/home/DASHCAMS/data_raw/ornitela_ftp_data/'
#  savedir<-'/home/DASHCAMS/data_processed/zTagStatus/'
#  deplymatrix<-'/home/DASHCAMS/data_raw/metadata/DASHCAMS_Deployment_Field_data.csv'
#}

if(Sys.info()[7]=="rachaelorben") {
  datadir<-'/Users/rachaelorben/Box/DASHCAMS/data/ornitela_ftp_data/'
  savedir<-'/Users/rachaelorben/Box/DASHCAMS/data/'
  deplymatrix<-'/Users/rachaelorben/Box/DASHCAMS/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}


#  Pulls in deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(deplymatrix)
#str(deploy_matrix)
deploy_matrix<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short)%>%
  filter(is.na(TagSerialNumber)==FALSE)
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)

unique(deploy_matrix$Project_ID)

dm<-deploy_matrix%>%filter(Project_ID=="USACRBR22")

tag_ids<-dm$TagSerialNumber

my_files <- fileSnapshot(path=datadir)
Files1<-dataframe(rownames(my_files$info))
Files<-Files1[Files1 %in% Files1 == tag_ids]

lapply(Files1, function(u) tag_ids[str_detect(Files1, tag_ids)])

queries1 <- Files1 %>% 
  filter(str_detect(Files1, tag_ids, collapse = '|'))

files.sel<-NULL
for (i in 1:length(tag_ids)){
  data_new1 <- data.frame(Files1[grepl(tag_ids[i], Files1)])
  files.sel<-rbind(files.sel,data_new1)
}

names(files.sel)<-"files"

Birds<-NULL 
for (i in 1:nrow(files.sel)){
  
  fileN<-files.sel$files[i]
  tagID<-sapply(strsplit(files.sel[i,], split='_', fixed=TRUE), function(x) (x[1]))
  
  deply_sel<-deploy_matrix[deploy_matrix$TagSerialNumber==tagID[1],]
  dat<-read.csv(file = paste0(datadir,files.sel[i,]),sep = ",") #could switch to fread to be quicker...
  
  dat$DeploymentStartDatetime<-deply_sel$DeploymentStartDatetime
  dat$Deployment_End_Short<-deply_sel$Deployment_End_Short
  
  Birds<-rbind(Birds,dat)
}
  
saveRDS(Birds,paste0(savedir,"BRAC_2022.rds")
  

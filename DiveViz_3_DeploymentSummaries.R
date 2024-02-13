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
  deplymatrix<-'/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
  source('/Users/rachaelorben/git_repos/CormOcean/MakeDive.R')
}

op <- options(digits.secs=3)

#  Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
dm<-deploy_matrix%>%dplyr::select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime,Deployment_End_Short,DeploymentEndDatetime_UTC,TagManufacture)%>%
  filter(is.na(TagSerialNumber)==FALSE)

#all project names
prjt_all<-unique(dm$Project_ID)

#projects with tags currently deployed
tags_current<-dm%>%filter(is.na(DeploymentEndDatetime_UTC))
prjt_current<-unique(tags_current$Project_ID)

#projects with all end dates complete
prjt_complete<-prjt_all[!(prjt_all %in% prjt_current)]

#projects to process: change as needed
#prjt<-prjt_current
prjt<-prjt_all
prjt<-prjt[prjt!="USACRBRDO14"]


# Loop through each project -----------------------------------------------
# you can run project individually by picking an i value i=10 gives you "PERIPGU22_SC" etc. 
# then just run the code after the initial for statement
for (i in i:length(prjt)){
  
  # Find Project Data Files 
  Files<-list.files(paste0(usrdir,savedir,"Processed_DiveID_ByBird/"), pattern = prjt[i],full.names = TRUE)
  filenames<-list.files(paste0(usrdir,savedir,"Processed_DiveID_ByBird/"),pattern = prjt[i])
  
  Birds_dpth<-NULL
  for (k in 1:length(Files)){
    birdy_d<-readRDS(paste0(usrdir,savedir,"Processed_DiveID_ByBird/",filenames[k]))
    Birds_dpth<-rbind(Birds_dpth,birdy_d)
  }
  rm(birdy_d)
  
  saveRDS(Birds_dpth, paste0(usrdir,savedir,"Processed_DiveID_ByDeployment/",prjt[i],"_DiveID.rds"))
}



# Pulls full deployment data back in and summarizes -----------------------


# Find Project Data Files 
filenames<-list.files(paste0(usrdir,savedir,"Processed_DiveID_ByDeployment"), full.names = TRUE)

D_sum_AllB<-NULL
daily_sum_AllB<-NULL
dive_sum_AllB<-NULL
for (k in 1:length(filenames)){
  Birds_dpth<-readRDS(filenames[k])
  
  names(Birds_dpth)
  Birds_dpth<-rename(Birds_dpth, "dive_id" = "divedatYN")
  
  D_sum<-Birds_dpth%>%group_by(Project, ID)%>%
    summarise(minDt=min(datetime),
              maxDt=max(datetime),
              maxDepth=max(depth,na.rm=TRUE),
              n=n(),
              uDepth=round(mean(depth,na.rm=TRUE),2),
              sdDepth=round(sd(depth,na.rm=TRUE),2))%>%
    mutate(dur=round(maxDt-minDt,2)) 
  
  daily_sum<-Birds_dpth%>%group_by(Project, ID, date)%>% 
    filter(!is.na(depth))%>% #removes rows with NA values for depth - needs to be switched to days with DiveSensor/ON-OFF events (to do later)
    summarise(minDt=min(datetime),
              maxDt=max(datetime),
              maxDepth=max(depth,na.rm=TRUE),
              nDives=n_distinct(dive_id),
              uDepth=round(mean(depth,na.rm=TRUE),2),
              sdDepth=round(sd(depth,na.rm=TRUE),2))

  dive_sum<-Birds_dpth%>%group_by(Project, ID, date,dive_id)%>% 
    filter(!is.na(depth))%>% #removes rows with NA values for depth - needs to be switched to days with DiveSensor/ON-OFF events (to do later)
    summarise(minDt=min(datetime),
              maxDt=max(datetime),
              maxDepth=max(depth,na.rm=TRUE),
              nDives=n_distinct(dive_id),
              uDepth=round(mean(depth,na.rm=TRUE),2),
              sdDepth=round(sd(depth,na.rm=TRUE),2))
  
  #puts each deployment in a master spreadsheet
  D_sum_AllB<-bind_rows(D_sum_AllB, D_sum)
  daily_sum_AllB<-bind_rows(daily_sum_AllB, daily_sum)
  dive_sum_AllB<-bind_rows(dive_sum_AllB, dive_sum)
  
  #makes a few deployment specific plots
  dt<-Sys.Date()
  ggplot()+
    geom_point(data=daily_sum, aes(y=nDives,x=date,group=ID, color=as.factor(ID)), size=0.05)+
    labs(title=prjt[i])+
    ylab("")+
    theme(legend.title=element_blank())+
    guides(colour = guide_legend(override.aes = list(size=3)))
  ggsave(paste0(usrdir,savedir,"PLOTS/Dives_DailySummary/",prjt[i],"_DivesPerDay.png"), dpi=300)

  ggplot()+
    geom_point(data=daily_sum, aes(y=-uDepth,x=date,group=ID,color=as.factor(ID)), size=0.05)+
    geom_point(data=daily_sum, aes(y=-maxDepth,x=date, group=ID,color=as.factor(ID)), size=0.05)+
    labs(title=prjt[i])+
    ylab("")+
    theme(legend.title=element_blank())+
    guides(colour = guide_legend(override.aes = list(size=3))) 
  ggsave(paste0(usrdir,savedir,"PLOTS/Dives_DailySummary/",prjt[i],"_Mean&MaxDepth.png"), dpi=300)
  
}


# Plots of multi-deployment summary data ----------------------------------
# Project info ------------------------------------------------------------
prj_info<-read.csv(paste0(usrdir,"/data/Field Data/Project Titles and IDs.csv"))
dive_sum_AllB<-left_join(dive_sum_AllB,prj_info,by="Project_ID")

# dive summary

names(dive_sum_AllB)

#not a useful plot - you want the daily dive summary instead for plotting
ggplot()+
  geom_point(data=dive_sum_AllB, aes(x=nDives, y=-maxDepth))


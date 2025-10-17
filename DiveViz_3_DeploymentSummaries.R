library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table) #rename
library(stringr)
library(R.utils)
library(tidyr)
library(R.matlab)

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

op <- options(digits.secs=3)

#  Deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(usrdir,deplymatrix))
deploy_matrix$DeploymentStartDatetime_Local<-mdy_hm(deploy_matrix$DeploymentStartDatetime_Local)-(deploy_matrix$UTC_offset_deploy*60*60)
deploy_matrix$DeploymentEndDatetime_UTC<-mdy_hm(deploy_matrix$DeploymentEndDatetime_UTC)
dm<-deploy_matrix%>%dplyr::select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime_Local,Deployment_End_Short,DeploymentEndDatetime_UTC,TagManufacture)%>%
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
#prjt<-prjt[prjt!="USACRBRPE19"] #I am not sure why this one is missing

prjt
# Loop through each project -----------------------------------------------
# you can run project individually by picking an i value i=10 gives you "PERIPGU22_SC" etc. 
# then just run the code after the initial for statement
for (i in 23:length(prjt)){
  
  # Find Project Data Files 
  Files<-list.files(paste0(usrdir,savedir,"Processed_2_DiveID_ByBird/"), pattern = prjt[i], full.names = TRUE)
  filenames<-list.files(paste0(usrdir,savedir,"Processed_2_DiveID_ByBird/"),pattern = prjt[i])
  
  Birds_dpth<-NULL
  for (k in 1:length(Files)){
    birdy_d<-readRDS(paste0(usrdir,savedir,"Processed_2_DiveID_ByBird/",filenames[k]))
    Birds_dpth<-bind_rows(Birds_dpth,birdy_d)
  }
  rm(birdy_d)

saveRDS(Birds_dpth, paste0(usrdir,savedir,"Processed_3_DiveID_ByDeployment/",prjt[i],"_DiveID.rds"))
#filename=paste0(usrdir,savedir,"Processed_3_DiveID_ByDeployment_mat/",prjt[i],"_DiveID.mat")

#rao: commented out Sept 2024 since data and column names don't match 
#rao: I think these might have been for Jim, but I am not sure if they are still needed?
# writeMat(filename,
#          oid=Birds_dpth$oid,
#          ID=Birds_dpth$ID,
#          datetime=Birds_dpth,
#          fixNames=TRUE)
}


# Pulls full deployment data back in and summarizes -----------------------


# Find Project Data Files 
filenames<-list.files(paste0(usrdir,savedir,"Processed_3_DiveID_ByDeployment"), full.names = TRUE)

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
             sdDepth=round(sd(depth,na.rm=TRUE),2))%>%
    mutate(dive_dur=maxDt-minDt)
  
  #puts each deployment in a master spreadsheet
  D_sum_AllB<-bind_rows(D_sum_AllB, D_sum)
  daily_sum_AllB<-bind_rows(daily_sum_AllB, daily_sum)
  dive_sum_AllB<-bind_rows(dive_sum_AllB, dive_sum)
  
  #makes a few deployment specific plots
  # dt<-Sys.Date()
  # ggplot()+
  #   geom_point(data=daily_sum, aes(y=nDives,x=date,group=ID, color=as.factor(ID)), size=0.05)+
  #   labs(title=prjt[i])+
  #   ylab("")+
  #   theme(legend.title=element_blank())+
  #   guides(colour = guide_legend(override.aes = list(size=3)))
  # ggsave(paste0(usrdir,savedir,"PLOTS/Dives_DailySummary/",prjt[i],"_DivesPerDay.png"), dpi=300)
  # 
  # ggplot()+
  #   geom_point(data=daily_sum, aes(y=-uDepth,x=date,group=ID,color=as.factor(ID)), size=0.05)+
  #   geom_point(data=daily_sum, aes(y=-maxDepth,x=date, group=ID,color=as.factor(ID)), size=0.05)+
  #   labs(title=prjt[i])+
  #   ylab("")+
  #   theme(legend.title=element_blank())+
  #   guides(colour = guide_legend(override.aes = list(size=3))) 
  # ggsave(paste0(usrdir,savedir,"PLOTS/Dives_DailySummary/",prjt[i],"_Mean&MaxDepth.png"), dpi=300)
  # 
}


# Plots of multi-deployment summary data ----------------------------------
# Project info ------------------------------------------------------------
prj_info<-read.csv(paste0(usrdir,"/data/Field Data/Project Titles and IDs.csv"))

D_sum_AllB<-left_join(D_sum_AllB,prj_info,by=c("Project"="Project_ID"))
daily_sum_AllB<-left_join(daily_sum_AllB,prj_info,by=c("Project"="Project_ID"))
dive_sum_AllB<-left_join(dive_sum_AllB,prj_info,by=c("Project"="Project_ID"))

# Save multiple objects
save(D_sum_AllB, daily_sum_AllB, dive_sum_AllB, file = paste0(usrdir,savedir,"/DiveSummaryFiles_AllBirds.RData"))
read(paste0(usrdir,savedir,"/DiveSummaryFiles_AllBirds.RData"))

# dive summary

# box plot of MAX Depth of each dive: from individual dive statistics 
names(dive_sum_AllB)
ggplot()+
  geom_boxplot(data=dive_sum_AllB%>%filter(Project!="BAHHASO22")%>%filter(maxDepth<100)%>%
                 filter(Species_Long!="Pelagic Cormorant & Brandt's Cormorant"),
               aes(group=Species_Long, y=-maxDepth, fill=Species_Long))+
  ylab("Dive Depth (m)")+
  theme_classic()+
  theme(axis.text.x = element_blank())
ggsave(paste0(usrdir,savedir,"PLOTS/SpeciesDiveDepthCompaire.png"), dpi=300)

# box plot of dive duration of each dive: from individual dive statistics 
dive_sum_AllB$Dur_sec<-as.numeric(dive_sum_AllB$dive_dur)
hist(dive_sum_AllB$Dur_sec)
ggplot()+
  geom_boxplot(data=dive_sum_AllB%>%filter(Project!="BAHHASO22")%>%filter(maxDepth<100)%>%
                 filter(Dur_sec<180)%>% #something is going on here - looks OK with these filtered out though
                 filter(Species_Long!="Pelagic Cormorant & Brandt's Cormorant"),
               aes(group=Species_Long, y=Dur_sec, fill=Species_Long))+
  ylab("Dive Duration (sec)")+
  theme_classic()+
  theme(axis.text.x = element_blank())
ggsave(paste0(usrdir,savedir,"PLOTS/SpeciesDiveDurationCompaire.png"), dpi=300)

# box plot of dives / day : from daily dive statistics 
names(daily_sum_AllB)
ggplot()+
  geom_boxplot(data=daily_sum_AllB%>%filter(Project!="BAHHASO22")%>%
               filter(Species_Long!="Pelagic Cormorant & Brandt's Cormorant"),
               aes(group=Species_Long, y=nDives, fill=Species_Long))+
  ylab("Dives / Day")+
  theme_classic()+
  theme(axis.text.x = element_blank())
ggsave(paste0(usrdir,savedir,"PLOTS/SpeciesDivePerDayCompaire.png"), dpi=300)



################ Plots for Presentation #############

## dive depth ordered by average species mass(g)

mass_rank<-read.csv(paste0(usrdir,"data/Field Data/Species_Av_Mass.csv"))

#sp_grp<-read.csv(paste0(usrdir, "data/Field Data/group_level.csv")) #  species family level

dive_sum_mass<-left_join(dive_sum_AllB,mass_rank, by ="Species_Long")

#dive_sum_massf<-left_join(dive_sum_mass, sp_grp, by = "Species_Long") # adds family level column 

names(dive_sum_mass)

dive_sum_mass$pencorm<-"Cormorant"
dive_sum_mass$pencorm[dive_sum_mass$Species=="HUPE"]<-"Penguin"
dive_sum_mass$pencorm[dive_sum_mass$Species=="AFPE"]<-"Penguin"

ggplot()+
  geom_boxplot(data=dive_sum_mass%>%filter(Project!="BAHHASO22", Project!="SOUDICA22")%>%filter(maxDepth<100)%>%
                 filter(Species_Long!="Pelagic Cormorant & Brandt's Cormorant", Species_Long!="Humboldt Penguin", Species_Long!="African Penguin"),
               aes(group=reorder(Species_Long,Rank), y=-maxDepth, fill= reorder(Species_Long,Rank)))+
  labs(fill = "Common Name")+
  ylab("Dive Depth (m)")+
  theme_classic()+
  theme(axis.text.x = element_blank())
#+
  #facet_wrap(~pencorm, scales = "free_x")

ggsave(paste0(usrdir,savedir,"PLOTS/SpeciesDiveDepth_ByMass_Corms.png"), dpi=300) 


###### Dive Duration

dive_sum_mass$Dur_sec<-as.numeric(dive_sum_mass$dive_dur)
hist(dive_sum_mass$Dur_sec)

ggplot()+
  geom_boxplot(data=dive_sum_mass%>%filter(Project!="BAHHASO22", Project!="SOUDICA22")%>%filter(maxDepth<100)%>%
                 filter(Dur_sec<180)%>% #something is going on here - looks OK with these filtered out though
                 filter(Species_Long!="Pelagic Cormorant & Brandt's Cormorant", Species_Long!="Humboldt Pengiun", Species_Long!="African Penguin"),
               aes(group=Species_Long, y=Dur_sec, fill=Species_Long))+
  labs(fill = "Common Name")+
  ylab("Dive Duration (sec)")+
  theme_classic()+
  theme(axis.text.x = element_blank())
#ggsave(paste0(usrdir,savedir,"PLOTS/SpeciesDiveDurationCompaire.png"), dpi=300)


#####
  
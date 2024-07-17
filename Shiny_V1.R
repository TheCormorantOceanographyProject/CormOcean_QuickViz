##Shniy-DataViz_DeploymentMaps###

#load packages

library(tidyverse)
library(R.utils)
library(sf)
library(MetBrewer)
library(shiny)
library(conflicted)
library(glue)
library(ggmap)
library(osmdata)
conflicts_prefer(dplyr::filter)

library(argosfilter)
library(ggpubr)

if(Sys.info()[7]=="alexa") {
  usrdir<-"/Users/alexa/Box Sync/"
  datadir<-'Test1/'
  savedir<-'DASHCAMS/Analysis/DataViz/'
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

titles<-read.csv(paste0(usrdir,"DASHCAMS/data/Field Data/Project Titles and IDs.csv"))
tagreg<-titles%>%select(Project_ID,Country)
dm<-merge(dm1,tagreg, by="Project_ID")


#all project names
prjt<-unique(deploy_matrix$Project_ID)
rm(deploy_matrix)
prjt<-prjt[prjt!="USACRBRDO14"]

# test with one project

NORSK23<-read.csv(paste0(usrdir,datadir,("NORSKEU23_GPS.csv")))
str(NORSK23)

NORSK23$Tag_ID <- as.character(NORSK23$Tag_ID) # convert Tag serials to character

#rename headings to use in shiny
NORSK23 <- NORSK23 %>% rename(Tag_ID = device_id) #order is new name first!
 
# create map options

for (i in 1:length(prjt)){
  
locs<-readRDS(paste0(usrdir,savedir,"Processed_GPS_Deployment_Data/",prjt[i],"_GPS_SpeedFiltered.rds"))
  
w2hr<-map_data('world')

locs_wgs84<-st_as_sf(NORSK23,coords=c('lon','lat'),remove = F,crs = 4326)

y_min<-min(NORSK23$lat)-.25
y_max<-max(NORSK23$lat)+.25
x_min<-min(NORSK23$lon)-.25
x_max<-max(NORSK23$lon)+.25

ids<-unique(NORSK23$Uni_ID)
}

#create a global function for map plots

#plot_location <- function(data = loc_wgs4, Tag_ID, 
 #                         , base = coast) {

# shiny 

ui<-fluidPage(
  titlePanel("European Shag movement"),
  
  #sidebarLayout(
   sidebarPanel(
     h6('This app explores the foraging locations of European Shags originating 
               from Sklinna Island colony, Norway, during the summer of 2023.'),
     
    #  selectInput("dm", label = "Select a project",
     #               choices = c(unique(as.character(dm$Project_ID)))),
     
     selectInput("NORSK23", label = "Select tag_ID for Individual Tracks",
                  choices = c(unique(as.character(NORSK23$Tag_ID)),
                              selected = '233703', multiple = FALSE))),
       uiOutput('TagSelect'),
  
      mainPanel(
        plotOutput("GPS_plot"))
      
)
    #)
  server <- function(input, output, session) {
    
 #   rv <- reactiveValues(prev_tag = '233703')
    
   # observeEvent(input$NORSK23, {
      # If event occurs, then run the following append function
  #    rv$
  # )
  
  output$GPS_plot <- renderPlot({
  
     ggplot()+
        geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray50",color="grey5",linewidth=0.1)+
        geom_sf(data = locs_wgs84, aes(color=NORSK23$Tag_ID), size=.01)+
        scale_color_manual(values=met.brewer("Tam", length(ids)))+
        coord_sf(xlim = c(x_min,x_max),ylim=c(y_min,y_max))+
        xlab("Longitude")+
        ylab("Latitude")+
        theme_bw()+
        theme(legend.title=element_blank(),
            legend.text = element_blank())+
      guides(colour = guide_legend(override.aes = list(size=3)))
  }
    
  )
  
}
shinyApp(ui, server)



##### Select by Project ## Global R ###

locs <- readRDS(paste0(usrdir,datadir,'project_locations.RDS'))
plot_location <- function(data = locs, Project, 
                          lat,lon) {
  
  temp <- data %>% 
    filter(Project == Project_ID)
  
  w2hr<-map_data('world')
  
  locs_wgs84<-st_as_sf(locs,coords=c('lon','lat'),remove = F,crs = 4326)
  
  y_min<-min(locs$lat)-.25
  y_max<-max(locs$lat)+.25
  x_min<-min(locs$lon)-.25
  x_max<-max(locs$lon)+.25
  
   ids<-unique(locs$Project)
  
 
  ggplot()+
    geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray50",color="grey5",linewidth=0.1)+
    geom_sf(data = locs_wgs84, aes(color=locs$Project), size=.01)+
    scale_color_manual(values=met.brewer("Tam", length(ids)))+
    coord_sf(xlim = c(x_min,x_max),ylim=c(y_min,y_max))+
    xlab("Longitude")+
    ylab("Latitude")+
    theme_bw()+
    theme(legend.title=element_blank(),
          legend.text = element_blank())+
    guides(colour = guide_legend(override.aes = list(size=3)))
  
  
  
}


### project shiny 

ui<-fluidPage(
  titlePanel("Cormorant movement data by Project"),
  
  #sidebarLayout(
  sidebarPanel(
    h6('This app explores the foraging locations of Cormorants and Shags from tagging projects acorss the globe.'),
    
    #  selectInput("dm", label = "Select a project",
    #               choices = c(unique(as.character(dm$Project_ID)))),
    
    selectInput(inputId = "project_id", label = "Select Project for tagging project",
                choices = c(unique(as.character(locs$Project)),
                            selected = 'USACRBRPE19', multiple = FALSE))),
  # uiOutput('TagSelect'),
  
  mainPanel(
    plotOutput("GPS_plot"))
  
)
#)
server <- function(input, output, session) {
  
  #   rv <- reactiveValues(prev_tag = '233703')
  
  # observeEvent(input$NORSK23, {
  # If event occurs, then run the following append function
  #    rv$
  # )
  
  output$GPS_plot <- renderPlot({
    
    plot_location(data = locs)
    
  }
  
  )
  
}
shinyApp(ui, server)


########################################## To update later ##############################
#create a function that loops through the project files when a folder is called

#ui<-fluidPage(
 # titlePanel("Tag deployments by project"),

  #sidebarLayout(
   # sidebarPanel( 
    #  selectInput("dm", label = "Select a project",
#                 choices = c(unique(as.character(dm$Project_ID)))))),
#  mainPanel(
 #   plotOutput("Project_plot")
#)
#)

#server <- function(input, output, session) {
 # Project_plot<- reactive(for (i in 1:length(prjt)){
    
  #  locs<-readRDS(paste0(usrdir,savedir,"Processed_GPS_Deployment_Data/",prjt[i],"_GPS_SpeedFiltered.rds"))
   # dm_prj<-dm%>%filter(Project_ID==prjt[i])
    
  #  w2hr<-map_data('world')
    
   # locs_wgs84<-st_as_sf(locs,coords=c('lon','lat'),remove = F,crs = 4326)
    
  #  y_min<-min(locs$lat)-.25
  #  y_max<-max(locs$lat)+.25
  #  x_min<-min(locs$lon)-.25
  #  x_max<-max(locs$lon)+.25
    
 #   ids<-unique(locs$Uni_ID)
    
    ##quartz()
#    ggplot()+
#     geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray50",color="grey5",linewidth=0.1)+
#      geom_sf(data = locs_wgs84, aes(color=Uni_ID), size=.01)+
#      scale_color_manual(values=met.brewer("Tam", length(ids)))+
#      coord_sf(xlim = c(x_min,x_max),ylim=c(y_min,y_max))+
#       xlab("Longitude")+
#       ylab("Latitude")+
#       theme_bw()+
#       theme(legend.title=element_blank(),
#       legend.text = element_blank())+
#            guides(colour = guide_legend(override.aes = list(size=3)))
    
  #)

#output$Project_plot<- renderPlot()
 # } 
#shinyApp(ui, server)

###### original maps code
#for (i in 1:length(prjt)){
  
#  locs<-readRDS(paste0(usrdir,savedir,"Processed_GPS_Deployment_Data/",prjt[i],"_GPS_SpeedFiltered.rds"))
#  dm_prj<-dm%>%filter(Project_ID==prjt[i])
  
#  w2hr<-map_data('world')
  
#  locs_wgs84<-st_as_sf(locs,coords=c('lon','lat'),remove = F,crs = 4326)
  
#  y_min<-min(locs$lat)-.25
#  y_max<-max(locs$lat)+.25
#  x_min<-min(locs$lon)-.25
#  x_max<-max(locs$lon)+.25
  
#  ids<-unique(locs$Uni_ID)
  
  #quartz()
#  ggplot()+
#    geom_polygon(data=w2hr,aes(long,lat,group=group),fill="gray50",color="grey5",linewidth=0.1)+
#    geom_sf(data = locs_wgs84, aes(color=Uni_ID), size=.01)+
#    scale_color_manual(values=met.brewer("Tam", length(ids)))+
#    coord_sf(xlim = c(x_min,x_max),ylim=c(y_min,y_max))+
#    xlab("Longitude")+
#    ylab("Latitude")+
#    theme_bw()+
#    theme(legend.title=element_blank(),
#          legend.text = element_blank())+
#    guides(colour = guide_legend(override.aes = list(size=3)))
# }  

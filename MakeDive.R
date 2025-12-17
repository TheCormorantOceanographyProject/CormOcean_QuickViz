#' MakeDive Function to make a DiveNum variable from development trackR
#'
#' The objective of this function to to partition diving data into "dives"
#' based on begining and returning to a fixed location (the surface).
#' It was based on the MakeTrip codes.  This function relies on a column with a
#' depth typically in m, a depth cut off below which dives start, but also the user
#' can specify what depth must be reached to indicate a dive and also specify a time
#' gap limit.  It numbers each dive starting with the first point beyond the
#' depth cutoff and ending with the last point beyond the cutoff with a
#' sequential number starting with 1.  All points inside the cutoff (surface) are
#' labled 0. Output is just the input variables and a counter column. Depth values can
#' not include NA.
#'
#' In the future it could be nice to have these labeled with sequetial numbers as well.
#' The input variables are identified by column index numbers in the data.frame (tracks).
#'
#' @author Rachael A. Orben & Abram B. Fleishman \email{abram@@conservationmetrics.com}
#' @param tracks data.frame of data that you want to parse into trips
#' @param idCol column index of column in data that is a unique key to individual
#'   bird_tag_deployment combos.
#' @param dtCol column index with datetime
#' @param depthCol column index with depth, typically in meters
#' @param tdiffCol column index with time difference in seconds. numeric
#' @param DepthCutOff Depth to use as a cut off to indicate surface values
#'   to use to split the dives (same units as depth). Default is 1.
#' @param DiveDepthYes Dives need to reach this depth to be considered a
#'   dive event; default is 3.
#' @param TimeDiffAllowed_sec Consecuative points need to have a time
#'   difference less than this parameter to be in the same event. Default is <2sec
#' @param NumLocCut  #dives need to contain this number of points to be
#'   considered a dive, could change this to a duration
#' @return A new data frame with input variables data plus two new columns: TripNum
#'   (consecutive trip number 0 = at colony) and ColonyMovement (in/out colony
#'   movements)
#' @importFrom dplyr lead bind_rows
#' @export

MakeDive<-function(tracks,
                   idCol=2, #column index with unique ID
                   dtCol=6, #column index with datetime
                   depthCol=9, #column index with depth
                   tdiffCol=18, #column index with time difference in seconds
                   DepthCutOff=1, #depth that dives happen below (meters)
                   DiveDepthYes=3, #dives need to reach 3 meters to be considered a dive event
                   TimeDiffAllowed_sec=2, #consecuative points need to have a time difference <2 to be in the same event
                   NumLocCut=3){ #dives need to contain three points to be considered a dive, could change this to a duration
  
  dat=data.frame(oid=1:nrow(tracks),
                 ID=tracks[,idCol],
                 datetime=tracks[,dtCol],
                 depth=tracks[,depthCol],
                 tdiff=tracks[,tdiffCol])
  colnames(dat)<-c("oid","ID","datetime","depth","tdiff")
  
  IDs<-unique(dat$ID)
  
  dat$divedatYN<-NA
  dat$DiveID<-NA
  for(j in 1:length(IDs)){
    
    #timer start
    ptm <- proc.time()
    
    # Subset for each bird
    BirdSub<-dat%>%dplyr::filter(ID==IDs[j])
    BirdSub$Surface<-NULL
    
    # Print bird number
    print(BirdSub$ID[1])
    
    # Find consecutively recorded dive data
    BirdSub<- BirdSub %>%
      mutate(gap_time=tdiff>TimeDiffAllowed_sec, # find times when there is a gap > 60 minutes
             gap_time=ifelse(is.na(gap_time),0,gap_time), #fill NAs
             divedatID=(cumsum(gap_time)))#, # gap_time is T/F so cumsum is adding 1
    #for each T aka giving a index number to each gap
    
    # Identify dive data with dives >DiveDepthYes
    dSum<-BirdSub%>%
      group_by(divedatID)%>%
      summarise(n=n(),
                maxD=max(depth),
                min_oid=min(oid),
                max_oid=max(oid),
                maxDt=max(datetime),
                minDt=min(datetime),
                first = dplyr::first(depth), #error checking
                last = dplyr::last(depth))%>% #many dive don't return to the surface
      dplyr::filter(maxD>=DiveDepthYes)%>%
      dplyr::filter(n>=NumLocCut) #removes dive data chuncks with only 1 or 2 data points
    
    print(nrow(dSum))
    
    if (nrow(dSum)==0) next
    
    for (k in 1:nrow(dSum)){
      print(k)
      BirdSub_dive<-BirdSub%>%filter(divedatID==dSum$divedatID[k])
      # If distance to surface is less than DistCutOff m make it a 0 else make it a 1
      # 0 =  surface 1 = on dive
      #BirdSub$InColony<-ifelse(BirdSub[[Dist2Colony]] < DistCutOff|is.na(BirdSub[[Dist2Colony]]),0,1)
      BirdSub_dive$Surface<-ifelse(BirdSub_dive$depth < DepthCutOff|is.na(BirdSub_dive$depth),0,1)
      
      # offset by one (drop first record) Detect state change for "out" events
      # else NA
      # if the current point is in the colony but the next point is out of
      # the colony label it an "out"
      BirdSub_dive$DiveMovement<-ifelse(BirdSub_dive$Surface == 0 & lead(BirdSub_dive$Surface) == 1,
                                        "Out", NA)
      
      # Detect state change for "In" events else "out" or NA
      # if the current ooint is out of the colony but the next point is in
      # the colony label it an "in"
      BirdSub_dive$DiveMovement<-ifelse(BirdSub_dive$Surface == 1 & lead(BirdSub_dive$Surface) == 0,
                                        "In", BirdSub_dive$DiveMovement)
      
      # Get indicies of out events
      Out<-grep("Out", x = BirdSub_dive$DiveMovement)
      
      # If there is an "in" event get the indicies of the in events
      # else make the 2nd to last point an in event
      if ("In" %in% BirdSub_dive$DiveMovement ) {
        In<-grep("In", x = BirdSub_dive$DiveMovement)
      } else {
        In<-length(BirdSub_dive$DiveMovement)-1
      }
      
      # if the first "in" comes after the first "out" than make first index an "out"
      if(In[1]<Out[1]|length(Out)==0)  Out<-c(1,Out)
      
      #  if the last out is a larger index than the last in make the last event an "in"
      if(Out[length(Out)]>In[length(In)]) In<-c(In,nrow(BirdSub_dive)-1)
      
      # add 1 to all the indecies in "in" so that they are the first point at colony
      In<-In+1
      
      # get the indeces of the ins and outs that have more than NumLocCut points
      sel<-which(abs(In-Out)>NumLocCut)
      
      #Trims trips that are shorter than a given number of locations, set to 0 to omit
      In<-In[sel]
      Out<-Out[sel]
      
      # add a vector of NAs to BirdSub that will be populated with trip numbers
      BirdSub_dive$Dive<-NA
      
      # add a tripnumber "i" to all the events between the ith out and ith in
      if(length(In)==0){
        warning(paste(BirdSub_dive$ID[1],":", k, "did not make any dives. If this is an error, reduce DepthCutOff or NumLocCut.\n",
                      BirdSub_dive$ID[1],":", k,"has fewer then NumLocCut=",NumLocCut,"points within DepthCutOff=", DepthCutOff,"."))
      }else{
        for(i in 1:length(Out)){
          BirdSub_dive$Dive[Out[i]:In[i]]<-i
        }
      }
      
      dat$divedatYN[BirdSub_dive$oid]<-BirdSub_dive$divedatID
      dat$DiveID[BirdSub_dive$oid]<-BirdSub_dive$Dive
      #gc() # Force R to release memory it is no longer using
    }
  }
  
  dat<-rename(dat,diveID=divedatYN)
  dat<-rename(dat,divedat1=DiveID)
  return(dat)
}

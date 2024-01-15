# CormOcean_QuickViz
Quick visualization for biologging tags deployed for the Cormorant Oceanography Project

**TagStatus**
These scripts provide real-time updates from the hourly FTP data to keep track of deployed birds. 

TagStatus_1wk.R 
This script is run daily by benthos to make quick diagnotic plots of the last week of tag data. 

TagStatus_24hr.R 
This script has an error and doesn't run. Could be updated to compliment 1wk output. 

**Realtime_Profiles**
realtime_profiles.R Script to make temperature and salinity profiles to send to ATN for output to GTS. 

**AxyPositionCalibrations**
AxyPositionCalibrations.R This script finds the position calibration segments.

**GPSviz**
These scripts process GPS data only. The data is pulled from the ATN .csv dataset. The GPS data is speed filtered, mapped, and individual duration plots are made. There is also a script to plot the data on a gloabl map. This series needs updating so that is is using the same datasets and the workflow matches. 

**Diveviz**
This series is meant to process and vizualize the diving data. More work needed. 

**GPSDive**
Combines GPS and dive data.

**ProjSpec**
These are specific project based scripts to meet collaborators needs or to investigate specific data. 

FUNCTION
MakeDive.R Function for making dives. DiveDataYN = Dive index & variable output needs to be changed. 

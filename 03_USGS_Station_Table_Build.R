##### loading required libraries #####
library(RCurl)
library(XML)
require(plyr)
require(RODBC)
require(reshape2)
require(stringr)
library(foreign)

options("scipen"=100, stringsAsFactors = FALSE)

#### Generate precursor objects and functions ####
source('//deqhq1/wqassessment/2012_WQAssessment/DO_Evaluation/R_scripts/01a_LASAR_DO.R')
source('//deqhq1/wqassessment/2012_WQAssessment/DO_Evaluation/R_scripts/01b_USGS_DO.R')
source('//deqhq1/wqassessment/2012_WQAssessment/DO_Evaluation/R_scripts/02_DO_Evaluation_Code_v2.R')
source('//deqhq1/wqassessment/2012_WQAssessment/Segmentation/R_scripts/Toxics clean up v2.R')
source('//deqhq1/wqassessment/2012_WQAssessment/Segmentation/R_scripts/Segmentation functions.R')
source('//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/02_WQPData_Preprocessing.R')
source('//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/01a_LASARQuery.R') 

#make station identifiers consistent across platform
wqp.usgs.stations <- wqp.stations[grep('USGS',wqp.stations$MonitoringLocationIdentifier),]
wqp.usgs.stations$Site_no <- gsub('USGS-','',wqp.usgs.stations$MonitoringLocationIdentifier)


#### Modify DO and Toxics input tables to be consistent ####
stations.coded.use <- rename(stations.coded.use, c('site_no' = 'Site_no', 'exceeds' = 'exceed', 'Valid_n' = 'total_n', '%exceed' = 'percent.exceed',
                                                   '2010_303d_cat' = 'X2010_303d_cat', 'Do Not Use for 2012?' = 'use', 'Trend_recent?' = 'recent_trend'))

stations.coded.use <- stations.coded.use[,c('Site_no','exceed','total_n','percent.exceed','Date_start','Date_stop','cat','Pollutant',
                                            'Agency', 'station_name', 'Stream_Name','LLID_Stream', 'RM', 'LLID_Lake','Lake_Name','crit_val','Season','TOTAL')]
stations.coded.use$ID <- NA
stations.coded.use <- rename(stations.coded.use, c('station_name'='Site_name', 'Stream_Name'='Str_name','LLID_Stream'='Str_LLID', 
                                                   'RM'='Str_RM', 'LLID_Lake'='LAKE_LLID', 'Lake_Name'='LAKE_NAME'))

DO.stations.info <- DO.stations.info[,c('ID','Agency', 'Site_no', 'Site_name', 'Str_name','Str_LLID', 'Str_RM', 'LAKE_LLID', 'LAKE_NAME')]

DO.final <- rename(DO.final, c('STATION'='Site_no','V1digressions'='exceed','V1totals'='total_n','percent'='percent.exceed'))

DO.final <- merge(DO.final, DO.stations.info, by = 'Site_no', all.x = TRUE)

DO.final$TOTAL <- NA

stations.all <- rbind(stations.coded.use, DO.final)

#Creates a consistently formatted LLID Stream Lake field
stations.all$LLID_Stream_Lake <- ifelse(is.na(stations.all$LAKE_LLID), 
                                        stations.all$Str_LLID,
                                        ifelse(is.na(stations.all$Str_LLID),
                                               stations.all$LAKE_LLID,
                                               paste(stations.all$Str_LLID, stations.all$LAKE_LLID, sep = '/')))

#check wqp stations against usgs stations already located
#matched.nums <- wqp.usgs.stations$Site_no[wqp.usgs.stations$Site_no %in% unique(stations.all[nchar(stations.all$Site_no) > 5,c('Site_no')])]
#new.station.nums <- wqp.usgs.stations$Site_no[!wqp.usgs.stations$Site_no %in% unique(stations.all[nchar(stations.all$Site_no) > 5,c('Site_no')])]

#matched.names <- wqp.usgs.stations$MonitoringLocationName[wqp.usgs.stations$MonitoringLocationName %in% unique(stations.all[nchar(stations.all$Site_no) > 5,c('Site_name')])]
#new.station.names <- wqp.usgs.stations$MonitoringLocationName[!wqp.usgs.stations$MonitoringLocationName %in% unique(stations.all[nchar(stations.all$Site_no) > 5,c('Site_name')])]

#The numbers of matches on site number are different than number of matches on site name
#this identifies where we have those discrepancies
wqp.nam.num <- wqp.usgs.stations[,c('Site_no','MonitoringLocationName')]
exist.nam.num <- unique(stations.all[nchar(stations.all$Site_no) > 5,c('Site_no','Site_name')])
look.for.diff <- merge(wqp.nam.num, exist.nam.num, by = 'Site_no')
look.for.diff[look.for.diff$MonitoringLocationName != look.for.diff$Site_name,]
look.for.diff.name <- merge(wqp.nam.num, exist.nam.num, by.y = 'Site_name', by.x = 'MonitoringLocationName')
look.for.diff.name[look.for.diff.name$Site_no.x != look.for.diff.name$Site_no.y,]
#conclusion is that we will need to decide which name to use or if we would rather move forward with numbers over names

#stations.all without parameter for station matching
stations.all.info <- unique(stations.all[,c('Site_no','Site_name','Agency','Str_RM','LLID_Stream_Lake','Str_LLID','Str_name','LAKE_LLID','LAKE_NAME')])

#look for stations that we already have str rm for
exist.usgs.stations <- merge(wqp.usgs.stations, stations.all.info, by = 'Site_no')

#check to make sure there aren't any NA's from the stations.all df
#any(is.na(exist.usgs.stations$Str_RM))

#There are 56 stations that we used in 2012 that have inconsistent RMs assigned. One assigned from DO and one assigned from Toxics processing.
View(exist.usgs.stations[exist.usgs.stations$Site_no %in% exist.usgs.stations[duplicated(exist.usgs.stations$Site_no),'Site_no'],])

#this issue is more widespread between locating methods used for existing 2012 toxics and DO and affects both USGS and lasar stations
sites2012 <- merge(DO.final[,c('Site_no','Str_RM')],unique(stations.coded.use[,c('Site_no','Str_RM')]),by='Site_no')
sites2012$diff <- as.numeric(sites2012$Str_RM.x) - as.numeric(sites2012$Str_RM.y)
View(arrange(sites2012[sites2012$Str_RM.x != sites2012$Str_RM.y,],diff))

#we will locate all usgs stations irrespective of new or old. to compile this we want all stations from first 2012 attempt with agency usgs
usgs.stations.all.info <- stations.all.info[stations.all.info$Agency == 'USGS',]
#due to inconsistencies in RM assignment we can't keep this field since it creates duplicates in the merges
usgs.stations.all.info <- within(usgs.stations.all.info, rm(Str_RM))
#This takes care of resolving those duplicates from the Str_RM
usgs.stations.all.info <- unique(usgs.stations.all.info)
usgs.stations <- merge(wqp.usgs.stations, usgs.stations.all.info, by = 'Site_no', all = TRUE)
usgs.stations$MonitoringLocationIdentifier <- paste('USGS-',usgs.stations$Site_no,sep='')
#There are still inconsistencies in the names of the stations so this is a brute force way to pick one.
usgs.stations <- usgs.stations[!duplicated(usgs.stations$MonitoringLocationIdentifier),]

#In tyring to resolve pulling in lat lon. There were duplicate stations ids creeping in the usgs.staions df. THis is because
#I used unique usgs.stations.all.info across all rows and the LLID assigned from 2012 Toxics was different than 2012 DO. The 2012
#DO assignment appears to be incorrect. This affects at least station 14153500. Definitely drives home that we should be looking
#at the LLID assignments we did for all of the usgs stations in 2012 (and maybe the lasar ones too?)
#View(usgs.stations[usgs.stations$MonitoringLocationIdentifier %in% usgs.stations[duplicated(usgs.stations$MonitoringLocationIdentifier),'MonitoringLocationIdentifier'],])
#Based on this it looks like it is only that one station that had different LLIDs. The others seemed to differ in station name.

need.lat.lon <- usgs.stations[is.na(usgs.stations$LatitudeMeasure),]

need.lat.lon.encoded <- URLencode(paste(need.lat.lon$MonitoringLocationIdentifier,collapse=';'), reserved = TRUE)

theStationURL <- paste('http://www.waterqualitydata.us/Station/search?',
                       'siteid=', need.lat.lon.encoded,
                       '&mimeType=csv', sep ='')
tmp.stations <- getURL(theStationURL)
need.lat.lon.stations <- read.csv(textConnection(tmp.stations), stringsAsFactors = FALSE)

need.lat.lon.stations.sub <- need.lat.lon.stations[,c('MonitoringLocationIdentifier','LatitudeMeasure','LongitudeMeasure','HorizontalCoordinateReferenceSystemDatumName')]
usgs.stations.full <- merge(usgs.stations, need.lat.lon.stations.sub, by = 'MonitoringLocationIdentifier', all.x = TRUE)
usgs.stations.full$LatitudeMeasure.x <- ifelse(is.na(usgs.stations.full$LatitudeMeasure.x),usgs.stations.full$LatitudeMeasure.y, usgs.stations.full$LatitudeMeasure.x)
usgs.stations.full$LongitudeMeasure.x <- ifelse(is.na(usgs.stations.full$LongitudeMeasure.x),usgs.stations.full$LongitudeMeasure.y, usgs.stations.full$LongitudeMeasure.x)
usgs.stations.full$HorizontalCoordinateReferenceSystemDatumName.x <- ifelse(is.na(usgs.stations.full$HorizontalCoordinateReferenceSystemDatumName.x),
                                                                            usgs.stations.full$HorizontalCoordinateReferenceSystemDatumName.y, 
                                                                            usgs.stations.full$HorizontalCoordinateReferenceSystemDatumName.x)

#There are two stations that we used in 2012 that aren't in the WQP or NWIS because
#we received data from the USGS Oregon Water Science Center directly. The two sites
#are 450955122291200 MILK CREEK AT CAMP ADAMS and 451607122423700 MOLALLA RIVER AT KNIGHTS BRIDGE, CANBY, OR
#The latitude and longitude for these will come from the raw info we receieved from them.
will.usgs.2012 <- read.dbf('//deqhq1/wqassessment/2012_WQAssessment/WQ_2012_Assessment_Raw/USGS_Junedata/willamette_sites.dbf',as.is=TRUE)
usgs.stations.full[usgs.stations.full$MonitoringLocationIdentifier %in% 
                     c('USGS-450955122291200', 'USGS-451607122423700'),
                   c('LatitudeMeasure.x','LongitudeMeasure.x','HorizontalCoordinateReferenceSystemDatumName.x')] <- will.usgs.2012[will.usgs.2012$site_no %in% 
                                                                                                                                     c('450955122291200', '451607122423700'),
                                                                                                                                   c('dec_lat_va','dec_long_v','coord_datu') ]

usgs.stations.full <- within(usgs.stations.full, rm(LatitudeMeasure.y,LongitudeMeasure.y,HorizontalCoordinateReferenceSystemDatumName.y))
usgs.stations.full <- rename(usgs.stations.full, c('LatitudeMeasure.x' = 'LatitudeMeasure', 
                                                   'LongitudeMeasure.x' = 'LongitudeMeasure',
                                                   'HorizontalCoordinateReferenceSystemDatumName.x' = 'HorizontalCoordinateReferenceSystemDatumName'))

usgs.stations.full$Latitude_clean <- as.character(as.numeric(usgs.stations.full$LatitudeMeasure))
usgs.stations.full$Longitude_clean <- as.character(as.numeric(usgs.stations.full$LongitudeMeasure))

rm(list = setdiff(ls(), c('usgs.stations.full')))

library(plyr)
library(RODBC)

#### EPA ####

#From Greg Coffeen. List of LASAR station numbers for each CEMAP Station name.
cemap.lookup <- read.csv('CEMAP_Info_from_Lab/OR_CEMAP_FinalStationList99-06.csv')

#Build the site_only field
wqp.stations$site_only <- gsub('EMAP_CS_WQX-','',wqp.stations$MonitoringLocationIdentifier)
wqp.stations$site_only <- gsub('EMAP_CS-','',wqp.stations$site_only)

#Creates CEMAP specific df with lasar numbers tacked on to the WQP data fields
cemap.lookup.wqp <- (merge(wqp.stations, cemap.lookup, by.x = 'site_only', by.y = 'StationID'))

#Useless check?
#View(wqp.stations[wqp.stations$site_only %in% cemap.lookup.wqp$site_only,])

#Looks to see if 'Environmental Monitoring and Assessment Program' organization is associated with anything other than CEMAP
#wqp.stations[!wqp.stations$site_only %in% cemap.lookup.wqp$site_only & 
#               wqp.stations$OrganizationFormalName == 'Environmental Monitoring and Assessment Program',]
#It's not.

#Specifies sets of organizations from EPA.
epa.orgs.not.superfund <- c("Environmental Monitoring and Assessment Program","EPA National Aquatic Resource Survey Data","EPA National Aquatic Resources Survey", "EPA National Aquatic Resources Survey (TEST Submission)")
epa.orgs.not.EMAP <- c("EPA National Aquatic Resource Survey Data","EPA National Aquatic Resources Survey", "EPA National Aquatic Resources Survey (TEST Submission)")

#Checks to see which parameters we are even looking at for these NARS/EMAP data sets. Mainly nitrates, phosphorus, zinc and selenium.
table(wqp.data[wqp.data$OrganizationFormalName %in% epa.orgs.not.EMAP,'MonitoringLocationIdentifier'],
wqp.data[wqp.data$OrganizationFormalName %in% epa.orgs.not.EMAP,'CharacteristicName'])

#Useless check?
#View(wqp.stations[wqp.stations$OrganizationFormalName %in% epa.orgs.not.EMAP,])

#All sites with the organization 'EPA National Aquatic Resource Survey Data' still need to be dealt with. Either left out, associated with
#an existing lasar station or finding their actual lat lon via some other means.
#Checking with Shannon Hubler to see if any of the EPA National Aquatic Resource Survey Data is already in LASAR and if 
#not, if he has better coordinates to get the stations located. -->It's not in LASAR (at least the chemistry isn't). 
#write.csv(wqp.stations[wqp.stations$OrganizationFormalName == 'EPA National Aquatic Resource Survey Data',],'NARS_List_for_SH.csv')
#Pointed to access database Shannon 'maintains'. This takes care of 139 of 175 stations that had truncated lat/lon. Either
#need to leave out the reaminging 36, locate them as is or try and track down better coordinates (as of 5/28/14). Will leave them out as of 5/29/14.
nars.con <- odbcConnectAccess('//deqlead01/biomon/databases/Biomon_Phoenix.mdb')
nars.ref <- sqlFetch(nars.con, 'STATION')
wqp.stations$site_only <- gsub('NARS-','',wqp.stations$site_only)
wqp.stations$site_only <- gsub('NARS_WQX-','',wqp.stations$site_only)
remaining <- wqp.stations[wqp.stations$site_only %in% wqp.stations[wqp.stations$OrganizationFormalName == 'EPA National Aquatic Resource Survey Data','site_only'][!wqp.stations[wqp.stations$OrganizationFormalName == 'EPA National Aquatic Resource Survey Data','site_only'] %in% nars.ref$Site_ID],]
#write.csv(remaining, 'StationsToLocate/NARS_List_for_SH_Remaining.csv', row.names = FALSE)
wqp.stations$LatChar <- as.character((as.numeric(wqp.stations$LatitudeMeasure)))
wqp.stations$LonChar <- as.character((as.numeric(wqp.stations$LongitudeMeasure)))
wqp.stations.nars <- merge(wqp.stations, nars.ref[,c('Site_ID','LONG','LAT')], by.x = 'site_only', by.y = 'Site_ID', all.x = TRUE)
wqp.stations.nars$LatChar2 <- ifelse(is.na(wqp.stations.nars$LAT),wqp.stations.nars$LatChar,wqp.stations.nars$LAT)
wqp.stations.nars$LonChar2 <- ifelse(is.na(wqp.stations.nars$LONG),wqp.stations.nars$LonChar,wqp.stations.nars$LONG)
wqp.stations.nars$LatChar <- wqp.stations.nars$LatChar2
wqp.stations.nars$LonChar <- wqp.stations.nars$LonChar2
wqp.stations.nars <- within(wqp.stations.nars, rm(LatChar2, LonChar2, LAT, LONG))
#We decided not to dig too far for these stations locations
wqp.stations.nars <- wqp.stations.nars[!wqp.stations.nars$MonitoringLocationIdentifier %in% remaining$MonitoringLocationIdentifier,]
wqp.stations.post.qc <- wqp.stations
wqp.stations <- wqp.stations.nars

#checks to make sure that the following cemap.data merge got the right number of rows
nrow(wqp.data[wqp.data$OrganizationFormalName == 'Environmental Monitoring and Assessment Program',])

#Assign temporary unique id to every row in the complete dataset
wqp.data$Temp.id<-paste("tempID", seq(1:nrow(wqp.data)), sep="_")

#looks at cemap data only to check against lasar data to see if we even need to keep this stuff from the wqp
cemap.data <- merge(wqp.data, cemap.lookup.wqp, by = 'MonitoringLocationIdentifier')

#Initial exploration processing steps
# #makes a code to relate
# cemap.data$code <- paste(cemap.data$Lasar_Number, cemap.data$ActivityStartDate, cemap.data$CharacteristicName)
# lasar$code <- paste(lasar$STATION_KEY, lasar$SAMPLE_DATE, lasar$NAME)
# 
# #chekcs to see how many we have
# any(cemap.data$code %in% lasar$code)
# 
# #names don't match
# unique(cemap.data$CharacteristicName)
# 
# #see which parameters have overlap
# unique(lasar[lasar$STATION_KEY %in% cemap.data$Lasar_Number,'NAME'])
# 
# #see if any phosphorus data in lasar data set.....(it's not)
# unique(lasar[lasar$STATION_KEY %in% cemap.data[cemap.data$CharacteristicName == 'Phosphorus','Lasar_Number'],'NAME'])

#need to put criteria.name back into wqp data in order to relate it to lasar so they have a common name
wqp.name.match <- read.csv('WQPNameMatch_05142014.csv',stringsAsFactors=FALSE)
cemap.data$criteria.name <- mapvalues(cemap.data$CharacteristicName, from = wqp.name.match$WQP.Name, to = wqp.name.match$Criteria.Name)

#now we can build the code again
cemap.data$code <- paste(cemap.data$Lasar_Number, cemap.data$ActivityStartDate, cemap.data$criteria.name)
lasar$code <- paste(lasar$STATION_KEY, lasar$SAMPLE_DATE, lasar$criteria.name)

#look to see if all cemap data is already in lasar....it's not
#all(cemap.data$code %in% lasar$code)

#trying to get a handle on what's in lasar and what's not
#nrow(cemap.data[cemap.data$code %in% lasar$code,])
#View(cemap.data[!cemap.data$code %in% lasar$code,])

#checks to see if all of the cemap stations that are already in lasar have data that is not in lasar.....they do
# unique(cemap.data[cemap.data$code %in% lasar$code,'MonitoringLocationIdentifier']) %in% unique(cemap.data[!cemap.data$code %in% lasar$code,'MonitoringLocationIdentifier'])

#checks to see which stations have some data in lasar and some data not in lasar
# unique(cemap.data[!cemap.data$code %in% lasar$code,'MonitoringLocationIdentifier']) %in% unique(cemap.data[cemap.data$code %in% lasar$code,'MonitoringLocationIdentifier'])

#hmm.maybe that last one isn't so useful. it really as easy as
# any(is.na((cemap.data[,c('Lasar_Number')])))
#Every CEMAP station has a lasar number. Next step will be to see if all of those lasar numbers have LLID.

#it looks like there is overlap of some nitrate data but it seems most cemap stations have nitrate, phosphorus and ph. 
#so even when there is no ph or phosphorus in lasar there may be in wqp. 
#wqp.stations[grep('^OR[0-9]',wqp.stations$site_only),]
#We'll only want to remove data from wqp.data that is definitely in the lasar dataset.
wqp.data.og <- wqp.data
wqp.data <- wqp.data[!wqp.data$Temp.id %in% cemap.data[cemap.data$code %in% lasar$code,'Temp.id'],]

#we also want to remove data associated with stations we aren't using
wqp.data <- wqp.data[!wqp.data$MonitoringLocationIdentifier %in% remaining$MonitoringLocationIdentifier,]

#compile EPA station list for Mike to locate
not.usgs <- wqp.stations[-grep('USGS',wqp.stations$MonitoringLocationIdentifier),]
not.usgs.not.cemap <- not.usgs[!not.usgs$OrganizationFormalName %in% c('Environmental Monitoring and Assessment Program'),]
not.usgs <- not.usgs.not.cemap
#write.csv(not.usgs, 'StationsToLocate/EPAandNPS_Stations_to_locate_05292014.csv', row.names = FALSE)

#### LASAR ####
#check lasar stations against geodatabase 2010
lasar.stations.in.gdb <- lasar.stations[(lasar.stations$STATION_KEY %in% stations2010$STATION),]
lasar.stations.not.in.gdb <- lasar.stations[!(lasar.stations$STATION_KEY %in% stations2010$STATION),]

#sites2012 is from 09_2012 IR Station Table Creation.R
#lasar.stations.not.in.gdb[lasar.stations.not.in.gdb$STATION_KEY %in% sites2012$Site_no,]
#There's only one. So remember to add this in later.
#stations.all.info[stations.all.info$Site_no == 12026,]

#lasar stations to locate then
lasar.stations.to.locate <- lasar.stations.not.in.gdb[lasar.stations.not.in.gdb$STATION_KEY != 12026,]

#This didn't return lat lon for a handful of sites
# lasar.lat.lon <- data.frame(Site_no = character(),latitude=character(),longitude=character())
# for (i in 1:nrow(lasar.stations.to.locate)) {
#   http <- paste("http://deq12.deq.state.or.us/lasar2/StationDetail.aspx?StationID=",lasar.stations.to.locate$STATION_KEY[i], sep='')
#   arm <- readLines(http)
#   latVal <- substr(grep('lblLatitude',arm,value=TRUE),53,59)
#   longVal <- substr(grep('lblLongitude',arm,value=TRUE),55,62)
#   row <- cbind(Site_no=lasar.stations.to.locate$STATION_KEY[i],latitude=latVal,longitude=longVal)
#   print(row)
#   ifelse(ncol(row) == 3, lasar.lat.lon <- rbind(lasar.lat.lon, row), next)
# }

#check against station use list from 2010
con.2010 <- odbcConnectAccess('//deqhq1/wqassessment/2010_WQAssessment/Databases/WorkingTables_2010.mdb')
sul2010 <- sqlFetch(con.2010, 'StationUseList_2010')

#let's see if they were to be included in the 2010 iteration
check <- (merge(lasar.stations.to.locate, sul2010, by.x = 'STATION_KEY', by.y = 'STATION'))

#These are the stations we don't have lat/lon for in LASARWeb
#View(sul2010[sul2010$STATION %in% lasar.stations.to.locate[!lasar.stations.to.locate$STATION_KEY %in% lasar.lat.lon$Site_no,'STATION_KEY'],]) #nrow=13

#There are 10 stations in the geodatabase that we are proposing to use this year that are marked as do not use for 2010
#View(sul2010[sul2010$STATION %in% lasar.stations.in.gdb$STATION_KEY & sul2010$USE == 0,]) #nrow=10

#There are 90 stations we need to locate that are marked as do not use for 2010
#View(sul2010[sul2010$STATION %in% lasar.stations.not.in.gdb$STATION_KEY & sul2010$USE == 0,]) #nrow=90
#stationNOTingdb.insul2010 <- sul2010[sul2010$STATION %in% lasar.stations.not.in.gdb$STATION_KEY & sul2010$USE == 0,]

#None of the stations we need to locate are marked as usable from 2010
#nrow(sul2010[sul2010$STATION %in% lasar.stations.not.in.gdb$STATION_KEY & sul2010$USE == 1,]) #nrow= 0

#nrow(sul2010[sul2010$STATION %in% lasar.stations$STATION_KEY,])

#going with the 2010 determinations on usability
lasar.stations <- lasar.stations[!lasar.stations$STATION_KEY %in% sul2010[sul2010$USE == 0,'STATION'],]

lasar.stations.in.gdb <- lasar.stations[(lasar.stations$STATION_KEY %in% stations2010$STATION),]
lasar.stations.not.in.gdb <- lasar.stations[!(lasar.stations$STATION_KEY %in% stations2010$STATION),]

#lasar stations to locate then
lasar.stations.to.locate <- lasar.stations.not.in.gdb[lasar.stations.not.in.gdb$STATION_KEY != 12026,]

#Exclude effluent or other non-representative stations
lasar.stations.to.locate <- lasar.stations.to.locate[-grep('fflue',lasar.stations.to.locate$LOCATION_DESCRIPTION),]
#lasar.stations.to.locate <- lasar.stations.to.locate[-grep('utfa',lasar.stations.to.locate$LOCATION_DESCRIPTION),] #These will require more review
#lasar.stations.to.locate <- lasar.stations.to.locate[-grep('andfil',lasar.stations.to.locate$LOCATION_DESCRIPTION),]
#Landfill stations to exclude - There are three we are keeping because they say SW which I think means Surface Water sites. We'll see where they land.
lasar.stations.to.locate <- lasar.stations.to.locate[!lasar.stations.to.locate$STATION_KEY %in% c(15574,32043,32044,36415,36416,36417),]
#Not sure if these are representative locations or not. Will map for now but keep in mind for review.
#lasar.stations.to.locate <- lasar.stations.to.locate[-grep('[Bb]oat [Bb]asin',lasar.stations.to.locate$LOCATION_DESCRIPTION),]

#This didn't return lat lon for a handful of sites
# lasar.lat.lon <- data.frame(Site_no = character(),latitude=character(),longitude=character())
# for (i in 1:nrow(lasar.stations.to.locate)) {
#   http <- paste("http://deq12.deq.state.or.us/lasar2/StationDetail.aspx?StationID=",lasar.stations.to.locate$STATION_KEY[i], sep='')
#   arm <- readLines(http)
#   latVal <- substr(grep('lblLatitude',arm,value=TRUE),53,59)
#   longVal <- substr(grep('lblLongitude',arm,value=TRUE),55,62)
#   row <- cbind(Site_no=lasar.stations.to.locate$STATION_KEY[i],latitude=latVal,longitude=longVal)
#   print(row)
#   ifelse(ncol(row) == 3, lasar.lat.lon <- rbind(lasar.lat.lon, row), next)
# }

#lasar.stations.to.locate.w.ll <- merge(lasar.stations.to.locate, lasar.lat.lon, by.x = 'STATION_KEY', by.y = 'Site_no', all.x = TRUE)

#we can really only move forward with stations that we have lat lon for 
#lasar.stations.to.locate.w.ll <- lasar.stations.to.locate.w.ll[!is.na(lasar.stations.to.locate.w.ll$latitude),]

#we need datum which isn't in lasarWeb.
library(foreign)
lasar.check <- read.dbf('//deqlead03/gis_wa/project_working_folders/lasar_stations/lasar_stations/LASAR_Stations_26sept13.dbf')

#sub lasar.check with just those stations we want to locate
lstl.ll <- (lasar.check[lasar.check$STATION_KE %in% lasar.stations.to.locate$STATION_KEY,])

#write.csv(lstl.ll, 'StationsToLocate/LASAR_Stations_to_locate_05292014.csv', row.names = FALSE)
#test <- read.csv('StationsToLocate/LASAR_Stations_to_locate.csv', stringsAsFactors=FALSE)

#desired fields post locate for analysis
c('Site_no','Site_name','Str_name','Str_LLID','Str_RM','LAKE_LLID','LAKE_NAME')

#fields should match stations2010 for further creation of stations2012

#Make sure to limit lasar data for only those stations we are moving forward with
lasar.og <- lasar
lasar <- lasar[lasar$STATION_KEY %in% c(lasar.stations.in.gdb$STATION_KEY,lstl.ll$STATION_KE),]

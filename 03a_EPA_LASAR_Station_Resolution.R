library(plyr)

cemap.lookup <- read.csv('OR_CEMAP_FinalStationList99-06.csv')

wqp.stations$site_only <- gsub('EMAP_CS_WQX-','',wqp.stations$MonitoringLocationIdentifier)
wqp.stations$site_only <- gsub('EMAP_CS-','',wqp.stations$site_only)

cemap.lookup.wqp <- (merge(wqp.stations, cemap.lookup, by.x = 'site_only', by.y = 'StationID'))


View(wqp.stations[wqp.stations$site_only %in% cemap.lookup.wqp$site_only,])

wqp.stations[!wqp.stations$site_only %in% cemap.lookup.wqp$site_only & 
               wqp.stations$OrganizationFormalName == 'Environmental Monitoring and Assessment Program',]

epa.orgs.not.superfund <- c("Environmental Monitoring and Assessment Program","EPA National Aquatic Resource Survey Data","EPA National Aquatic Resources Survey", "EPA National Aquatic Resources Survey (TEST Submission)")
epa.orgs.not.EMAP <- c("EPA National Aquatic Resource Survey Data","EPA National Aquatic Resources Survey", "EPA National Aquatic Resources Survey (TEST Submission)")

table(wqp.data[wqp.data$OrganizationFormalName %in% epa.orgs.not.EMAP,'MonitoringLocationIdentifier'],
wqp.data[wqp.data$OrganizationFormalName %in% epa.orgs.not.EMAP,'CharacteristicName'])

View(wqp.stations[wqp.stations$OrganizationFormalName %in% epa.orgs.not.EMAP,])

#All sites with the organization 'EPA National Aquatic Resource Survey Data' still need to be dealt with. Either left out, associated with
#an existing lasar station or finding their actual lat lon via some other means.

nrow(wqp.data[wqp.data$OrganizationFormalName == 'Environmental Monitoring and Assessment Program',])

cemap.data <- merge(wqp.data, cemap.lookup.wqp, by = 'MonitoringLocationIdentifier')

cemap.data$code <- paste(cemap.data$Lasar_Number, cemap.data$ActivityStartDate, cemap.data$CharacteristicName)
lasar$code <- paste(lasar$STATION_KEY, lasar$SAMPLE_DATE, lasar$NAME)

any(cemap.data$code %in% lasar$code)

#names don't match
unique(cemap.data$CharacteristicName)

#see which parameters have overlap
unique(lasar[lasar$STATION_KEY %in% cemap.data$Lasar_Number,'NAME'])

#see if any phosphorus data in lasar data set.....(it's not)
unique(lasar[lasar$STATION_KEY %in% cemap.data[cemap.data$CharacteristicName == 'Phosphorus','Lasar_Number'],'NAME'])

wqp.name.match <- read.csv('WQPNameMatch_05142014.csv',stringsAsFactors=FALSE)

cemap.data$criteria.name <- mapvalues(cemap.data$CharacteristicName, from = wqp.name.match$WQP.Name, to = wqp.name.match$Criteria.Name)

cemap.data$code <- paste(cemap.data$Lasar_Number, cemap.data$ActivityStartDate, cemap.data$criteria.name)
lasar$code <- paste(lasar$STATION_KEY, lasar$SAMPLE_DATE, lasar$criteria.name)

all(cemap.data$code %in% lasar$code)

nrow(cemap.data[cemap.data$code %in% lasar$code,])
View(cemap.data[!cemap.data$code %in% lasar$code,])

cemap.data[cemap.data$code %in% lasar$code,'MonitoringLocationIdentifier'] %in% cemap.data[!cemap.data$code %in% lasar$code,'MonitoringLocationIdentifier']

#it looks like there is overlap of some nitrate data but it seems most cemap stations have nitrate, phosphorus and ph. 
#so even when there is no ph or phosphorus in lasar there may be in wqp. 
wqp.stations[grep('^OR[0-9]',wqp.stations$site_only),]

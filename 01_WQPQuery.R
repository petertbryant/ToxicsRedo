#### Using the water quality portal REST service for Characteristic names ####
library(RCurl)
library(XML)
library(RODBC)

source('//deqhq1/wqassessment/2012_wqassessment/toxicsredo/wqpquery_functions.R')

con <- odbcConnect('WQAssessment')

#### Define state to query ####
#you can use the wqp.domain.get if you want outside Oregon but here is the Oregon code
Oregon <- 'US%3A41'

#### Define site types to query ####
wqp.siteTypes <- WQP.domain.get('Sitetype')

#Using the wqp.siteTypes enter the values you want to query for in siteType.
#Separate each value with the URL encoded semi-colon '%3B'. For values with commas use the URL encoded value '%2C+'
siteType = 'Estuary%3BOcean%3BStream%3BStream%3BLake%2C+Reservoir%2C+Impoundment'

#### Define sample media to query ####
wqp.sampleMedia <- WQP.domain.get('Samplemedia')

#Separate each value you want to query with the URL encoded semi-colon '%3B'.
sampleMedia <- 'Water'

#### Define characteristics to query ####
#First get the list of Characteristic names from the WQP. These names are consistent with EPA's SRS. 
wqp.characteristics <- WQP.domain.get('Characteristicname')

## Compare to Table 30 names ##
#Pull in the compiled criteria table used for the Toxics Monitoring prgram
source('//deqlead01/wqm/TOXICS_2012/Data/R/criteria.R')

#Select only those parameters that have a Table 30 or Table 40 criterion.
deq.pollutants <- criteria.values.melted.applicable[criteria.values.melted.applicable$variable %in% 
                                                      c('Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism',
                                                        'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only',
                                                        'Table 30 Toxic Substances - Freshwater Acute',
                                                        'Table 30 Toxic Substances - Freshwater Chronic',
                                                        'Table 30 Toxic Substances - Saltwater Acute',
                                                        'Table 30 Toxic Substances - Saltwater Chronic'),]

#look for matching names in the Water Quality Portal
matched <- deq.pollutants[deq.pollutants$Pollutant %in% wqp.characteristics$value,]

#Identify the parameters we need to resolve for naming issues.
not.matched <- deq.pollutants[!deq.pollutants$Pollutant %in% wqp.characteristics$value,]

#output DEQ table pollutants that we do not have a match for
#write.csv(unique(not.matched$Pollutant),'//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/WQPNameMatch.csv')

#once the matches have been identified
to.match <- read.csv('//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/WQPNameMatch.csv', stringsAsFactors = FALSE)

#for PCBs the standard is a total of all aroclors and congeners and so is a many to one. No comparison is done to individual compounds.
#given that I'll list out all the aroclor and pcb names to include in the wqp query after taking out the blank match.
#to.match[to.match$Criteria.Name != 'Polychlorinated Biphenyls (PCBs)',]
aroclors <- wqp.characteristics[grep('[Aa]roclor',wqp.characteristics$value),'value']
pcbs <- wqp.characteristics[grep('[Pp][Cc][Bb]',wqp.characteristics$value),'value']

#then we put the characterisitic names together
to.query <- c(to.match$WQP.Name, 
              aroclors, 
              pcbs, 
              matched$Pollutant, 
              c('pH','Temperature, water','Temperature','Hardness, Ca, Mg','Hardness, Ca, Mg as CaCO3',
                'Hardness, Calcium','Hardness, carbonate','Hardness, carbonate as CaCO3','Hardness, magnesium',
                'Total Hardness','Calcium','Magnesium','Calcium as CaCO3','Magnesium as CaCO3','Ammonia', 'Ammonia as N'))
to.query <- to.query[!to.query %in% c('Dinitrophenol','','Nitrosamine')]
to.query <- unique(to.query)

#now put you characteristics in the characteristicName variable for use in the query
#characteristicName <- 'pH'
characteristicName1 <- URLencode.PTB(paste(to.query[1:210],collapse=';'),reserved = TRUE)
characteristicName2 <- URLencode.PTB(paste(to.query[211:length(to.query)],collapse=';'),reserved = TRUE)

#### Define start and end date ####
#The expected format is mm-dd-yyyy
startDate <- '01-01-2000'
endDate <- '12-31-2011'

#### Pass the query using the arguments you defined above ####
#Note that you can also pass different geographical scales but that is not currently built into this
#Please refer to the linked page from the function definition for other available search parameters
success.table <- data.frame('Characteristic' = to.query, 
                            'Success' = rep('Not Run',length(to.query)), 
                            'stations' = rep(0, length(to.query)),
                            'samples' = rep(0,length(to.query)),stringsAsFactors = FALSE)

for (i in length(to.query)) {
  tmp.stations <- wqp.station.query(stateCode = Oregon, 
                                    siteType = siteType, 
                                    sampleMedia = sampleMedia, 
                                    characteristicName = to.query[i], 
                                    startDate = startDate, 
                                    endDate = endDate)
  
  if (names(tmp.stations)[1] != 'OrganizationIdentifier') {
    print('The server is not accepting queries at this time. Please wait a bit to resume.')
    print(paste('You can resume with the iterator', i, 'which is parameter', to.query[i]))
    break
  }
  
  if(is.character(tmp.stations)) {
    print(paste('Awww shucks. Parameter',to.query[i],'has no data'))
    success.table[i,'Success'] <- 'Unsuccessful'
  } else {
    tmp.data <- wqp.data.query(stateCode = Oregon, 
                               siteType = siteType, 
                               sampleMedia = sampleMedia, 
                               characteristicName = to.query[i], 
                               startDate = startDate, 
                               endDate = endDate)
    
    if (names(tmp.data)[1] != 'OrganizationIdentifier') {
      print('The server is not accepting queries at this time. Please wait a bit to resume.')
      print(paste('You can resume with the iterator', i, 'which is parameter', to.query[i]))
      break
    }
    
    if(nrow(tmp.data) > 0) {
      if(i == 1) {
        wqp.stations <- tmp.stations
        sqlSave(con, tmp.data, 'WQPData_05022014', varTypes = WQPvarTypes)
      } else {
        wqp.stations <- rbind(wqp.stations, tmp.stations)
        tmp.data$USGSPCode <- as.character(tmp.data$USGSPCode)
        tmp.data$DetectionQuantitationLimitMeasure.MeasureValue <- as.numeric(tmp.data$DetectionQuantitationLimitMeasure.MeasureValue)
        sqlSave(con, tmp.data, 'WQPData_05022014', append = TRUE, varTypes = WQPvarTypes)
      }
      print(paste('Success for Parameter:',to.query[i]))
      
      success.table[i,'Success'] <- 'Success!'

      
      print(paste('There are', nrow(tmp.stations), 'stations with', nrow(tmp.data), 'samples for',to.query[i]))
      
      print(paste(length(to.query)-i,'parameters left to query')) 
      
    } else {
      print(paste('No data available for', to.query[i]))
      success.table[i,'Success'] <- 'No data returned'
    }
    
    success.table[i,'samples'] <- nrow(tmp.data)
    success.table[i,'stations'] <- nrow(tmp.stations)
   
    rm(tmp.data)
    gc()
  }
  
  wqp.stations$x <- apply(wqp.stations[,names(wqp.stations)],1,paste,collapse=',')
  wqp.stations <- wqp.stations[!duplicated(wqp.stations$x),]
  wqp.stations <- within(wqp.stations, rm(x))
}


wqp.stations$x <- apply(wqp.stations[,c('MonitoringLocationIdentifier', 'VerticalMeasure.MeasureValue')],1,paste, collapse = ',')
wqp.stations.dups.removed <- wqp.stations[!duplicated(wqp.stations$x),]
wqp.stations.dups.removed <- within(wqp.stations.dups.removed, rm(x))

sqlSave(con, wqp.stations.dups.removed, 'WQPStations_05022014')
sqlSave(con, success.table, 'WQPQuery_Status')


wqp.data <- sqlFetch(con, 'WQPData_05022014')
wqp.data[wqp.data$CharacteristicName == '']
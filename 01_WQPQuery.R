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
#The last character vector here includes parameters that were missed in the initial parameter identification
#as well as parameters necessary to calculate sample specific criteria.
to.query <- c(to.match$WQP.Name, 
              aroclors, 
              pcbs, 
              matched$Pollutant, 
              c('pH','Temperature, water','Temperature','Hardness, Ca, Mg','Hardness, Ca, Mg as CaCO3',
                'Hardness, Calcium','Hardness, carbonate','Hardness, carbonate as CaCO3','Hardness, magnesium',
                'Total Hardness','Calcium','Magnesium','Calcium as CaCO3','Magnesium as CaCO3','Ammonia', 'Ammonia as N',
                'Chlordane, technical, and/or chlordane metabolites','Oxychlordane','cis-Nonachlor','trans-Nonachlor',
                'Nonachlor','trans-Chlordane','cis-Chlordane','Chlordane, technical'))
to.query <- to.query[!to.query %in% c('Dinitrophenol','','Nitrosamine')]
to.query <- unique(to.query)

#somehow chlordane isn't getting into the to.query vector now when it originally did at index 204
#this code inserts it back into that specific index
#to.query.start <- to.query[1:203]
#to.query.end <- to.query[204:length(to.query)]
#to.query <- c(to.query.start, 'Chlordane', to.query.end)

#### Define start and end date ####
#The expected format is mm-dd-yyyy
startDate <- '01-01-2000'
endDate <- '12-31-2011'

#### Pass the query using the arguments you defined above ####
#Note that you can also pass different geographical scales but that is not currently built into this
#Please refer to the linked page from the function definition for other available search parameters

#This code was run the first time to set up the tracking table
#success.table <- data.frame('Characteristic' = to.query, 
#                            'Success' = rep('Not Run',length(to.query)), 
#                            'stations' = rep(0, length(to.query)),
#                            'samples' = rep(0,length(to.query)),stringsAsFactors = FALSE)

#This next block pulls in the tracking table to see what's been done up to this point
success.table <- sqlFetch(con, 'WQPQuery_Status')

#Need to add in Ammonia and Ammonia as N to the success table. This parameter has only ph and temperature dependent criteria and so was 
#missed when the criteria value column was coerced to numeric.
success.table <- rbind(success.table, data.frame('Characteristic' = c('Ammonia','Ammonia as N','Chlordane, technical, and/or chlordane metabolites','Oxychlordane','cis-Nonachlor','trans-Nonachlor',
                                                                      'Nonachlor','trans-Chlordane','cis-Chlordane','Chlordane, technical'), 
                                                 'Success' = rep('Not run',10), 
                                                 'stations' = rep(0,10), 
                                                 'samples' = rep(0,10), stringsAsFactors = FALSE))

#we have to pull this back from the databse to add to it now that we have parameters to add.
wqp.stations <- sqlFetch(con, 'WQPStations_05022014')
wqp.stations <- within(wqp.stations, rm(x))

for (i in 259:length(to.query)) {
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
        names(tmp.stations) <- gsub('\\.','',names(tmp.stations))
        wqp.stations <- rbind(wqp.stations, tmp.stations)
        tmp.data$USGSPCode <- as.character(tmp.data$USGSPCode)
        tmp.data$DetectionQuantitationLimitMeasure.MeasureValue <- as.numeric(tmp.data$DetectionQuantitationLimitMeasure.MeasureValue)
        sqlSave(con, tmp.data, 'WQPData_05022014', append = TRUE, varTypes = WQPvarTypes, rownames = FALSE)
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


wqp.stations$x <- apply(wqp.stations[,c('MonitoringLocationIdentifier', 'VerticalMeasureMeasureValue')],1,paste, collapse = ',')
wqp.stations.dups.removed <- wqp.stations[!duplicated(wqp.stations$x),]
wqp.stations.dups.removed <- within(wqp.stations.dups.removed, rm(x))

#sqlSave(con, wqp.stations.dups.removed, 'WQPStations_05022014', rownames = FALSE)
#sqlSave(con, success.table, 'WQPQuery_Status', rownames = FALSE)


wqp.data <- sqlFetch(con, 'WQPData_05022014')
wqp.data[wqp.data$CharacteristicName == '']
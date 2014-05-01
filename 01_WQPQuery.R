#### Using the water quality portal REST service for Characteristic names ####
library(RCurl)
library(XML)

source('//deqhq1/wqassessment/2012_wqassessment/toxicsredo/wqpquery_functions.R')

#### Define state to query ####
#you can use the wqp.domain.get if you want outside Oregon but here is the Oregon code
Oregon <- 'US%3A41'

#### Define site types to query ####
wqp.siteTypes <- WQP.domain.get('siteType')

#Using the wqp.siteTypes enter the values you want to query for in siteType.
#Separate each value with the URL encoded semi-colon '%3B'. For values with commas use the URL encoded value '%2C+'
siteType = 'Stream%3BLake%2C+Reservoir%2C+Impoundment'

#### Define sample media to query ####
wqp.sampleMedia <- WQP.domain.get('samplemedia')

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
to.match[to.match$Criteria.Name != 'Polychlorinated Biphenyls (PCBs)',]
aroclors <- wqp.characteristics[grep('[Aa]roclor',wqp.characteristics$value),'value']
pcbs <- wqp.characteristics[grep('[Pp][Cc][Bb]',wqp.characteristics$value),'value']

#then we put the characterisitic names together
to.query <- c(to.match$WQP.Name, 
              aroclors, 
              pcbs, 
              matched$Pollutant, 
              c('pH','Temperature, water','Temperature','Hardness, Ca, Mg','Hardness, Ca, Mg as CaCO3',
                'Hardness, Calcium','Hardness, carbonate','Hardness, carbonate as CaCO3','Hardness, magnesium',
                'Total Hardness','Calcium','Magnesium','Calcium as CaCO3','Magnesium as CaCO3'))
to.query <- to.query[!to.query %in% c('Dinitrophenol','','Nitrosamine')]

#now put you characteristics in the characteristicName variable for use in the query
#characteristicName <- 'pH'
characteristicName1 <- URLencode.PTB(paste(to.query[1:210],collapse=';'),reserved = TRUE)
characteristicName2 <- URLencode.PTB(paste(to.query[211:length(to.query)],collapse=';'),reserved = TRUE)
characteristicsNames <- c(characteristicName1, characteristicName2)

#### Define start and end date ####
#The expected format is mm-dd-yyyy
startDate <- '01-01-2000'
endDate <- '12-31-2011'

#### Pass the query using the arguments you defined above ####
#Note that you can also pass different geographical scales but that is not currently built into this
#Please refer to the linked page from the function definition for other available search parameters
for (i in 1:length(characteristicNames)) {
  tmp.stations <- wqp.station.query(Oregon, siteType, sampleMedia, characteristicsNames[i], startDate, endDate)
  tmp.data <- wqp.data.query(Oregon, siteType, sampleMedia, characteristicsNames[i], startDate, endDate)
  
  if (nrow(tmp.stations) > 0) {
    if(i == 1){
      wqp.stations <- tmp.stations
      wqp.data <- tmp.data
    } else {
      wqp.stations <- rbind(wqp.stations, tmp.stations)
      wqp.data <- rbind(wqp.data, tmp.data)
    }
  } 
}


# for (i in 281:length(to.query)) {
#   theStationURL <- paste('http://www.waterqualitydata.us/Station/search?',
#                          'statecode=', Oregon,
#                          '&siteType=', siteType, 
#                          '&sampleMedia=', sampleMedia,
#                         # '&characteristicName=', characteristicName,
#                            '&characteristicName=', URLencode.PTB(to.query[i]),
#                          '&startDateLo=', startDate,
#                          '&startDateHi=', endDate,
#                          '&mimeType=csv', sep ='')
#   tmp.stations <- getURL(theStationURL)
#   
#   ifelse(substr(tmp.stations,1,12) == 'Organization', 
#          print(paste('Success for parameter:',to.query[i], i, 'with', (length(to.query)-i), 'remaining to test.')), 
#          print(paste('!!!!!!!Unsuccessful for parameter:',to.query[i],'!!!!!!!!')))
# 
# #   if (nrow(wqp.stations) > 0) {
# #     ifelse(i ==1 , wqp.stations.set <- wqp.stations, wqp.stations.set <- rbind(wqp.stations.set, wqp.stations))
# #   } else {
# #     print(paste('Parameter', to.query[i], 'needs help'))
# #   }
# }
# 

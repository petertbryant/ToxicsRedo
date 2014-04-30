#### Using the water quality portal REST service for Characteristic names ####
library(RCurl)
library(XML)

#### Define domain value retrieval function ####
WQP.domain.get <- function(value) {
  theURL <- paste('http://www.waterqualitydata.us/Codes/', value, '?mimeType=xml', sep = '')
  tmp <- getURL(theURL)
  tmp.top <- xmlRoot(xmlTreeParse(tmp))
  tmp.df <- data.frame(value = rep("",length(tmp.top)),desc = rep("",length(tmp.top)),providers = rep("",length(tmp.top)), stringsAsFactors = FALSE)
  for (i in 1:length(tmp.top)) {
    tmp.df$value[i] <- xmlGetAttr(tmp.top[[i]],'value')
    tmp.df$desc[i] <- xmlGetAttr(tmp.top[[i]],'desc')
    tmp.df$providers[i] <- xmlGetAttr(tmp.top[[i]],'providers')  
  }
  return(tmp.df)
}

URLencode.PTB <- function (URL, reserved = FALSE) 
{
  OK <- "[^-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvywxyz0-9']"
  x <- strsplit(URL, "")[[1L]]
  z <- grep(OK, x)
  if (length(z)) {
    y <- sapply(x[z], function(x) {if(x == ' ') {
      '+'
    } else if (x == '('){
      '('
    } else if (x == ')'){
      ')'
    } else {
      paste0("%", as.character(charToRaw(x)), collapse = "")
    }
    })
   x[z] <- y
  }
  paste(x, collapse = "")
}

#This function takes a string as the value argument and retrieves domains in XML format and converts them to dataframes in R
#for the domains for the available search terms go to http://www.waterqualitydata.us/webservices_documentation.jsp#Domain
#As of 4/8/2014 the available parameter names include: countrycode, statecode, countycode, Sitetype, Organization, Samplemedia,
#Characteristictype, Characteristicname, providers

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

#trying a the Levenshtein distance algorithm to find matches
#string.compare <- adist(deq.pollutants$Pollutant, wqp.characteristics$value)
#
#matches <- data.frame(wqp = character(), deq = character(), stringsAsFactors = FALSE)
#for (i in 1:nrow(string.compare)) {
#  matches[i,'deq'] <- deq.pollutants[i,'Pollutant']
#  matches[i,'wqp'] <- wqp.characteristics[which.min(string.compare[i,]),'value']
#}

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

#### Define start and end date ####
#The expected format is mm-dd-yyyy
startDate <- '01-01-2000'
endDate <- '12-31-2011'

#### Compile the query using the arguments you defined above ####
#Note that you can also pass different geographical scales but that is not currently built into this
#Please refer to the linked page from the function definition for other available search parameters

#The query is completed in two parts: One for the data and one for the station information
theStationURL <- paste('http://www.waterqualitydata.us/Station/search?',
                     'statecode=', Oregon,
                     '&siteType=', siteType, 
                     '&sampleMedia=', sampleMedia,
                    '&characteristicName=', characteristicName,
                   # '&characteristicName=', URLencode.PTB(paste(c(to.query[to.query == 'Bis(chloromethyl) ether'],to.query[grep('1,3-Dichloropropene',to.query)]), collapse = ';')),
                     '&startDateLo=', startDate,
                     '&startDateHi=', endDate,
                     '&mimeType=csv', sep ='')

theDataURL <- paste('http://www.waterqualitydata.us/Result/search?',
                       'statecode=', Oregon,
                       '&siteType=', siteType, 
                       '&sampleMedia=', sampleMedia,
                       '&characteristicName=', characteristicName,
                       '&startDateLo=', startDate,
                       '&startDateHi=', endDate,
                       '&mimeType=csv', sep ='')

#### Pass the query to WQP ####
#This query is for the stations
tmp.stations <- getURL(theStationURL)
wqp.stations <- read.csv(textConnection(tmp.stations), stringsAsFactors = FALSE)

#This query if for the data
tmp.data <- getURL(theDataURL)
wqp.data <- read.csv(textConnection(tmp.data), stringsAsFactors = FALSE)


for (i in 281:length(to.query)) {
  theStationURL <- paste('http://www.waterqualitydata.us/Station/search?',
                         'statecode=', Oregon,
                         '&siteType=', siteType, 
                         '&sampleMedia=', sampleMedia,
                        # '&characteristicName=', characteristicName,
                           '&characteristicName=', URLencode.PTB(to.query[i]),
                         '&startDateLo=', startDate,
                         '&startDateHi=', endDate,
                         '&mimeType=csv', sep ='')
  tmp.stations <- getURL(theStationURL)
  
  ifelse(substr(tmp.stations,1,12) == 'Organization', 
         print(paste('Success for parameter:',to.query[i], i, 'with', (length(to.query)-i), 'remaining to test.')), 
         print(paste('!!!!!!!Unsuccessful for parameter:',to.query[i],'!!!!!!!!')))

#   if (nrow(wqp.stations) > 0) {
#     ifelse(i ==1 , wqp.stations.set <- wqp.stations, wqp.stations.set <- rbind(wqp.stations.set, wqp.stations))
#   } else {
#     print(paste('Parameter', to.query[i], 'needs help'))
#   }
}


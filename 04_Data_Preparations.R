library(plyr)
library(reshape2)
library(stringr)
library(RODBC)
library(foreign)

options(stringsAsFactors = FALSE, scipen = 100)

#### This file is to bring the two datasets lasar and wqp.data into a single dataframe for moving forward with 
#bring the two data sets into one now, lasar and wqp.data to feed into Toxics Analysis
con <- odbcConnect('WQAssessment')
wqp.data <- sqlFetch(con, 'WQPData_woLASAROverlap_06132014')
wqp.stations <- sqlFetch(con, 'WQPStations_wUpdatedLatLon_06132014')
lasar <- sqlFetch(con, 'LASAR_Toxics_Query_wcriterianame_wAddOns_06232014')
odbcCloseAll()
#names(wqp.data)
#....we've got some work to do
lasar$Result_clean <- as.numeric(lasar$Result_clean)
consal.lasar <- read.csv('Estuary_Analysis/LASAR_consal.csv')
consal.lasar$criterianame <- consal.lasar$NAME
consal.lasar[consal.lasar$NAME == 'Conductivity','Result_clean'] <- consal.lasar[consal.lasar$NAME == 'Conductivity','Result_clean']/1000
#This equation comes from http://pubs.usgs.gov/tm/2006/tm1D3/pdf/TM1D3.pdf page 36 
#The report says this equation is used to convert specific conductance to salinity
consal.lasar[consal.lasar$NAME == 'Conductivity','R'] <- consal.lasar[consal.lasar$NAME == 'Conductivity','Result_clean']/53.087
consal.lasar[consal.lasar$NAME == 'Conductivity','Result_clean'] <- (0.0120 + (-0.2174*(consal.lasar[consal.lasar$NAME == 'Conductivity','R']^(1/2))) + (25.3283*consal.lasar[consal.lasar$NAME == 'Conductivity','R']) + (13.7714*(consal.lasar[consal.lasar$NAME == 'Conductivity','R']^(3/2))) + 
                    (-6.4788*(consal.lasar[consal.lasar$NAME == 'Conductivity','R']^2)) + (2.5842*(consal.lasar[consal.lasar$NAME == 'Conductivity','R']^(5/2))))
consal.lasar[consal.lasar$NAME == 'Conductivity','UNIT'] <- 'ppth'
consal.lasar[consal.lasar$NAME == 'Conductivity','criterianame'] <- 'Salinity'
consal.lasar[consal.lasar$NAME == 'Conductivity','NAME'] <- 'Salinity'
consal.lasar.max <- ddply(consal.lasar, .(STATION_KEY,SAMPLE_DATE,SAMPLE_TIME,NAME,LOCATION_DESCRIPTION,UNIT,criterianame,SAMPLE_MATRIX), summarise, Result_clean = max(Result_clean))
consal.lasar.max <- cbind(consal.lasar.max, data.frame('PARAMETER_KEY' = rep(5229,nrow(consal.lasar.max)),
                                                       'Result' = consal.lasar.max$Result_clean,
                                                       'ABBREVIATION' = rep('Field',nrow(consal.lasar.max)),
                                                       'Namefull' = rep('FieldSalinity',nrow(consal.lasar.max)),
                                                       'METHOD_DETECTION_LIMIT' = rep(0,nrow(consal.lasar.max)),
                                                       'METHOD_REPORTING_LIMIT' = rep(0,nrow(consal.lasar.max)),
                                                       'QA_QC_TYPE' = rep('Sample',nrow(consal.lasar.max)),
                                                       'STATUS' = rep('A',nrow(consal.lasar.max)),
                                                       'SUBPROJECT_NAME' = rep(NA,nrow(consal.lasar.max))))
consal.lasar.max$SAMPLE_TIME <- substr(consal.lasar.max$SAMPLE_TIME,12,19)
#join back up with lasar
lasar <- rbind(lasar, consal.lasar.max)
rm(consal.lasar, consal.lasar.max)

#Pull in the compiled criteria table used for the Toxics Monitoring prgram
source('//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/TMP-RCode/criteria.R')

#Names Names Names!!! We want to associate 
wqp.name.match <- read.csv('WQPNameMatch_05142014.csv',stringsAsFactors=FALSE)
deq.pollutants <- criteria.values.melted.applicable[criteria.values.melted.applicable$variable %in% 
                                                      c('Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism',
                                                        'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only',
                                                        'Table 30 Toxic Substances - Freshwater Acute',
                                                        'Table 30 Toxic Substances - Freshwater Chronic',
                                                        'Table 30 Toxic Substances - Saltwater Acute',
                                                        'Table 30 Toxic Substances - Saltwater Chronic'),]
Table3040.applicable <- merge(deq.pollutants, wqp.name.match[,c('Criteria.Name','DEQ.Table.name')], by.x = 'Pollutant', by.y = 'Criteria.Name')
Table3040.applicable <- rename(Table3040.applicable, c('Pollutant' = 'Criteria.Name.full', 'DEQ.Table.name' = 'criterianame'))
T3040.ES <- Table3040.applicable
T3040.ES$Matrix <- 'ES'
T3040.ES$ID <- paste(T3040.ES$criterianame, T3040.ES$Matrix)
T3040.ES$x <- apply(T3040.ES[,names(T3040.ES)],1,paste,collapse=',')
T3040.ES <- T3040.ES[!duplicated(T3040.ES$x),]
T3040.ES <- within(T3040.ES, rm(x))
wo.to.keep <- T3040.ES[T3040.ES$criterianame %in% c('Barium','Chlorophenoxy Herbicide (2,4,5,-TP)','2,4-D','Copper','Methoxychlor','Nitrates') & 
                         T3040.ES$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism',]
T3040.ES <- T3040.ES[T3040.ES$variable != 'Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism',]
T3040.ES <- rbind(T3040.ES, wo.to.keep)
Table3040.applicable <- rbind(Table3040.applicable, T3040.ES)

#Pull in criteria determination calculation functions
source('//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/TMP-RCode/hardness_eval_functions_Element_Names.R')

#View(arrange(criteria.values.melted.applicable.nonnum[grepl(', Total',criteria.values.melted.applicable.nonnum$Pollutant) & 
#                                                        criteria.values.melted.applicable.nonnum$variable %in% deq.pollutants$variable,],Pollutant))
#View(arrange(criteria.values.melted.applicable.nonnum[grepl('issolv',criteria.values.melted.applicable.nonnum$Pollutant) & 
#                                                        criteria.values.melted.applicable.nonnum$variable %in% deq.pollutants$variable,],Pollutant))

#### First the Water Quality Portal data ####
#In case we need to pull the data in again
#con <- odbcConnect('WQAssessment')
#wqp.data <- sqlFetch(con, 'WQPData_05022014')

#Remove Bed Sediment and Suspended samples
wqp.data <- wqp.data[!wqp.data$ResultSampleFractionText %in% c('Bed Sediment','Suspended'),]

#Remove samples labeled as Interstitial
wqp.data <- wqp.data[wqp.data$ActivityMediaSubdivisionName != 'Interstitial',]

#Make a date-time column
wqp.data$Sampled <- paste(wqp.data$ActivityStartDate, wqp.data$ActivityStartTimeTime)

#Make a column with criteria name based on earlier mapping 
wqp.data <- merge(wqp.data, unique(wqp.name.match[,c('WQP.Name','DEQ.Table.name')]), by.x = 'CharacteristicName', by.y = 'WQP.Name', all.x = TRUE)
wqp.data <- rename(wqp.data, c('DEQ.Table.name' = 'criterianame'))
wqp.data$criterianame <- ifelse(is.na(wqp.data$criterianame),wqp.data$CharacteristicName,wqp.data$criterianame)
#wqp.data$criterianame2 <- mapvalues(wqp.data$CharacteristicName, from = wqp.name.match$WQP.Name, to = wqp.name.match$DEQ.Table.name)

#table(wqp.data[wqp.data$ResultSampleFractionText %in% c('Dissolved','Recoverable','Total'),'CharacteristicName'],wqp.data[wqp.data$ResultSampleFractionText %in% c('Dissolved','Recoverable','Total'),'ResultSampleFractionText'])

# wqp.name.check <- merge(wqp.data[,c('CharacteristicName','ResultSampleFractionText')], wqp.name.match, by.x = 'CharacteristicName', by.y = 'WQP.Name', all.x = TRUE)
# wqp.name.check$x <- apply(wqp.name.check[,names(wqp.name.check)],1,paste,collapse=',')
# wqp.name.check <- wqp.name.check[!duplicated(wqp.name.check$x),]
# wqp.name.check <- within(wqp.name.check, rm(x))

#Pull in site_only so when we build summary info later it will fit that formatting better
wqp.data <- merge(wqp.data, wqp.stations[,c('MonitoringLocationIdentifier','site_only')], by = 'MonitoringLocationIdentifier', all.x = TRUE)
wqp.data$site_only <- gsub('USGS-|11NPSWRD-|NARSTEST-|R10PORTLANDHARBOR-','',wqp.data$site_only)

#Not sure what I was doing with this one
#wqp.data$adid <- paste(wqp.data$CharacteristicName, wqp.data$ActivityStartDate)

#Let's pull out only those columns we need to make this business work
wqp.data.sub <- wqp.data[,c('site_only','OrganizationFormalName',
                            'Sampled', 'CharacteristicName','ResultSampleFractionText',
                            'ResultMeasureValue','ResultMeasureMeasureUnitCode','MeasureQualifierCode',
                            'ActivityTypeCode','DetectionQuantitationLimitMeasureMeasureValue','DetectionQuantitationLimitMeasureMeasureUnitCode',
                            'MonitoringLocationName', 'criterianame','ResultAnalyticalMethodMethodIdentifier')]

#Now we make the names match the script I built for the Toxics Monitroing Program
wqp.data.sub <- rename(wqp.data.sub, c('site_only' = 'SampleRegID',
                                       'OrganizationFormalName' = 'Agency',
                                       'CharacteristicName' = 'Name',
                                       'ResultSampleFractionText' = 'Fraction',
                                       'ResultMeasureValue' = 'tResult',
                                       'ResultMeasureMeasureUnitCode' = 'Unit',
                                       'MeasureQualifierCode' = 'Status',
                                       'ActivityTypeCode' = 'SampleType',
                                       'DetectionQuantitationLimitMeasureMeasureValue' = 'tMRL',
                                       'DetectionQuantitationLimitMeasureMeasureUnitCode' = 'tMRLUnit',
                                       'MonitoringLocationName' = 'SampleAlias',
                                       'ResultAnalyticalMethodMethodIdentifier' = 'SpecificMethod'))

# str(data.wo.void)
# 'data.frame':  163574 obs. of  15 variables:
#   $ Analyte         : chr  "1-Methylphenanthrene" "1-Methylphenanthrene" "1-Methylphenanthrene" "1-Methylphenanthrene" ...
# $ Project         : chr  "Mid Coast" "South Coast" "Owyhee" "North Coast" ...
# $ SampleRegID     : int  37399 28303 10730 13654 37399 10990 20434 12962 11047 34309 ...
# $ SampleAlias     : chr  "Umpqua River at Discovery Center Docks" "Elk Creek at ODFW Hatchery" "Owyhee River at Rome (Hwy.95)" "Necanicum R at 12th Street approach" ...
# $ Sampled         : chr  "11/19/2013 9:45" "10/28/2013 13:00" "9/23/2013 9:25" "12/2/2013 14:25" ...
# $ SampleType      : chr  "Grab Sample" "Grab Sample" "Grab Sample" "Grab Sample" ...
# $ Matrix          : chr  "Estuary" "River/Stream" "River/Stream" "Estuary" ...
# $ tResult         : num  0 0 0 0 0 0 0 0 0 0 ...
# $ tMRL            : num  5.93 5.89 5.93 5.37 5.36 5.86 6.15 5.42 5.95 5.91 ...
# $ Unit            : chr  "ng/L" "ng/L" "ng/L" "ng/L" ...
# $ SpecificMethod  : chr  "EPA 8270D" "EPA 8270D" "EPA 8270D" "EPA 8270D" ...
# $ Status          : chr  "A" "A" "B" "B" ...
# $ chem.group      : chr  "Combustion By-Products" "Combustion By-Products" "Combustion By-Products" "Combustion By-Products" ...
# $ Detect.nondetect: num  0 0 0 0 0 0 0 0 0 0 ...
# $ code            : chr  "1-Methylphenanthrene 37399 11/19/2013 9:45" "1-Methylphenanthrene 28303 10/28/2013 13:00" "1-Methylphenanthrene 10730 9/23/2013 9:25" "1-Methylphenanthrene 13654 12/2/2013 14:25" ...


#### Now the lasar data ####
#Preserve lasar query
#lasar.ng <- lasar

#let's rebuild the criterianame
lasar.names.match <- read.csv('lasar_names_match.csv',stringsAsFactors = FALSE)
lasar.names.match <- rename(lasar.names.match, c('Pollutant' = 'Criteria.Name.full'))
lasar <- within(lasar, rm(criterianame))
lasar <- merge(lasar, lasar.names.match, by.x = 'NAME', by.y = 'lasar.name', all.x = TRUE)
lasar$x <- apply(lasar[,names(lasar)],1,paste,collapse=',')
lasar <- lasar[!duplicated(lasar$x),]
lasar <- within(lasar, rm(x))
lasar <- merge(lasar, unique(Table3040.applicable[,c('Criteria.Name.full','criterianame')]), by = 'Criteria.Name.full', all.x = TRUE)
#lasar <- rename(lasar, c('DEQ.Table.name' = 'criterianame'))
lasar$criterianame <- ifelse(is.na(lasar$criterianame),lasar$NAME,lasar$criterianame)

#Make recoverable lower case to be consistent 
lasar$ABBREVIATION <- gsub('R','r',lasar$ABBREVIATION)

#Not sure if this field gets used but here it is
#lasar$test <- ifelse(lasar$ABBREVIATION %in% c('Dissolved', 'Total recoverable'),paste(lasar$NAME, ", ", lasar$ABBREVIATION, sep = ''),lasar$NAME)

#Pull the lasar data frame in new if we need to 
#lasar <- sqlFetch(con, 'LASAR_Toxics_Query_06112014')

#Make a date-time field
lasar$Sampled <- paste(as.character(lasar$SAMPLE_DATE), as.character(lasar$SAMPLE_TIME)) #i had this in here because the lasar date and time were coming in as posix but today they come in as character so i'm taking it out: substr(as.character(lasar$SAMPLE_TIME),12,19)

#see how much diff there is with method_detection_limit and method_reporting_limit
#View(lasar[which(lasar$METHOD_DETECTION_LIMIT != lasar$METHOD_REPORTING_LIMIT),])
#we're going with METHOD_REPORTING_LIMIT

#again not sure why this is here
#lasar$adid <- paste(lasar$NAME, lasar$Sampled)

#found some hanging bad QA QC Types from lasar data set
lasar <- lasar[!lasar$QA_QC_TYPE %in% c('Equipment Blank - Field', 
                                        'Matrix Spike - Field', 
                                        'Matrix Spike Duplicate - Field', 
                                        'Transfer Blank'),]

#Make the lasar names match the script and be consistent with the new wqp names
#lasar <- rename(lasar, c('Pollutant' = 'CriteriaTableName'))
lasar.new.names <- rename(lasar, c('NAME' = 'Name',
                                   'ABBREVIATION' = 'Fraction',
                                   #'criteria.name' = 'Analyte', 
                                   'STATION_KEY' = 'SampleRegID',
                                   'LOCATION_DESCRIPTION' = 'SampleAlias',
                                   'QA_QC_TYPE' = 'SampleType',
                                   'Result_clean' = 'tResult',
                                   'METHOD_REPORTING_LIMIT' = 'tMRL',
                                   'UNIT' = 'Unit',
                                   #'METHOD' = 'SpecificMethod',
                                   'STATUS' = 'Status'))
lasar.new.names$Agency <- 'ODEQ'
lasar.new.names$tMRLUnit <- lasar.new.names$Unit

#I had to pull method out from the lasar dataset since I didn't know where to get accurate method information related
#to the method_key in the parameter_result table. Until i find out where to get that info I'll populate a column with NAs
lasar.new.names$SpecificMethod <- NA

#This subsets the lasasr dataframe to only have the columns to be used for aggregation
lasar.new.names <- lasar.new.names[,names(wqp.data.sub)]

#### Pulling wqp.data and lasar together now! ####
data.complete <- rbind(lasar.new.names, wqp.data.sub)

#### Cleaning up the complete dataset and making some fields for consistent future processing ####
#should have a numeric result and mrl fields
data.complete$tResult <- as.numeric(data.complete$tResult)
data.complete[which(data.complete$tMRL == '0.0105 Est'),'tMRL'] <- 0.0105
data.complete[which(data.complete$tMRL == '0.0030 Est'),'tMRL'] <- 0.0030
data.complete[which(data.complete$tMRL == '0.0035 Est'),'tMRL'] <- 0.0035
data.complete[which(data.complete$tMRL == '0.02                                 0.02'),'tMRL'] <- 0.02
data.complete[which(data.complete$tMRL == '0.10                                                                                                                                                                        0.10'),'tMRL'] <- 0.10
data.complete$tMRL <- as.numeric(data.complete$tMRL)

#need to determine detect/non-detect in order to accurately select maximum concentration in case there are more than one method
#reported for a specific sample
data.complete$dnd <- ifelse(is.na(data.complete$tMRL),1,ifelse(data.complete$tResult<=data.complete$tMRL,0,1))

#result should also be in micrograms since all the criteria are as well
#first there are several improperply labeled units as well as some pH ones labeled with None for unit. pH is inlcuded in this analysis
#for the purposes of calculating pentachlorophenol and ammonia criteria 
data.complete$Unit <- str_trim(data.complete$Unit)
data.complete <- data.complete[!data.complete$Unit %in% c('%','mg/Kg wet','ng/SPMD','ueq/L'),]
data.complete[data.complete$Unit %in% c('mg/L','mg/l','mg/l as N'),'tResult'] <- data.complete[data.complete$Unit %in% c('mg/L','mg/l','mg/l as N'),'tResult']*1000
data.complete[data.complete$Unit %in% c('mg/L','mg/l','mg/l as N'),'Unit'] <- 'µg/L'
data.complete[data.complete$Unit %in% c('ng/L', 'ng/l'),'tResult'] <- data.complete[data.complete$Unit %in% c('ng/L', 'ng/l'),'tResult']/1000
data.complete[data.complete$Unit %in% c('ng/L', 'ng/l'),'Unit'] <- 'µg/L'
data.complete[data.complete$Unit %in% c('pg/L', 'pg/l'),'tResult'] <- data.complete[data.complete$Unit %in% c('pg/L', 'pg/l'),'tResult']/1000000
data.complete[data.complete$Unit %in% c('pg/L', 'pg/l'),'Unit'] <- 'µg/L'

data.complete[data.complete$Unit == 'ppb','Unit'] <- 'µg/L'
data.complete[data.complete$Unit == 'ug/l','Unit'] <- 'µg/L'

#for the MRL units too
data.complete$tMRLUnit <- str_trim(data.complete$tMRLUnit)
data.complete <- data.complete[!data.complete$tMRLUnit %in% c('%','mg/Kg wet','ng/SPMD','ueq/L'),]
data.complete[data.complete$tMRLUnit %in% c('mg/L','mg/l','mg/l as N'),'tMRL'] <- data.complete[data.complete$tMRLUnit %in% c('mg/L','mg/l','mg/l as N'),'tMRL']*1000
data.complete[data.complete$tMRLUnit %in% c('mg/L','mg/l','mg/l as N'),'tMRLUnit'] <- 'µg/L'
data.complete[data.complete$tMRLUnit %in% c('ng/L', 'ng/l'),'tMRL'] <- data.complete[data.complete$tMRLUnit %in% c('ng/L', 'ng/l'),'tMRL']/1000
data.complete[data.complete$tMRLUnit %in% c('ng/L', 'ng/l'),'tMRLUnit'] <- 'µg/L'
data.complete[data.complete$tMRLUnit %in% c('pg/L', 'pg/l'),'tMRL'] <- data.complete[data.complete$tMRLUnit %in% c('pg/L', 'pg/l'),'tMRL']/1000000
data.complete[data.complete$tMRLUnit %in% c('pg/L', 'pg/l'),'tMRLUnit'] <- 'µg/L'

data.complete[which(data.complete$tMRLUnit == 'ppb'),'tMRLUnit'] <- 'µg/L'
data.complete[which(data.complete$tMRLUnit == 'ug/l'),'tMRLUnit'] <- 'µg/L'

#making the name.full can happen after we combine data sets
total.to.recoverable <- c('Arsenic','Mercury','Copper','Zinc','Nickel','Lead','Selenium',
                          'Chromium','Iron','Barium','Thallium','Manganese','Silver','Antimony',
                          'Cadmium','Cyanide','Chromium(VI)')
dissolved.metals <- c('Arsenic','Cadmium','Chromium','Chromium(VI)','Lead','Nickel','Selenium','Silver','Zinc')
data.complete[data.complete$Name %in% total.to.recoverable & data.complete$Fraction %in% c('Total', 'Recoverable'),'Fraction'] <- 'Total recoverable'
#Assumes total sample fraction for those samples that remain unstandardized
data.complete[data.complete$Name %in% total.to.recoverable & !grepl('Total recoverable|Dissolved', data.complete$Fraction),'Fraction'] <- 'Total recoverable'
#compile Name.full for metals
data.complete$Name.full <- ifelse(data.complete$Name %in% c(total.to.recoverable, dissolved.metals) & 
                                    data.complete$Fraction %in% c('Dissolved', 'Total recoverable'),
                                  paste(data.complete$Name, ", ", data.complete$Fraction, sep = ''),
                                  data.complete$Name)

#Fields for future grouping
data.complete$id <- paste(data.complete$SampleRegID, data.complete$Name.full, data.complete$Sampled)
data.complete$id <- as.numeric(as.factor(data.complete$id))
data.complete$day <- substr(data.complete$Sampled,1,10)
data.complete$code <- paste(data.complete$SampleRegID, data.complete$Name.full, data.complete$day)
data.complete$code <- as.numeric(as.factor(data.complete$code))
data.complete$index <- rownames(data.complete)

#### Resolve duplicates ####
#resolve field duplicates specified by sample type
fd <- data.complete[data.complete$SampleType %in% c('Sample - Field Duplicate', 'Quality Control Sample-Field Replicate', 
                                                    'Quality Control Field Replicate Msr/Obs', 'Quality Control Sample-Other'),]
fd.fp <- data.complete[data.complete$code %in% fd$code,]
sample.max <- ddply(fd.fp, .(code), summarise, maximum = max(tResult), rows = length(tResult), index = index[which.max(tResult)])
fd.fp.max <- fd.fp[fd.fp$index %in% sample.max$index,]
data.complete.wo.fd.fp.pairs <- data.complete[!data.complete$index %in% fd.fp$index,]
data.complete.w.resolved.fd <- rbind(data.complete.wo.fd.fp.pairs, fd.fp.max)

#resolve duplicates at the same date-time
#because there are some with multiple methods and two MRL levels we can't just pick the max
#value unless it was a detection
# data.complete.wo.dups.index <- ddply(data.complete.w.resolved.fd, .(id), function(sub) {
#                                ifelse(sum(sub$dnd) == 0,sub[which.min(sub$tMRL),'index'],
#                                       ifelse((sum(sub$dnd) >= 2),sub[which.max(sub$tResult),'index'],
#                                              sub[sub$dnd == 1,'index']))})
# 
# # library(dplyr)
# # MRL.decision <- function(result) {
# #   ifelse(result == 0,sub[which.min(sub$tMRL),'index'],
# #          ifelse((result >= 2),sub[which.max(sub$tResult),'index'],
# #                 sub[sub$dnd == 1,'index']))}
# # data.complete.w.resolved.fd.groups <- group_by(data.complete.w.resolved.fd, id)
# # data.complete.w.resolved.fd.index <- summarise(data.complete.w.resolved.fd.groups, MRL.decision(sum(dnd)))
# # data.complete.wo.dups.time <- data.complete.w.resolved.fd[data.complete.w.resolved.fd$index %in% data.complete.w.resolved.fd.index$index,]
# # detach(package:dplyr)
# 
# data.complete.wo.dups.time <- data.complete.w.resolved.fd[data.complete.w.resolved.fd$index %in% data.complete.wo.dups.index$V1,]

#We also have multiple samples within a day. 
#View(data.complete.wo.dups[data.complete.wo.dups$code %in% data.complete.wo.dups[duplicated(data.complete.wo.dups$code),'code'],])
# system.time(data.complete.wo.dups.by.day.index <- ddply(data.complete.w.resolved.fd, .(code), function(sub) {
#   ifelse(sum(sub$dnd) == 0,sub[which.min(sub$tMRL),'index'],
#          ifelse((sum(sub$dnd) >= 2),sub[which.max(sub$tResult),'index'],
#                 sub[sub$dnd == 1,'index']))}))

#data.complete.wo.dups.by.day.index <- pick.min.MRL(data.complete.w.resolved.fd)

# data.complete.wo.dups <- data.complete.w.resolved.fd[data.complete.w.resolved.fd$index %in% data.complete.wo.dups.by.day.index$V1,]

sub <- with(data.complete.w.resolved.fd, resolveMRLs(code, dnd, tResult))
data.complete.wo.dup.MRLs <- data.complete.w.resolved.fd[sub,]
data.complete.wo.dups <- remove.dups(data.complete.wo.dup.MRLs)

#### Grouping parameters to be compared to composite criteria ####
#We will add this here for now but should be coming from Station locate process
# data.complete.wo.dups$Matrix <- 'FW' #mapvalues(data.complete.wo.dups$Matrix, from = c("River/Stream", "Estuary"), to = c('FW','SW'))
sul2012 <- read.dbf('C:/Users/pbryant/Desktop/Stations_2012_Analysis.dbf')
#xsul2012 <- read.dbf('C:/Users/pbryant/Desktop/All_stations_final_est.dbf')
# con.2010 <- odbcConnectAccess('//deqhq1/wqassessment/2010_WQAssessment/Databases/WorkingTables_2010.mdb')
# sul2010 <- sqlFetch(con.2010, 'StationUseList_2010')
# odbcCloseAll()
# sul2010$Matrix <- ifelse(sul2010$MARINE_WATERS == 1,'SW',ifelse(sul2010$ESTUARY == 1,'ES','FW'))
sul2012 <- rename(sul2012, c('MATRIX' = 'Matrix'))
data.complete.w.matrix <- merge(data.complete.wo.dups, sul2012[,c('STATION','Matrix')], by.x = 'SampleRegID', by.y = 'STATION')
# data.complete.w.matrix$Matrix <- ifelse(is.na(data.complete.w.matrix$Matrix.y),data.complete.w.matrix$Matrix.x,data.complete.w.matrix$Matrix.y)
# data.complete.w.matrix <- within(data.complete.w.matrix, rm(Matrix.x, Matrix.y))

#Some of the criteria apply to Totals and not individual degradates. Here we do that totalling so comparisons can be done.
#First, we will make a Total DDT - This Criteria is never the most stringent and will not be included for analysis
# ddt <- data.complete.wo.dups[data.complete.wo.dups$Name %in% c("4,4`-DDD", "4,4`-DDE", "4,4`-DDT", "p,p'-DDD", "p,p'-DDE", "p,p'-DDT"),]
# ddt$tResult <- ddt$tResult*ddt$dnd
# ddt.casted <- dcast(ddt, Agency + SampleRegID + SampleAlias + Matrix +  
#                       Sampled + SpecificMethod + Fraction + day ~ Name, value.var = 'tResult')
# ddt.casted$'Total DDT' <- rowSums(ddt.casted[,c("4,4`-DDD", "4,4`-DDE", "4,4`-DDT","p,p'-DDD", "p,p'-DDE", "p,p'-DDT")],na.rm=TRUE)
# ddt.casted.sub <- within(ddt.casted, rm("4,4`-DDD", "4,4`-DDE", "4,4`-DDT","p,p'-DDD", "p,p'-DDE", "p,p'-DDT"))
# ddt.melted <- melt(ddt.casted.sub, 
#                    id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SpecificMethod',  
#                                'Fraction','day'),
#                    variable.name = 'Name',
#                    value.name = 'tResult')
# ddt.melted$dnd <- ifelse(ddt.melted$tResult > 0,1,0)
# ddt.melted$Name.full <- ddt.melted$Name
# ddt.melted.addons <- data.frame('tMRL' = rep(0,nrow(ddt.melted)),
#                                 'tMRLUnit' = rep('µg/L',nrow(ddt.melted)),
#                                 'Unit' = rep('µg/L',nrow(ddt.melted)), 
#                                 'Status' = rep('A',nrow(ddt.melted)))
# ddt.melted <- cbind(ddt.melted, ddt.melted.addons)
# ddt.melted$id <- paste(ddt.melted$SampleRegID, ddt.melted$Name.full, ddt.melted$Sampled)
# ddt.melted$day <- substr(ddt.melted$Sampled,1,10)
# ddt.melted$code <- paste(ddt.melted$SampleRegID, ddt.melted$Name.full, ddt.melted$day)
# ddt.melted$index <- as.character(max(as.numeric(data.complete.wo.dups$index)) + as.numeric(rownames(ddt.melted)))
# ddt.melted$criterianame <- 'Total DDT'
# ddt.melted$SampleType <- 'Sample'
# dcwd.ddt <- rbind(data.complete.wo.dups, ddt.melted)

#Now Total Endosulfan
endo <- data.complete.w.matrix[data.complete.w.matrix$Name %in% c("Endosulfan I", "Endosulfan II", "Endosulfan sulfate", ".alpha.-Endosulfan", ".beta.-Endosulfan"),]
endo$tResult <- endo$tResult*endo$dnd
endo.casted <- dcast(endo, Agency + SampleRegID + SampleAlias + Matrix + Fraction +
                       Sampled +  SpecificMethod ~ Name, value.var = 'tResult')
endo.casted$Endosulfan <- rowSums(endo.casted[,c("Endosulfan I", "Endosulfan II", "Endosulfan sulfate",".alpha.-Endosulfan", ".beta.-Endosulfan")],na.rm=TRUE)
endo.casted.sub <- within(endo.casted, rm("Endosulfan I", "Endosulfan II", "Endosulfan sulfate",".alpha.-Endosulfan", ".beta.-Endosulfan"))
endo.melted <- melt(endo.casted.sub, id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SpecificMethod','Fraction'),variable.name = 'Name',value.name = 'tResult')#melt
endo.melted$dnd <- ifelse(endo.melted$tResult > 0,1,0)
endo.melted.addons <- data.frame('tMRL' = rep(0,nrow(endo.melted)), 
                                 'tMRLUnit' = rep('µg/L',nrow(endo.melted)),
                                 'Unit' = rep('µg/L',nrow(endo.melted)), 
                                 'Status' = rep('A',nrow(endo.melted)))
endo.melted <- cbind(endo.melted, endo.melted.addons)
endo.melted$Name.full <- endo.melted$Name
endo.melted$id <- paste(endo.melted$SampleRegID, endo.melted$Name.full, endo.melted$Sampled)
endo.melted$day <- substr(endo.melted$Sampled,1,10)
endo.melted$code <- paste(endo.melted$SampleRegID, endo.melted$Name.full, endo.melted$day)
endo.melted$index <- as.character(max(as.numeric(data.complete.w.matrix$index)) + as.numeric(rownames(endo.melted)))
endo.melted$criterianame <- 'Endosulfan'
endo.melted$SampleType <- 'Sample'
dcwd.endo <- rbind(data.complete.w.matrix, endo.melted)

#Now Total Chlordane
chlordane <- dcwd.endo[dcwd.endo$Name %in% c("Oxychlordane", "alpha-Chlordane", "cis-Chlordane", 'trans-Chlordane',"gamma-Chlordane+trans-Nonachlor", "trans-Nonachlor", "cis-Nonachlor"),]
chlordane$tResult <- chlordane$tResult*chlordane$dnd
chlordane.casted <- dcast(chlordane, Agency + SampleRegID + SampleAlias + Matrix +
                            Sampled + SpecificMethod +Fraction ~ Name, value.var = 'tResult')
chlordane.casted$Chlordane <- rowSums(chlordane.casted[,c("Oxychlordane", "cis-Chlordane", 'trans-Chlordane',"trans-Nonachlor", "cis-Nonachlor")],na.rm=TRUE)
chlordane.casted.sub <- within(chlordane.casted, rm("Oxychlordane", "cis-Chlordane", 'trans-Chlordane',"trans-Nonachlor", "cis-Nonachlor"))
chlordane.melted <- melt(chlordane.casted.sub, id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SpecificMethod','Fraction'),variable.name = 'Name',value.name = 'tResult')#melt
chlordane.melted$dnd <- ifelse(chlordane.melted$tResult > 0,1,0)
chlordane.melted.addons <- data.frame('tMRL' = rep(0,nrow(chlordane.melted)), 
                                      'tMRLUnit' = rep('µg/L',nrow(chlordane.melted)),
                                      'Unit' = rep('µg/L',nrow(chlordane.melted)), 
                                      'Status' = rep('A',nrow(chlordane.melted)))
chlordane.melted <- cbind(chlordane.melted, chlordane.melted.addons)
chlordane.melted$Name.full <- chlordane.melted$Name
chlordane.melted$id <- paste(chlordane.melted$SampleRegID, chlordane.melted$Name.full, chlordane.melted$Sampled)
chlordane.melted$day <- substr(chlordane.melted$Sampled,1,10)
chlordane.melted$code <- paste(chlordane.melted$SampleRegID, chlordane.melted$Name.full, chlordane.melted$day)
chlordane.melted$index <- as.character(max(as.numeric(dcwd.endo$index)) + as.numeric(rownames(chlordane.melted)))
chlordane.melted$criterianame <- 'Chlordane'
chlordane.melted$SampleType <- 'Sample'
dcwd.endo.chlord <- rbind(dcwd.endo, chlordane.melted)

#Now total PCBs
#First the congeners
pcb <- dcwd.endo.chlord[grep('PCB',dcwd.endo.chlord$Name),]
pcb$tResult <- pcb$tResult*pcb$dnd
pcb.casted <- dcast(pcb, Agency + SampleRegID + SampleAlias + Matrix + 
                      Sampled + SpecificMethod + Fraction ~ Name, value.var = 'tResult')
pcb.casted$'Polychlorinated Biphenyls (PCBs)' <- rowSums(pcb.casted[,unique(dcwd.endo.chlord[grep('PCB',dcwd.endo.chlord$Name),'Name'])],na.rm=TRUE)
pcb.casted.sub <- pcb.casted[,!names(pcb.casted) %in% unique(dcwd.endo.chlord[grep('PCB',dcwd.endo.chlord$Name),'Name'])]
pcb.melted <- melt(pcb.casted.sub, id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SpecificMethod','Fraction'),variable.name = 'Name',value.name = 'tResult')#melt
pcb.melted$dnd <- ifelse(pcb.melted$tResult > 0,1,0)
pcb.melted.addons <- data.frame('tMRL' = rep(0,nrow(pcb.melted)), 
                                'tMRLUnit' = rep('µg/L',nrow(pcb.melted)),
                                'Unit' = rep('µg/L',nrow(pcb.melted)), 
                                'Status' = rep('A',nrow(pcb.melted)))
pcb.melted <- cbind(pcb.melted, pcb.melted.addons)
pcb.melted$Name.full <- pcb.melted$Name
pcb.melted$id <- paste(pcb.melted$SampleRegID, pcb.melted$Name.full, pcb.melted$Sampled)
pcb.melted$day <- substr(pcb.melted$Sampled,1,10)
pcb.melted$code <- paste(pcb.melted$SampleRegID, pcb.melted$Name.full, pcb.melted$day)
pcb.melted$index <- as.character(max(as.numeric(dcwd.endo.chlord$index)) + as.numeric(rownames(pcb.melted)))
pcb.melted$criterianame <- 'Polychlorinated Biphenyls (PCBs)'
pcb.melted$SampleType <- 'Sample'
dcwd.endo.chlord.pcb <- rbind(dcwd.endo.chlord, pcb.melted)

#Now the Aroclors
aroclor <- dcwd.endo.chlord.pcb[grep('roclor',dcwd.endo.chlord.pcb$Name),]
aroclor$tResult <- aroclor$tResult*aroclor$dnd
aroclor.casted <- dcast(aroclor, Agency + SampleRegID + SampleAlias + Matrix + 
                      Sampled + SpecificMethod + Fraction ~ Name, value.var = 'tResult')
aroclor.casted$'Polychlorinated Biphenyls (PCBs)' <- rowSums(aroclor.casted[,unique(dcwd.endo.chlord.pcb[grep('roclor',dcwd.endo.chlord.pcb$Name),'Name'])],na.rm=TRUE)
aroclor.casted.sub <- aroclor.casted[,!names(aroclor.casted) %in% unique(dcwd.endo.chlord.pcb[grep('roclor',dcwd.endo.chlord.pcb$Name),'Name'])]
aroclor.melted <- melt(aroclor.casted.sub, id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SpecificMethod','Fraction'),variable.name = 'Name',value.name = 'tResult')#melt
aroclor.melted$dnd <- ifelse(aroclor.melted$tResult > 0,1,0)
aroclor.melted.addons <- data.frame('tMRL' = rep(0,nrow(aroclor.melted)), 
                                'tMRLUnit' = rep('µg/L',nrow(aroclor.melted)),
                                'Unit' = rep('µg/L',nrow(aroclor.melted)), 
                                'Status' = rep('A',nrow(aroclor.melted)))
aroclor.melted <- cbind(aroclor.melted, aroclor.melted.addons)
aroclor.melted$Name.full <- aroclor.melted$Name
aroclor.melted$id <- paste(aroclor.melted$SampleRegID, aroclor.melted$Name.full, aroclor.melted$Sampled)
aroclor.melted$day <- substr(aroclor.melted$Sampled,1,10)
aroclor.melted$code <- paste(aroclor.melted$SampleRegID, aroclor.melted$Name.full, aroclor.melted$day)
aroclor.melted$index <- as.character(max(as.numeric(dcwd.endo.chlord.pcb$index)) + as.numeric(rownames(aroclor.melted)))
aroclor.melted$criterianame <- 'Polychlorinated Biphenyls (PCBs)'
aroclor.melted$SampleType <- 'Sample'
dcwd.endo.chlord.pcb.aroclor <- rbind(dcwd.endo.chlord.pcb, aroclor.melted)

#we need to take the calcium and magnesium and calculate hardness where we can
calmag <- dcwd.endo.chlord.pcb.aroclor[dcwd.endo.chlord.pcb.aroclor$Name %in% c('Calcium','Magnesium'),]
calmag$tResult <- calmag$tResult*calmag$dnd
calmag.casted <- dcast(calmag, Agency + SampleRegID + SampleAlias + Matrix +
                         Sampled + SpecificMethod +Fraction ~ Name, value.var = 'tResult')
calmag.casted$'Hardness, carbonate as CaCO3' <- 2.497*(calmag.casted$Calcium) + 4.1189*(calmag.casted$Magnesium)
calmag.casted.sub <- within(calmag.casted, rm("Calcium","Magnesium"))
calmag.melted <- melt(calmag.casted.sub, id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SpecificMethod','Fraction'),variable.name = 'Name',value.name = 'tResult')#melt
calmag.melted$dnd <- ifelse(calmag.melted$tResult > 0,1,0)
calmag.melted.addons <- data.frame('tMRL' = rep(0,nrow(calmag.melted)), 
                                   'tMRLUnit' = rep('µg/L',nrow(calmag.melted)),
                                   'Unit' = rep('µg/L',nrow(calmag.melted)), 
                                   'Status' = rep('A',nrow(calmag.melted)))
calmag.melted <- cbind(calmag.melted, calmag.melted.addons)
calmag.melted[calmag.melted$Fraction %in% c('Total', 'Recoverable'),'Fraction'] <- 'Total recoverable'
calmag.melted$Name.full <- paste(calmag.melted$Name, calmag.melted$Fraction, sep = ', ')
calmag.melted$id <- paste(calmag.melted$SampleRegID, calmag.melted$Name.full, calmag.melted$Sampled)
calmag.melted$day <- substr(calmag.melted$Sampled,1,10)
calmag.melted$code <- paste(calmag.melted$SampleRegID, calmag.melted$Name.full, calmag.melted$day)
calmag.melted$index <- as.character(max(as.numeric(dcwd.endo.chlord.pcb.aroclor$index)) + as.numeric(rownames(calmag.melted)))
calmag.melted$criterianame <- calmag.melted$Name.full
calmag.melted$SampleType <- 'Sample'
dcwd.w.totals <- rbind(dcwd.endo.chlord.pcb.aroclor, calmag.melted)

dcwd.w.totals[dcwd.w.totals$Name.full == "Hardness, carbonate as CaCO3",'Name.full'] <- paste(dcwd.w.totals[dcwd.w.totals$Name.full == "Hardness, carbonate as CaCO3",'Name.full'], dcwd.w.totals[dcwd.w.totals$Name.full == "Hardness, carbonate as CaCO3",'Fraction'], sep = ', ')
dcwd.w.totals[dcwd.w.totals$Name.full == "Hardness, carbonate as CaCO3, Total",'Name.full'] <- "Hardness, carbonate as CaCO3, Total recoverable"
dcwd.w.totals[grep('ardnes',dcwd.w.totals$Name.full),'tResult'] <- dcwd.w.totals[grep('ardnes',dcwd.w.totals$Name.full),'tResult']/1000
dcwd.w.totals[grep('ardnes',dcwd.w.totals$Name.full),'criterianame'] <- dcwd.w.totals[grep('ardnes',dcwd.w.totals$Name.full),'Name.full']
dcwd.w.totals <- dcwd.w.totals[!is.na(dcwd.w.totals$tResult),]

#we want the max between endosulfan and its sum of isomers, between chlordane
#and is sum of isomers and between sum of pcb congeners and aroclors.
dcwd.w.totals <- remove.dups(dcwd.w.totals)

#### Associate with criteria ####
#Not sure where the best place is for this but we need to fix some of the issues with Arsenic and Dissolved/Total/Total inorganic
#We can assume per Andrea that all of dissolved is in Total inorganic form. That leaves the conversion for anything in Total recoverable.
dcwd.w.totals[dcwd.w.totals$Name.full %in% c('Arsenic, Total recoverable','Arsenic'),'tResult'] <- dcwd.w.totals[dcwd.w.totals$Name.full %in% c('Arsenic, Total recoverable','Arsenic'),'tResult']*0.76
#dcwd.w.totals[grep('Arsenic',dcwd.w.totals$Name.full),'criterianame'] <- 'Arsenic, Total inorganic'

#We need an ID to match with the criteria
dcwd.w.totals$ID <- paste(dcwd.w.totals$criterianame, dcwd.w.totals$Matrix)
Table3040.applicable$ID <- paste(Table3040.applicable$criterianame, Table3040.applicable$Matrix)

#make the tMRL 0 where it is NA for determining valid samples later on
dcwd.w.totals[is.na(dcwd.w.totals$tMRL),'tMRL'] <- 0

#Now that the names are consistent we can match using analyte name and bring in the criteria

#This is an arbitraty way to pick the minimum criteria
#deq.pollutants <- deq.pollutants[!duplicated(deq.pollutants$ID),]
# Let's hold off on taking the minimum here for now and consider doing after the condition specific criteria calcs
# This is a great idea but computationally, it takes WAY longer because
# #We need a smarter way to do it
# deq.pollutants.min <- ddply(deq.pollutants, .(ID), function(m) {m[which(m$value == min(m$value)),]})
# #Now that we have the minimum we arbitrarily pick one of the variables that had min (e.g. Table 40 W+O or Table 40 O only for Acrolein)
# deq.pollutants <- deq.pollutants.min[!duplicated(deq.pollutants.min$ID),]
dcc <- merge(dcwd.w.totals, Table3040.applicable, by = 'ID', all.x = TRUE)
dcc <- within(dcc, rm(criterianame.y))
#dcc <- rename(dcc, c('Pollutant.x' = 'Pollutant'))

# #So i can re-run from here. I am going to output the intermediates
# dcwd.w.totals <- within(dcwd.w.totals, rm(id))
# dcc <- within(dcc, rm(id))
# con <- odbcConnect('WQAssessment')
# # sqlSave(con, dcwd.w.totals, tablename = '2012_INTERMEDIATE_dcwdwtotals')
# # sqlSave(con, dcc, tablename = '2012_INTERMEDIATE_dcc')
# dcwd.w.totals <- sqlFetch(con, '2012_INTERMEDIATE_dcwdwtotals')
# dcwd.w.totals <- rename(dcwd.w.totals, c('Namefull' = 'Name.full'))
# dcwd.w.totals$tMRL <- as.numeric(dcwd.w.totals$tMRL)
# dcwd.w.totals$tResult <- as.numeric(dcwd.w.totals$tResult)
# dcc <- sqlFetch(con, '2012_INTERMEDIATE_dcc')
# dcc <- rename(dcc, c('Namefull' = 'Name.full', 'Matrixx' = 'Matrix.x', 'Matrixy' = 'Matrix.y'))
# dcc$tMRL <- as.numeric(dcc$tMRL)
# dcc$tResult <- as.numeric(dcc$tResult)
# dcc$value <- as.numeric(dcc$value)
# odbcCloseAll()

#### Hardness criteria calculation ####
#Using the hardness evaluation function loaded above we can calculate the hardness based criteria values
#and bring them into the dataframe with the other criteria values. First, though we remove the hardness 
#metals from the dataframe since the output of the function maintains all the columns of the original dataframe
#Calculate criteria based on hardness
hm <- hardness.crit.calc(dcwd.w.totals)
#Push CFC and CFA for lead to table to be used later for conversions
td.conv <- hm[grepl('Dissolved',hm$Criteria.Name.full) & hm$Name.full != hm$Criteria.Name.full,c('code','CFC','CFA','CFC_SW','CFA_SW')]
#We are going to hold off on this for now
# #Determine minimum applicable criteria for each sample
# hm.min <- ddply(hm, .(criterianame, SampleRegID, Sampled, Fraction), function(m) {m[which(m$value == min(m$value)),]})

# #Preserve result column
# hm$tResult.old <- hm$tResult
# 
# #Select appropriate sample fraction when both available and convert when NOT except where the sample is ND. 
# hm.frac <- ddply(hm, .(criterianame, SampleRegID, Sampled), function(m) {
#   if(nrow(m) > 1) {
#     use <- m[m$criterianame == m$Name.full,] 
#     use$tResult <- use$tResult.old
#   } else {
#     use <- m
#     use$tResult <- ifelse(use$tMRL < use$value,ifelse(grepl('Chronic',use$variable),use$tResult*use$CFC,use$tResult*use$CFA),use$tResult.old)
#   }
#   return(use)
# })
# hm.frac$check <- paste(hm.frac$criterianame, hm.frac$SampleRegID, hm.frac$Sampled)
# any(duplicated(hm.frac$check))
# hm.frac <- within(hm.frac, rm(check))
# #Percentages of using a non-matching sample fraction to compare to the standard
# # table(hm.frac[hm.frac$criterianame != hm.frac$Name.full,'criterianame'])/ table(hm.frac$criterianame) *100
hm <- hm[,names(dcc)]
# hm.frac$ID <- paste(hm.frac$criterianame, hm.frac$Matrix.x)
dcc.wo.hm <- dcc[!dcc$ID %in% hm$ID,]
dcc.hm <- rbind(dcc.wo.hm, hm)

#### Pentachlorophenol pH dependent criteria calculation ####
#Similarly pentachlorophenol is parameter dependent and is handled the same as hardness
# penta <- pentachlorophenol.crit.calc(dcwd.w.totals)
# penta <- penta[,names(dcc)]
# penta40 <- dcc[dcc$criterianame == 'Pentachlorophenol' & !is.na(dcc$variable),]
# penta.all <- rbind(penta, penta40)
# penta.min <- ddply(penta.all, .(criterianame, SampleRegID, Sampled, Fraction), function(m) {m[which(m$value == min(m$value)),]})

## Turns out Table 40 criteria for penta are ALWAYS lower for this dataset ##

#dcc.wo.penta <- dcc.hm[!dcc.hm$criterianame %in% penta$criterianame,]
#dcc.penta <- rbind(dcc.wo.penta, penta)
#rm(list = ls()[c(grep('penta', ls()))])

#make a couple sites saltwater for testing
#data.wo.void[data.wo.void$SampleRegID %in% c(28574,13141),'Matrix'] <- 'SW'

#### Ammonia pH and temperature dependent criteria calculation ####
#Ammonia is also parameter dependent and is handled similarly
amm <- ammonia.crit.calc(dcwd.w.totals)
amm <- amm[,names(dcc.hm)]
#amm.min <- ddply(amm, .(criterianame, SampleRegID, Sampled, Fraction), function(m) {m[which(m$value == min(m$value)),]})
#amm.min$ID <- paste(amm.min$criterianame, amm.min$Matrix.x)
dcc.wo.amm <- dcc.hm[!dcc.hm$ID %in% amm$ID,]
dcc.w.calcs <- rbind(dcc.wo.amm, amm)

#### EVALUATION ####
#First we'll take the minimum associated criteria for each sample
#dcc.min <- ddply(dcc.w.calcs, .(Name.full, SampleRegID, Sampled), function(m) {m[which(m$value == min(m$value)),]})
#regenerate this so it is new as long as any(duplicated(rownames(dcc.w.calcs))) evaluates to false
dcc.w.calcs$index <- rownames(dcc.w.calcs)
#ddply ran out of memory so I am using dplyr here which works way faster. Not sure this is the correct usage. There is probably
#a more elegant way but it's doing what I want so far. 
# library(dplyr)
# dcc.w.calcs.groups <- group_by(dcc.w.calcs, code)
# dcc.w.calcs.filter <- filter(dcc.w.calcs.groups, rank(value, ties.method="first")==1)
# dcc.min.index <- summarise(dcc.w.calcs.groups, value = min(value), index = which.min(value))
# dcc.min <- dcc.w.calcs[dcc.w.calcs$index %in% dcc.min.index$index,]
# detach(package:dplyr)

#dplyr can't return whole rows efficiently yet. this is a data table solution retrieved from this thread http://stackoverflow.com/questions/21308436/dplyr-filter-get-rows-with-minimum-of-variable-but-only-the-first-if-multiple
library(data.table)
dt <- as.data.table(dcc.w.calcs)
setkey(dt, code,value)
dt.min <- dt[J(unique(code)), mult="first"]
dcc.min <- as.data.frame(dt.min)
rm(dt, dt.min)

#Select appropriate sample fraction when both available and convert when NOT except where the sample is ND. 
#Pick up here: This section is not working yet.
dcc.min$tResult.old <- dcc.min$tResult
dissolved.metals.criteria <- unique(dcc.min[grep('Dissolved',dcc.min$Criteria.Name.full),c('Criteria.Name.full')])
sub1 <- dcc.min[dcc.min$Criteria.Name.full %in% dissolved.metals.criteria,]
sub2 <- merge(sub1, td.conv, by = 'code', all.x = TRUE)
sub2$x <- apply(sub2[,names(sub2)],1,paste,collapse=',')
sub2 <- sub2[!duplicated(sub2$x),]
sub2 <- within(sub2, rm(x))
sub3 <- sub2[sub2$Criteria.Name.full != sub2$Name.full,]
sub4 <- sub3[sub3$dnd == 1,] 
sub4[sub4$Criteria.Name.full == 'Chromium, Dissolved',c('CFA')] <- 0.982 
sub4[sub4$Criteria.Name.full == 'Chromium, Dissolved',c('CFC')] <- 0.962
sub4[sub4$Criteria.Name.full == 'Chromium, Dissolved',c('CFA_SW','CFC_SW')] <- 0.993
sub4[sub4$Criteria.Name.full == 'Selenium, Dissolved', 'CFA'] <- 0.996
sub4[sub4$Criteria.Name.full == 'Selenium, Dissolved', 'CFC'] <- 0.922
sub4[sub4$Criteria.Name.full == 'Selenium, Dissolved', c('CFC_SW','CFA_SW')] <- 0.998 
sub4$tResult <- ifelse(grepl('Chronic',sub4$variable),
                       ifelse(grepl('Saltwater',sub4$variable),
                              sub4$tResult*sub4$CFC_SW,
                              sub4$tResult*sub4$CFC),
                       ifelse(grepl('Saltwater',sub4$variable),
                              sub4$tResult*sub4$CFA_SW,
                              sub4$tResult*sub4$CFA))

sub4 <- sub4[,names(dcc.min)]
dcc.min.wo.sub4 <- dcc.min[!dcc.min$index %in% sub4$index,]
dcc.min <- rbind(dcc.min.wo.sub4, sub4)

#evaluate exceedances to the minimum criteria
dcc.min$exceed <- ifelse(dcc.min$criterianame.x == 'Alkalinity',ifelse(dcc.min$tResult > dcc.min$value,0,1),ifelse(dcc.min$tResult < dcc.min$value,0,1))

#Now on to determining which are valid exceedances
#Where the MRL is greater than the criteria we can't use that sample to determine attainment or non-attainment
dcc.min$Valid <- ifelse(dcc.min$tMRL <= dcc.min$value, 1, 0)

#Dissolved versus total criteria
dtc <- dcc.min[dcc.min$Criteria.Name.full != dcc.min$Name.full & dcc.min$Fraction == 'Dissolved' & grepl('Total',dcc.min$Criteria.Name.full),]
dtc$Valid <- ifelse(dtc$tResult > dtc$value,ifelse(dtc$Valid == 0,0,1),0)
dcc.min.wo.dtc <- dcc.min[!dcc.min$index %in% dtc$index,]
dcc.min <- rbind(dcc.min.wo.dtc, dtc)





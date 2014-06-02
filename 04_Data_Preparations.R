library(plyr)

options(stringsAsFactors = FALSE, scipen = 100)

#### This file is to bring the two datasets lasar and wqp.data into a single dataframe for moving forward with 
#bring the two data sets into one now, lasar and wqp.data to feed into Toxics Analysis
names(lasar)
names(wqp.data)
#....we've got some work to do

View(arrange(criteria.values.melted.applicable.nonnum[grepl(', Total',criteria.values.melted.applicable.nonnum$Pollutant) & 
                                                        criteria.values.melted.applicable.nonnum$variable %in% deq.pollutants$variable,],Pollutant))
View(arrange(criteria.values.melted.applicable.nonnum[grepl('issolv',criteria.values.melted.applicable.nonnum$Pollutant) & 
                                                        criteria.values.melted.applicable.nonnum$variable %in% deq.pollutants$variable,],Pollutant))

#### First the Water Quality Portal data ####
#Remove Bed Sediment and Suspended samples
wqp.data <- wqp.data[!wqp.data$ResultSampleFractionText %in% c('Bed Sediment','Suspended'),]

#Remove samples labeled as Interstitial
wqp.data <- wqp.data[wqp.data$ActivityMediaSubdivisionName != 'Interstitial',]

#Make a date-time column
wqp.data$Sampled <- paste(wqp.data$ActivityStartDate, wqp.data$ActivityStartTimeTime)

#Make a column with criteria name based on earlier mapping 
wqp.name.match <- read.csv('WQPNameMatch_05142014.csv',stringsAsFactors=FALSE)
wqp.data$criteria.name <- mapvalues(wqp.data$CharacteristicName, from = wqp.name.match$WQP.Name, to = wqp.name.match$Criteria.Name)

#table(wqp.data[wqp.data$ResultSampleFractionText %in% c('Dissolved','Recoverable','Total'),'CharacteristicName'],wqp.data[wqp.data$ResultSampleFractionText %in% c('Dissolved','Recoverable','Total'),'ResultSampleFractionText'])

# wqp.name.check <- merge(wqp.data[,c('CharacteristicName','ResultSampleFractionText')], wqp.name.match, by.x = 'CharacteristicName', by.y = 'WQP.Name', all.x = TRUE)
# wqp.name.check$x <- apply(wqp.name.check[,names(wqp.name.check)],1,paste,collapse=',')
# wqp.name.check <- wqp.name.check[!duplicated(wqp.name.check$x),]
# wqp.name.check <- within(wqp.name.check, rm(x))

#Pull in site_only so when we build summary info later it will fit that formatting better
wqp.data <- merge(wqp.data, wqp.stations[,c('MonitoringLocationIdentifier','site_only')], by = 'MonitoringLocationIdentifier', all.x = TRUE)

#Not sure what I was doing with this one
#wqp.data$adid <- paste(wqp.data$CharacteristicName, wqp.data$ActivityStartDate)

#Let's pull out only those columns we need to make this business work
wqp.data.sub <- wqp.data[,c('site_only','OrganizationFormalName',
                            'Sampled', 'CharacteristicName','ResultSampleFractionText',
                            'ResultMeasureValue','ResultMeasureMeasureUnitCode','MeasureQualifierCode',
                            'ActivityTypeCode','DetectionQuantitationLimitMeasureMeasureValue','DetectionQuantitationLimitMeasureMeasureUnitCode',
                            'MonitoringLocationName', 'criteria.name','ResultAnalyticalMethodMethodIdentifier')]

#Now we make the names match the script I built for the Toxics Monitroing Program
wqp.data.sub <- rename(wqp.data.sub, c('site_only' = 'SampleRegID',
                                       'OrganizationFormalName' = 'Agency',
                                       'CharacteristicName' = 'Pollutant',
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
#Make a date-time field
lasar$Sampled <- paste(as.character(lasar$SAMPLE_DATE), substr(as.character(lasar$SAMPLE_TIME),12,19))

#see how much diff there is with method_detection_limit and method_reporting_limit
#View(lasar[which(lasar$METHOD_DETECTION_LIMIT != lasar$METHOD_REPORTING_LIMIT),])
#we're going with METHOD_REPORTING_LIMIT

#again not sure why this is here
#lasar$adid <- paste(lasar$NAME, lasar$Sampled)

#Make the lasar names match the script and be consistent with the new wqp names
lasar.new.names <- rename(lasar, c('NAME' = 'Pollutant',
                         'ABBREVIATION' = 'Fraction',
                         #'criteria.name' = 'Analyte', 
                         'STATION_KEY' = 'SampleRegID',
                         'LOCATION_DESCRIPTION' = 'SampleAlias',
                         'QA_QC_TYPE' = 'SampleType',
                         'Result_clean' = 'tResult',
                         'METHOD_REPORTING_LIMIT' = 'tMRL',
                         'UNIT' = 'Unit',
                         'METHOD' = 'SpecificMethod',
                         'STATUS' = 'Status'))
lasar.new.names$Agency <- 'ODEQ'
lasar.new.names$tMRLUnit <- lasar.new.names$Unit

#This subsets the lasasr dataframe to only have the columns to be used for aggregation
lasar.new.names <- lasar.new.names[,names(wqp.data.sub)]

#### Pulling wqp.data and lasar together now! ####
data.complete <- rbind(lasar.new.names, wqp.data.sub)

#### Cleaning up the complete dataset and making some fields for consistent future processing ####
#should have a numeric result and mrl fields
data.complete$tResult <- as.numeric(data.complete$tResult)
data.complete$tMRL <- as.numeric(data.complete$tMRL)

#making the name.full can happen after we combine data sets
total.to.recoverable <- c('Arsenic','Mercury','Copper','Zinc','Nickel','Lead','Selenium',
                          'Chromium','Iron','Barium','Thallium','Manganese','Silver','Antimony',
                          'Cadmium','Cyanide','Chromium(VI)')
dissolved.metals <- c('Arsenic','Cadmium','Chromium','Chromium(VI)','Lead','Nickel','Selenium','Silver','Zinc')
data.complete[data.complete$Pollutant %in% total.to.recoverable & data.complete$Fraction %in% c('Total', 'Recoverable'),'Fraction'] <- 'Total recoverable'
data.complete$Name.full <- ifelse(data.complete$Pollutant %in% c(total.to.recoverable, dissolved.metals) & 
                                    data.complete$Fraction %in% c('Dissolved', 'Total recoverable'),
                                  paste(data.complete$Pollutant, ", ", data.complete$Fraction, sep = ''),
                                  data.complete$Pollutant)

#found some hanging bad QA QC Types from lasar data set
data.complete <- data.complete[!data.complete$SampleType %in% c('Equipment Blank - Field', 
                                                                'Matrix Spike - Field', 
                                                                'Matrix Spike Duplicate - Field', 
                                                                'Transfer Blank'),]

data.complete$id <- paste(data.complete$SampleRegID, data.complete$Name.full, data.complete$Sampled)
data.complete$day <- substr(data.complete$Sampled,1,10)
data.complete$code <- paste(data.complete$SampleRegID, data.complete$Name.full, data.complete$day)
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
remove.dups <- function(tname) {
  no.dups <- aggregate(tResult ~ id, data = tname, FUN = min)
  tname <- tname[!duplicated(tname$id),]
  tname <- merge(no.dups, tname, by = 'id')
  tname$tResult <- round(tname$tResult.x, 2)
  tname <- within(tname, rm(tResult.x, tResult.y))
}

data.complete.wo.dups <- remove.dups(data.complete.w.resolved.fd)

#### Pick total or dissolved based on criteria ####
#We will make the assumption that if fraction is not specified, then it is toal
data.complete.wo.dups[data.complete.wo.dups$Name.full %in% c(total.to.recoverable),'Name.full'] <- paste(data.complete.wo.dups[data.complete.wo.dups$Name.full %in% c(total.to.recoverable),'Name.full'], ', Total recoverable', sep ='')

View(arrange(unique(data.complete.wo.dups[data.complete.wo.dups$Pollutant %in% c(total.to.recoverable, dissolved.metals) & 
                                            data.complete.wo.dups$Name.full != data.complete.wo.dups$criteria.name,c('Name.full','criteria.name')]),criteria.name))

View(arrange(unique(data.complete.wo.dups[data.complete.wo.dups$Pollutant %in% c(total.to.recoverable, dissolved.metals),c('Name.full','criteria.name')]),criteria.name))

metals <- unique(data.complete.wo.dups[data.complete.wo.dups$Pollutant %in% c(total.to.recoverable, dissolved.metals) ,'criteria.name'])

deq.metals.criteria <- criteria.values.melted.applicable.nonnum[criteria.values.melted.applicable.nonnum$Pollutant %in% 
                                                                  metals & criteria.values.melted.applicable.nonnum$variable %in% 
                                                                  deq.pollutants$variable,]
source('TMP-RCode/hardness_eval_functions_Element_Names.R')

dc.dissolved.crit <- (data.complete.wo.dups[data.complete.wo.dups$criteria.name %in% constants$Analyte,])

paste(data.complete.wo.dups$SampleRegID, data.complete.wo.dups$Sampled, data.complete.wo.dups$Pollutant)

library(plyr)
library(reshape2)
library(stringr)
library(RODBC)

options(stringsAsFactors = FALSE, scipen = 100)

#### This file is to bring the two datasets lasar and wqp.data into a single dataframe for moving forward with 
#bring the two data sets into one now, lasar and wqp.data to feed into Toxics Analysis
con <- odbcConnect('WQAssessment')
wqp.data <- sqlFetch(con, 'WQPData_woLASAROverlap_06132014')
wqp.stations <- sqlFetch(con, 'WQPStations_wUpdatedLatLon_06132014')
lasar <- sqlFetch(con, 'LASAR_Toxics_Query_wCriteriaNames_06132014')
odbcCloseAll()
#names(wqp.data)
#....we've got some work to do

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
wqp.name.match <- read.csv('WQPNameMatch_05142014.csv',stringsAsFactors=FALSE)
wqp.data$criterianame <- mapvalues(wqp.data$CharacteristicName, from = wqp.name.match$WQP.Name, to = wqp.name.match$Criteria.Name)

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
                            'MonitoringLocationName', 'criterianame','ResultAnalyticalMethodMethodIdentifier')]

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
#Preserve lasar query
#lasar.ng <- lasar

#Make recoverable lower case to be consistent 
lasar$ABBREVIATION <- gsub('R','r',lasar$ABBREVIATION)

#Not sure if this field gets used but here it is
lasar$test <- ifelse(lasar$ABBREVIATION %in% c('Dissolved', 'Total recoverable'),paste(lasar$NAME, ", ", lasar$ABBREVIATION, sep = ''),lasar$NAME)

#Pull the lasar data frame in new if we need to 
#lasar <- sqlFetch(con, 'LASAR_Toxics_Query_06112014')

#Make a date-time field
lasar$Sampled <- paste(as.character(lasar$SAMPLE_DATE), as.character(lasar$SAMPLE_TIME)) #i had this in here because the lasar date and time were coming in as posix but today they come in as character so i'm taking it out: substr(as.character(lasar$SAMPLE_TIME),12,19)

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
#because there are some with multiple methods and two MRL levels we can't just pick the max
#value unless it was a detection
for (i in 1:length(unique(data.complete.w.resolved.fd$id))) {
  sub <- data.complete.w.resolved.fd[data.complete.w.resolved.fd$id == unique(data.complete.w.resolved.fd$id)[i],]
  1if (nrow(sub) > 1) {
    if any(sub$dnd == 1) {
      sub <- aggregate(tMRL ~ id, data = sub, FUN = min)
    }
  }
  
  max.result <- aggregate(tResult ~ id, data = sub, FUN = max)
}

remove.dups <- function(tname) {
  no.dups <- aggregate(tResult ~ id, data = tname, FUN = max)
  tname <- tname[!duplicated(tname$id),]
  tname <- merge(no.dups, tname, by = 'id')
  #tname$tResult <- round(tname$tResult.x, 2)
  tname$tResult <- tname$tResult.x
  tname <- within(tname, rm(tResult.x, tResult.y))
}

data.complete.wo.dups <- data.complete.w.resolved.fd

#### Grouping parameters to be compared to composite criteria ####
#We will add this here for now but should be coming from Station locate process
data.complete.wo.dups$Matrix <- 'FW' #mapvalues(data.complete.wo.dups$Matrix, from = c("River/Stream", "Estuary"), to = c('FW','SW'))

#Some of the criteria apply to Totals and not individual degradates. Here we do that totalling so comparisons can be done.
#First, we will make a Total DDT
ddt <- data.complete.wo.dups[data.complete.wo.dups$Pollutant %in% c("4,4`-DDD", "4,4`-DDE", "4,4`-DDT", "p,p'-DDD", "p,p'-DDE", "p,p'-DDT"),]
ddt$tResult <- ddt$tResult*ddt$dnd
ddt.casted <- dcast(ddt, Agency + SampleRegID + SampleAlias + Matrix + 
                      Sampled + SampleType + SpecificMethod +tMRL + tMRLUnit ~ Pollutant, value.var = 'tResult')
ddt.casted$'Total DDT' <- rowSums(ddt.casted[,c("4,4`-DDD", "4,4`-DDE", "4,4`-DDT","p,p'-DDD", "p,p'-DDE", "p,p'-DDT")],na.rm=TRUE)
ddt.casted.sub <- within(ddt.casted, rm("4,4`-DDD", "4,4`-DDE", "4,4`-DDT","p,p'-DDD", "p,p'-DDE", "p,p'-DDT"))
ddt.melted <- melt(ddt.casted.sub, 
                   id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SampleType','SpecificMethod', 'tMRL', 'tMRLUnit'),
                   variable.name = 'Pollutant',
                   value.name = 'tResult')
#ddt.melted$Detect.nondetect <- ifelse(ddt.melted$tResult > 0,1,0)
ddt.melted.addons <- data.frame('tMRL' = rep(0,nrow(ddt.melted)), 'Unit' = rep('µg/L',nrow(ddt.melted)), 'Status' = rep('A',nrow(ddt.melted)))
ddt.melted <- cbind(ddt.melted, ddt.melted.addons)
data.complete.wo.dups <- rbind(data.complete.wo.dups, ddt.melted)

#Now Total Endosulfan
endo <- data.complete.wo.dups[data.complete.wo.dups$Analyte %in% c("Endosulfan I", "Endosulfan II", "Endosulfan Sulfate"),]
endo.casted <- dcast(endo, Agency + SampleRegID + SampleAlias + Matrix +
                       Sampled + SampleType + SpecificMethod + chem.group ~ Analyte, value.var = 'tResult')
endo.casted$Endosulfan <- rowSums(endo.casted[,c("Endosulfan I", "Endosulfan II", "Endosulfan Sulfate")],na.rm=TRUE)
endo.casted.sub <- within(endo.casted, rm("Endosulfan I", "Endosulfan II", "Endosulfan Sulfate"))
endo.melted <- melt(endo.casted.sub, id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SampleType','SpecificMethod','chem.group'),variable.name = 'Analyte',value.name = 'tResult')#melt
endo.melted$Detect.nondetect <- ifelse(endo.melted$tResult > 0,1,0)
endo.melted.addons <- data.frame('tMRL' = rep(0,nrow(endo.melted)), 'Unit' = rep('µg/L',nrow(endo.melted)), 'Status' = rep('A',nrow(endo.melted)))
endo.melted <- cbind(endo.melted, endo.melted.addons)
data.complete.wo.dups <- rbind(data.complete.wo.dups, endo.melted)

#Now Total Chlordane
chlordane <- data.complete.wo.dups[data.complete.wo.dups$Analyte %in% c("Oxychlordane", "alpha-Chlordane", "cis-Chlordane", 'trans-Chlordane',"gamma-Chlordane+trans-Nonachlor", "trans-Nonachlor", "cis-Nonachlor"),]
chlordane.casted <- dcast(chlordane, Agency + SampleRegID + SampleAlias + Matrix +
                            Sampled + SampleType + SpecificMethod + chem.group ~ Analyte, value.var = 'tResult')
chlordane.casted$Chlordane <- rowSums(chlordane.casted[,c("Oxychlordane", "alpha-Chlordane", "cis-Chlordane", 'trans-Chlordane',"gamma-Chlordane+trans-Nonachlor", "trans-Nonachlor", "cis-Nonachlor")],na.rm=TRUE)
chlordane.casted.sub <- within(chlordane.casted, rm("Oxychlordane", "alpha-Chlordane", "cis-Chlordane", 'trans-Chlordane',"gamma-Chlordane+trans-Nonachlor", "trans-Nonachlor", "cis-Nonachlor"))
chlordane.melted <- melt(chlordane.casted.sub, id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SampleType','SpecificMethod','chem.group'),variable.name = 'Analyte',value.name = 'tResult')#melt
chlordane.melted$Detect.nondetect <- ifelse(chlordane.melted$tResult > 0,1,0)
chlordane.melted.addons <- data.frame('tMRL' = rep(0,nrow(chlordane.melted)), 'Unit' = rep('µg/L',nrow(chlordane.melted)), 'Status' = rep('A',nrow(chlordane.melted)))
chlordane.melted <- cbind(chlordane.melted, chlordane.melted.addons)
data.complete.wo.dups <- rbind(data.complete.wo.dups, chlordane.melted)

#Now total PCBs
pcb <- data.complete.wo.dups[grep('PCB',data.complete.wo.dups$Analyte),]
pcb.casted <- dcast(pcb, Agency + SampleRegID + SampleAlias + Matrix + 
                      Sampled + SampleType + SpecificMethod + chem.group ~ Analyte, value.var = 'tResult')
pcb.casted$'Polychlorinated Biphenyls (PCBs)' <- rowSums(pcb.casted[,unique(data.complete.wo.dups[grep('PCB',data.complete.wo.dups$Analyte),'Analyte'])],na.rm=TRUE)
pcb.casted.sub <- pcb.casted[,!names(pcb.casted) %in% unique(data.complete.wo.dups[grep('PCB',data.complete.wo.dups$Analyte),'Analyte'])]
pcb.melted <- melt(pcb.casted.sub, id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SampleType','SpecificMethod','chem.group'),variable.name = 'Analyte',value.name = 'tResult')#melt
pcb.melted$Detect.nondetect <- ifelse(pcb.melted$tResult > 0,1,0)
pcb.melted.addons <- data.frame('tMRL' = rep(0,nrow(pcb.melted)), 'Unit' = rep('µg/L',nrow(pcb.melted)), 'Status' = rep('A',nrow(pcb.melted)))
pcb.melted <- cbind(pcb.melted, pcb.melted.addons)
data.complete.wo.dups <- rbind(data.complete.wo.dups, pcb.melted)

#### Associate with criteria and calculate hardness based criteria ####
#We need an ID to match with the criteria so we'll simplify the Matrix field
data.complete.wo.dups$Matrix <- 'FW' #mapvalues(data.complete.wo.dups$Matrix, from = c("River/Stream", "Estuary"), to = c('FW','SW'))
data.complete.wo.dups$ID <- paste(data.complete.wo.dups$criteria.name, data.complete.wo.dups$Matrix)

#Now that the names are consistent we can match using analyte name and bring in the criteria
deq.pollutants <- criteria.values.melted.applicable[criteria.values.melted.applicable$variable %in% 
                                                      c('Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism',
                                                        'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only',
                                                        'Table 30 Toxic Substances - Freshwater Acute',
                                                        'Table 30 Toxic Substances - Freshwater Chronic',
                                                        'Table 30 Toxic Substances - Saltwater Acute',
                                                        'Table 30 Toxic Substances - Saltwater Chronic'),]
deq.pollutants <- deq.pollutants[!duplicated(deq.pollutants$Pollutant),]
criteria.for.analytes.we.have <- deq.pollutants[deq.pollutants$Pollutant %in% data.complete.wo.dups$criteria.name,]
dcc <- merge(data.complete.wo.dups, criteria.for.analytes.we.have, by = 'ID', all.x = TRUE)

#Where the MRL is greater than the criteria we can't use that sample to determine attainment or non-attainment
dcc$Valid <- ifelse()

#Using the hardness evaluation function loaded above we can calculate the hardness based criteria values
#and bring them into the dataframe with the other criteria values. First, though we remove the hardness 
#metals from the dataframe since the output of the function maintains all the columns of the original dataframe
hm <- hardness.crit.calc(data.complete.wo.dups)
hm <- hm[,names(dcc)]
dvc.wo.hm <- dvc[!dvc$Analyte %in% hm$Analyte,]
dvc.hm <- rbind(dvc.wo.hm, hm)


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
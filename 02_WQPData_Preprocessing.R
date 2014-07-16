#This file is to pre process the WQP data to handle flags, NA's, duplicates, and to make sure we've got everything we should

library(RODBC)
library(plyr)
#library(dplyr) #note that this package has some overlap with plyr

options(stringsAsFactors = FALSE)

con <- odbcConnect('WQAssessment')

wqp.data <- sqlFetch(con, 'WQPData_05022014')


wqp.data$x <- apply(wqp.data[,names(wqp.data)],1,paste,collapse=',')
wqp.data <- wqp.data[!duplicated(wqp.data$x),]
wqp.data <- within(wqp.data, rm(x))

wqp.stations <- sqlFetch(con, 'WQPStations_05052014')
st <- sqlFetch(con, 'WQPQueryStatus_05052014')

wqp.data <- merge(wqp.data, wqp.stations[,c('MonitoringLocationIdentifier','MonitoringLocationName')], all.x = TRUE)

#add in phosphate data that we missed the first time
#names(tmp.phos.data) <- gsub('\\.','',names(tmp.phos.data))
wqp.data <- rbind(wqp.data, tmp.phos.data)

#str(wqp.data)

#There seem to be a lot of NAs in this field. 
#table(wqp.data$ResultMeasureValue, useNA = "always") #Need to discern if they are ND's or what
#table(wqp.data[is.na(wqp.data$ResultMeasureValue),'MeasureQualifierCode'],useNA = 'always') #No qualifiers associated
#wqp.data <- wqp.data[!is.na(wqp.data$ResultMeasureValue),] #Let's just remove them
#Actually it turns out that we want to keep them in since there is another explanatory field explaining why they are NA
#The ResultDetectionConditionText stores whether the sample was a ND. But there are a few that are Not Reported that we do want to remove.
#We will take the subset of data initially excluded for having NA and then run only those data through the QC below and then append 
#to the dataset that has already received subsequent processing. This note is updated as of 07162014.
wqp.ND <- wqp.data[is.na(wqp.data$ResultMeasureValue),] 
wqp.ND[!wqp.ND$ResultDetectionConditionText %in% c('Not Reported'),'ResultMeasureValue'] <- 0
#Now we can remove the rest
wqp.ND <- wqp.ND[!is.na(wqp.ND$ResultMeasureValue),] 
wqp.ND <- wqp.ND[wqp.ND$OrganizationFormalName != 'Bureau of Reclamation',]
wqp.ND <- wqp.ND[-grep('Qualifier=Q',wqp.ND$ResultCommentText),]
wqp.ND$Tempid <- NA
# wqp.data.wOrtho <- sqlFetch(con, 'WQPData_wOrthophos_07092014')
# wqp.data.wND <- rbind(wqp.data.wOrtho, wqp.ND)
# sqlSave(con, wqp.data.wND, tablename = 'WQPData_wND_07162014', rownames = FALSE)

MORE.stations.to.locate <- unique(wqp.ND$MonitoringLocationIdentifier[!wqp.ND$MonitoringLocationIdentifier %in% wqp.data.wOrtho$MonitoringLocationIdentifier])
MORE.stl.info <- wqp.stations[wqp.stations$MonitoringLocationIdentifier %in% MORE.stations.to.locate,]
#These are all usgs stations and some of them were used in the draft too so were included in the first list I gave MIke
sul2012 <- read.dbf('C:/Users/pbryant/Desktop/Stations_2012_Analysis.dbf')
MORE.stl.info$site_only <- gsub('USGS-','',MORE.stl.info$MonitoringLocationIdentifier)
MORE.stl.info <- MORE.stl.info[!MORE.stl.info$site_only %in% sul2012$STATION,]
#write.csv(MORE.stl.info, 'StationstoLocate/MORE_stations_to_locate_07162014.csv', row.names = FALSE)

#looks like there's some groundwater that crept in
wqp.data <- wqp.data[which(wqp.data$ActivityMediaSubdivisionName != 'Groundwater'),]

#remove NA rows - Not needed anymore. Figured out that ActivityMediaSubdivisionName had 133 rows of NA which after doing the 
#previous line forced the whole row to be NA
#wqp.data <- wqp.data[!is.na(wqp.data$OrganizationFormalName),]

#look at who collected these data
#table(wqp.data$OrganizationFormalName)
#View(wqp.data[wqp.data$OrganizationFormalName == 'USGS Georgia Water Science Center',])

#### Initial high level QC information identification screen ####
#Apparently QC is a pretty squishy process per our QA/QC folks at the lab. As such the recommendation was to go through each
#sampling organization and look at qualifiers or text fields containing qualifier information. One of the main issues is that
#if you don't see a qualifier you would expect that piece of data to be ok to use but the consistency with which these fields
#are filled out is inconsistent and that assumption can't be made across sampling organizations (or probably even projects). 
#As such this review will attempt to identify if their is any level of consistency in filling out these fields such that we can 
#safely assume data without qualifiers associated with that organization are of accpetable quality for inclusion. 
#Some outright assumptions that apply to all the data we will use from NWIS and STORET is that they followed proper sampling technique,
#sample handling and sample preservation.
# unique(wqp.data$OrganizationFormalName)
# [1] "Bureau of Reclamation"                                             "National Park Service Water Resources Division"                   
# [3] "Nevada Dept. of Conservation and Natural Resources"                "Cow Creek Umpqua Indians"                                         
# [5] "U.S. Army Corps of Engineers Walla Walla District"                 "Confederated Tribes of the Coos, Lower Umpqua and Siuslaw Indians"
# [7] "Environmental Monitoring and Assessment Program"                   "EPA National Aquatic Resource Survey Data"                        
# [9] "EPA National Aquatic Resources Survey"                             "EPA National Aquatic Resources Survey (TEST Submission)"          
# [11] "Nez Perce Tribe"                                                   "EPA Region 10 Superfund Portland Harbor Site"                     
# [13] "Umatilla National Forest (Washington and Oregon)"                  "USGS Oregon Water Science Center"                                 
# [15] "USGS Idaho Water Science Center"                                   "USGS Georgia Water Science Center"
#a little manual looping here so i can step through these one by one
# i = 16
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'ResultCommentText'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'ResultValueTypeName'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'ResultLaboratoryCommentText'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'DetectionQuantitationLimitTypeName'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'AnalysisStartDate'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'PreparationStartDate'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'MeasureQualifierCode'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'LaboratoryName'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'CharacteristicName'])

#For Bureau of Reclamation: No Result Comments, No qualifiers, inconsistency in sample type information grab vs composite, No lab information
#Conclusion: too much uncertainty to use ----> EXCLUDE
wqp.data <- wqp.data[wqp.data$OrganizationFormalName != 'Bureau of Reclamation',]

#For National Park Service Water Resources Division: A few result comments, lab used is specified
#Conclusion: For these parameters; pH and Chloride these are probably OK. ----> INCLUDE

#For Nevada Dept. of Conservation and Natural Resources: No supporting info
#conclusion: -----> EXCLUDE
wqp.data <- wqp.data[wqp.data$OrganizationFormalName != 'Nevada Dept. of Conservation and Natural Resources',]

#For Cow Creek Umpqua Indians: No supporting info
#Conclusion: too much uncertainty to use ----> EXCLUDE
wqp.data <- wqp.data[wqp.data$OrganizationFormalName != 'Cow Creek Umpqua Indians',]

#For U.S. Army Corps of Engineers Walla Walla District: No supporting info
#Conclusion: too much uncertainty to use ----> EXCLUDE
wqp.data <- wqp.data[wqp.data$OrganizationFormalName != 'U.S. Army Corps of Engineers Walla Walla District',]

#For Confederated Tribes of the Coos, Lower Umpqua and Siuslaw Indians: pH only -- beyond current scope
#Conclusion: For now ------> EXCLUDE
wqp.data <- wqp.data[wqp.data$OrganizationFormalName != 'Confederated Tribes of the Coos, Lower Umpqua and Siuslaw Indians',]

#For Environmental Monitoring and Assessment Program: Data has result comments and lab specified for nitrates
#Conclusion: -----> INCLUDE

#For EPA National Aquatic Resource Survey Data & EPA National Aquatic Resources Survey (TEST Submission): Data has result comments which indicate B data and nothing less, Sampling organization reliable
#Issues appear to be related to holding time which indicates B data
# FROM http://water.epa.gov/type/rsl/monitoring/riverssurvey/upload/NRSA0809_Data_WaterChemistry_AlertQual_130926.csv
# Table of alert code descritions relating to values from ResultCommentText
# PUBLICATION_DATE  SAMPLE_TYPE	ALERT	QUALIFIER	DEFINITION
# 2/25/2013	CHEM	R		Value below reporting limit for parameter
# 2/25/2013	CHEM	S		Sample shipping time exceeded two day limit
# 2/25/2013	CHEM	H		Holding time of sample exceeded parameter-specific limit
#Conclusion: -----> INCLUDE

#For Nez Perce Tribe: No supporting info
#Conclusion: too much uncertainty to use ----> EXCLUDE
wqp.data <- wqp.data[wqp.data$OrganizationFormalName != 'Nez Perce Tribe',]

#For EPA Region 10 Superfund Portland Harbor Site: Qualifers and ResultComments provided, analysisstart date to calculate hold time
#Conclusion: INCLUDE (at this point but requires a finer screen)

#For Umatilla National Forest (Washington and Oregon): No supporting info
#Conclusion: too much uncertainty to use ----> EXCLUDE
wqp.data <- wqp.data[wqp.data$OrganizationFormalName != 'Umatilla National Forest (Washington and Oregon)',]

#For USGS Oregon Water Science Center: Qualifers and ResultComments provided, analysisstart date to calculate hold time, lab provided
#Conclusion: INCLUDE (at this point but requires a finer screen)

#For USGS Idaho Water Science Center: No comments or qualifiers, analysisstart date to calculate hold time, lab provided
#Conclusion: INCLUDE (at this point but requires a finer screen)

#For USGS Georgia Water Science Center: No comments or qualifiers, analysisstart date to calculate hold time, lab provided
#Conclusion: INCLUDE (at this point but requires a finer screen)



#### Secondary QC screen at the specific comment level ####
# i = 9
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'ResultCommentText'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'ResultValueTypeName'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'ResultLaboratoryCommentText'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'DetectionQuantitationLimitTypeName'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'AnalysisStartDate'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'PreparationStartDate'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'MeasureQualifierCode'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'LaboratoryName'])
# unique(wqp.data[wqp.data$OrganizationFormalName == unique(wqp.data$OrganizationFormalName)[i],'CharacteristicName'])

#Looks like I need to find out from Andrea if Inorganic phosphorus is applicable to our standard
# View(arrange(wqp.data[wqp.data$ResultCommentText %in% c("Analyte=Dissolved inorganic phosphorus (mg P/L); Detection limit= 0.3 ug/l",                                                     
#                                                  "Program: EMAP-West Coast; analtye: Dissolved inorganic phosphorus (mg P/L)",
#                                                  "Program: EMAP-West Coast ; equipment: YSI instrument; parameter: pH (pH units)",
#                                                  "Program: EMAP-West Coast; analtye: Dissolved inorganic phosphorus (mg P/L); MDL: 0.004 (mg P/L)",                                
#                                                  "Program: EMAP-West Coast; analtye: Dissolved inorganic phosphorus (mg P/L); MDL: 0.004 (mg P/L); QA code: Below reporting level",
#                                                  "Analyte: Dissolved inorganic phosphorus (mg P/L); Detection limit= 0.4367 ug/l",                                                
#                                                  "Analyte: Dissolved inorganic phosphorus (mg P/L); MDL: 0.000437 (mg P/L); Equipment: Kemmerer Bottle",                           
#                                                  "Analyte: Dissolved inorganic phosphorus (mg P/L); MDL: 0 (mg P/L); Equipment: Kemmerer Bottle"),],ResultCommentText))


# View(arrange(wqp.data[wqp.data$ResultCommentText %in% c( "sample arrived at room temp / Holding time: 271 days"                                 
#                                                 , "sample arrived at room temp / Holding time: 226 days"                                 
#                                                 , "sample arrived at room temp / Holding time: 22 days"                                  
#                                                 , "sample arrived at room temp / Holding time: 239 days"                                 
#                                                 , "sample arrived at room temp / Holding time: 7 days"                                   
#                                                 , "sample arrived at room temp / Holding time: 211 days"
#                                                 , "sample arrived at room temp / Holding time: 225 days"                                 
#                                                 , "sample arrived at room temp / Holding time: 210 days"                                 
#                                                 , "sample arrived at room temp / Holding time: 238 days"                                 
#                                                 , "sample arrived at room temp / Holding time: 270 days"                                 
#                                                 , "sample arrived at room temp / Holding time: 21 days"                                  
#                                                 , "sample arrived at room temp / Holding time: 6 days" 
#                                                 , "Ice melted.  Sample delivered cool, but not cold. / Holding time: 595 days"           
#                                                 , "Ice melted.  Sample delivered cool, but not cold. / Holding time: 5 days"             
#                                                 , "Ice melted.  Sample delivered cool, but not cold."                                    
#                                                 , "Ice melted.  Sample delivered cool, but not cold. / Holding time: 259 days"           
#                                                 , "Ice melted.  Sample delivered cool, but not cold. / Holding time: 41 days" ),],ResultCommentText))
#Data with improper thermal preservation still receives a qualifier of B per DEQ guidance
#Determination: INCLUDE

#There are some Tentatively Identified Compounds flagged using N (as seen at http://www.caslab.com/EPA-Data-Qualifiers/) that we 
#don't want to include
wqp.data <- wqp.data[-grep('[Pp]resumptive',wqp.data$ResultCommentText),]

# #These have potential for follow-up later
# View(arrange(wqp.data[wqp.data$ResultCommentText %in% c("algae bloom on upstream pond  high DO drives pH up",
#                                                         'Below normal reading, replaced old pH probe.',
#                                                         'can not read acuratly from bridge  /no acess to stream bank',
#                                                         'Cu contamination problem has been investigated but source not found.',
#                                                         "CWS Qualifier=X: Result is noted as unusual, but no identifiable reaon is found, and QC is okay.",
#                                                         'Discharging water from the dam, lowering the pH from Cherry Grove',
#                                                         "Disscharging water from the dam, lowering the pH from Cherry Grove",
#                                                         "Estimate all liquid TP samples, boiling stone contamination",
#                                                         "Estimate data due to blank contamination; Used blank from previous digest",
#                                                         "Estimate data, contaminated boiling stones",
#                                                         "heavy rains reading typical of stormwater",
#                                                         "sample was over range and I didn't catch it, AST",
#                                                         "void bottom of river 12.3 ft"),],ResultCommentText))

#Some data for USGS from CWS lab have qualifier indicating sample contamination. These will be excluded.
wqp.data <- wqp.data[-grep('Qualifier=C',wqp.data$ResultCommentText),]
wqp.data <- wqp.data[!wqp.data$ResultCommentText %in% c("Marked C due to possible contamination from the filter.",
                                                        "QCS FAILED; Filter type:Q",
                                                        "Sample marked as C due to possible contamination.",
                                                        "Sample marked C due to possible contamination from the filter."),]

#Some data for USGS from CWS lab have qualifier indicating major QC issue. These will be excluded.
wqp.data <- wqp.data[-grep('Qualifier=Q',wqp.data$ResultCommentText),]
wqp.data <- wqp.data[!wqp.data$ResultCommentText %in% c("Sample marked as Q due to trip blank. Note Rec < Sol"),]

#### Pare station list down to those with data remaining ####
wqp.stations <- wqp.stations[wqp.stations$MonitoringLocationIdentifier %in% wqp.data$MonitoringLocationIdentifier,]
#write.csv(wqp.stations, '//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/wqpstations_postQC_05192014.csv',row.names=FALSE)
#sqlSave(con, wqp.stations, tablename = 'WQPStations_postQC_05192014', rownames = FALSE)

#### Update success table with sample/station counts ####
# wqp.data$id <- paste(wqp.data$MonitoringLocationIdentifier, 
#                      wqp.data$ActivityStartDate, 
#                      wqp.data$ActivityStartTimeTime, 
#                      wqp.data$ActivityDepthHeightMeasureMeasureValue, 
#                      wqp.data$CharacteristicName)
# wqp.data.ph <- wqp.data[wqp.data$CharacteristicName == 'pH',]
# wqp.data.everything.else <- wqp.data[wqp.data$CharacteristicName != 'pH',]
# ph.grouped <- group_by(wqp.data.ph, id)
# ee.grouped <- group_by(wqp.data.everything.else, id)
# ee.dups.removed <- summarise(ee.grouped, ResultMeasureValue = max(ResultMeasureValue))
# ph.dups.removed <- summarise(ph.grouped, ResultMeasureValue = min(ResultMeasureValue))
# to.ref <- rbind(ee.dups.removed, ph.dups.removed)
# to.ref$relate <- paste(to.ref$id, to.ref$ResultMeasureValue)
# wqp.data$relate <- paste(wqp.data$id, wqp.data$ResultMeasureValue)
# wqp.data <- merge(wqp.data, to.ref[,'relate'], by = 'relate')
# wqp.data <- wqp.data[!duplicated(wqp.data$relate),]
# wqp.data <- within(wqp.data, rm('x', 'relate', 'id'))

#### Continuing on ####
#WQP detect/nondetect
# wqp.data$dn <- ifelse(wqp.data$MeasureQualifierCode %in% c('U','UJ') | wqp.data$ResultMeasureValue < ifelse(is.na(wqp.data$DetectionQuantitationLimitMeasureMeasureValue),
#                                                                                                             0,
#                                                                                                             wqp.data$DetectionQuantitationLimitMeasureMeasureValue), 0, 1)
# 
# table(wqp.data[wqp.data$dn == 0,'MeasureQualifierCode'])
# table(wqp.data$dn)

#Output the interim QC'd data to help keep working space/environment clean and make it quicker to re-run further analyses
# con <- odbcConnect('WQAssessment')
# sqlSave(con, wqp.data, tablename = 'WQPData_postQC_06122014', rownames = FALSE)
# odbcCloseAll()

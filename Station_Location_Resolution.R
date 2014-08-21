require(RODBC)
require(foreign)
require(plyr)

options(stringsAsFactors = FALSE, scipen=100)
# source('//deqhq1/wqassessment/2012_WQAssessment/Segmentation/R_scripts/01_2012 IR Segmentation.R')

# Pull in station data from draft summary, 2010, and 2012.
sul.draft<-stations.all
wqa<-odbcConnect('WQAssessment')
expression <- 'select COLUMN_NAME, DATA_TYPE from information_schema.columns where table_name = \'StationUseList\''
wqa.col.names<-sqlQuery(wqa, expression)
ars<-sqlFetch(wqa, 'Assessment_Report_Summary')
ar<-sqlFetch(wqa, 'Assessment_Record')
a<-sqlFetch(wqa, 'Assessment')
as<-sqlFetch(wqa, 'Assessment_Segment')
# Two stations were located in 2010 and again in the toxics redo (station 34086 & 35348). 
# I'm keeping what was done in the toxics redo and only using the sul from the toxics redo
sul.2012.table<-sqlQuery(wqa, query = 'Select * from StationUseList where Year_Added = 2012')
sul.2012<-rename(sul.2012.table[,c('STATION', 'STREAM_LLID', 'LAKE_LLID', 'RIVER_MILE')], c('STREAM_LLID'='STREAM_LLID_2012', 'LAKE_LLID' = 'LAKE_LLID_2012', 'RIVER_MILE'='RM_2012'))
sul.2010.table<-sqlQuery(wqa, query = 'Select * from StationUseList where Year_Added = 2010')
sul.2010<-rename(sul.2010.table[,c('STATION', 'STREAM_LLID', 'LAKE_LLID', 'RIVER_MILE')], c('STREAM_LLID'='STREAM_LLID_2010', 'LAKE_LLID' = 'LAKE_LLID_2010', 'RIVER_MILE'='RM_2010'))
sul.2010$STATION<-as.character(sul.2010$STATION)
sul.draft<-rename(sul.draft, c('Site_no'='STATION', 'Str_LLID'='STREAM_LLID_DRAFT', 'LAKE_LLID' = 'LAKE_LLID_DRAFT', 'Str_RM'='RM_DRAFT'))
odbcCloseAll()

### Split up draft stations into toxics and DO ###

# First, toxics
# duplicated.stations<-unique(sul.draft$STATION[duplicated(sul.draft$STATION)])
sul.draft.toxics<-sul.draft[sul.draft$Pollutant != 'Dissolved Oxygen',]
# duplicated.toxics.stations<-unique(sul.draft.toxics$STATION[duplicated(sul.draft.toxics$STATION)])
# sul.draft.toxics[sul.draft.toxics$STATION=='14211720',]
sul.draft.toxics$st.llid.rm<-paste(sul.draft.toxics$STATION, sul.draft.toxics$STREAM_LLID_DRAFT, sul.draft.toxics$LAKE_LLID_DRAFT, sul.draft.toxics$RM_DRAFT, sep='-')
# length(unique(sul.draft.toxics$STATION))
# length(unique(sul.draft.toxics$st.llid.rm))
# Why are these lengths different?
sul.draft.toxics.unique<-sul.draft.toxics[!duplicated(sul.draft.toxics$st.llid.rm),c('STATION', 'STREAM_LLID_DRAFT', 'LAKE_LLID_DRAFT', 'RM_DRAFT', 'st.llid.rm')]
# sul.draft.toxics.unique[sul.draft.toxics.unique$STATION %in% sul.draft.toxics.unique$STATION[duplicated(sul.draft.toxics.unique$STATION)],]
# sul.draft.toxics[sul.draft.toxics$STATION == '31873',c('STATION', 'RM_DRAFT', 'Pollutant')] #1.3 rm for Beryllium and Selenium. 1.4 rm for Chromnium, Copper, Nickel, and Zinc.
# sul.draft.toxics[sul.draft.toxics$STATION == '10896',] #1.3 rm for Iron
# These are duplicate statios, and rm's are not consistent. Check to see if this station is in DO and the current sul.

# Now DO
sul.draft.do<-sul.draft[sul.draft$Pollutant == 'Dissolved Oxygen',]
duplicated.do.stations<-unique(sul.draft.do$STATION[duplicated(sul.draft.do$STATION)])
# sul.draft.do[sul.draft.do$STATION=='10151',]
sul.draft.do$st.llid.rm<-paste(sul.draft.do$STATION, sul.draft.do$STREAM_LLID_DRAFT, sul.draft.do$LAKE_LLID_DRAFT, sul.draft.do$RM_DRAFT, sep='-')

# sul.draft.do[sul.draft.do$STATION %in% c('31873', '10896'),]
# sul.2012[sul.2012$STATION %in% c('31873', '10896'),]
#These rm designations are close, so I'm setting them to 1.4 for consistency
sul.draft.toxics$RM_DRAFT[sul.draft.toxics$STATION %in% c('31873', '10896')]<-1.4
sul.draft.do$RM_DRAFT[sul.draft.do$STATION %in% c('31873', '10896')]<-1.4
sul.draft.toxics$st.llid.rm<-paste(sul.draft.toxics$STATION, sul.draft.toxics$STREAM_LLID_DRAFT, sul.draft.toxics$LAKE_LLID_DRAFT, sul.draft.toxics$RM_DRAFT, sep='-')
sul.draft.toxics.unique<-sul.draft.toxics[!duplicated(sul.draft.toxics$st.llid.rm),c('STATION', 'STREAM_LLID_DRAFT', 'LAKE_LLID_DRAFT', 'RM_DRAFT', 'st.llid.rm')]
sul.draft.toxics.unique<-rename(sul.draft.toxics.unique, c('STREAM_LLID_DRAFT'='STREAM_LLID_toxics', 'LAKE_LLID_DRAFT'='LAKE_LLID_toxics', 'RM_DRAFT'='RM_toxics', 'st.llid.rm'='slr_toxics'))
sul.draft.do$st.llid.rm<-paste(sul.draft.do$STATION, sul.draft.do$STREAM_LLID_DRAFT, sul.draft.do$LAKE_LLID_DRAFT, sul.draft.do$RM_DRAFT, sep='-')
# sul.draft.do.unique<-sul.draft.do[!duplicated(sul.draft.do$st.llid.rm),c('STATION', 'STREAM_LLID_DRAFT', 'LAKE_LLID_DRAFT', 'RM_DRAFT', 'st.llid.rm')]
# sul.draft.do.unique<-rename(sul.draft.do.unique, c('STREAM_LLID_DRAFT'='STREAM_LLID_do', 'LAKE_LLID_DRAFT'='LAKE_LLID_do', 'RM_DRAFT'='RM_do', 'st.llid.rm'='slr_do'))

#I'm also keeping a record of updates as I go
updates<-data.frame(STATION=c('31873', '10896'), RM_OLD=c(1.38, 1.3), RM_NEW = c(1.4, 1.4), 
                    Comment=c('Duplicate station of 10896. Use rm 1.4 for consistency',
                              'Duplicate station of 31873. Use rm 1.4 for consistency'))
sus<-data.frame(STATION=unique(c(sul.draft$STATION, sul.2010$STATION, sul.2012$STATION)))
sus$draft.do<-ifelse(sus$STATION %in% sul.draft.do$STATION, 1, 0)
sus$draft.toxics<-ifelse(sus$STATION %in% sul.draft.toxics$STATION, 1, 0)
sus$ar2010<-ifelse(sus$STATION %in% sul.2010$STATION, 1, 0)
sus$ar2012<-ifelse(sus$STATION %in% sul.2012$STATION, 1, 0)
#View(sus[sus$ar2012==0 & sus$ar2010==0,])
draft.stations.unverified<-sus[sus$ar2012==0 & sus$ar2010==0,'STATION']

#These stations need to be verified. I'm writing out a csv which will then be used by a python script to locate them.
st.tdo<-sus[sus$ar2012==0 & sus$ar2010==0,]
# write.csv(st.tdo[1], 
#           'E:/GitHub/ToxicsRedo/StationsToLocate/Post_ToxicsRedo_Stations/toxics_do_unverified.csv', 
#           row.names=FALSE)
#I could only locate 51 stations in the LASAR shapefile. Pull in the attribute table to see which ones are missing
# st.tdo.shp<-read.dbf('E:/GitHub/ToxicsRedo/StationsToLocate/Post_ToxicsRedo_Stations/toxics_do_unverified.dbf')
# missing.stations<-st.tdo[which(!st.tdo$STATION %in% st.tdo.shp$STATION_KE),'STATION']
# st.lasar<-read.dbf('//Deqlead03/gis_wa/Project_Working_Folders/LASAR_Stations/LASAR_Stations/LASAR_Stations_26sept13.dbf')


# View(sus[sus$ar2012==1 & sus$ar2010==0,])
# View(sus[(sus$draft.do == 1 | sus$draft.toxics == 1) & (sus$ar2012 == 0 & sus$ar2010 == 0),])
# 
# length(unique(sul.draft.do$STATION))
# length(unique(sul.draft.do$st.llid.rm))

common.draft.stations<-sul.draft.toxics.unique$STATION[sul.draft.toxics.unique$STATION %in% sul.draft.do.unique$STATION]
sul.draft.overlap<-merge(sul.draft.toxics.unique[sul.draft.toxics.unique$STATION %in% common.draft.stations,], 
                         sul.draft.do.unique[sul.draft.do.unique$STATION %in% common.draft.stations,], by='STATION')
# all(sul.draft.overlap$slr_toxics == sul.draft.overlap$slr_do)
# Overlapping toxics and DO stations llid and rm do NOT AGREE

# Which stations have inconsistent RM or LLIDs?
sul.draft.overlap$e<-abs(as.numeric(sul.draft.overlap$RM_toxics) - as.numeric(sul.draft.overlap$RM_do))
# View(arrange(sul.draft.overlap[sul.draft.overlap$e>0,], desc(e)))
# View(sul.draft.overlap[which(sul.draft.overlap$slr_toxics != sul.draft.overlap$slr_do),])
# View(sul.draft.overlap[which(sul.draft.overlap$STREAM_LLID_toxics != sul.draft.overlap$STREAM_LLID_do),])
# View(sul.2012.table[sul.2012$STATION=='452637123142501',])
# View(ars[grep('14153500', ars$Summary),])
# View(ars[grep('452637123142501', ars$Summary),])
# View(ars[grep('14152500', ars$Summary),])
# View(ars[grep('453510122593301', ars$Summary),])
# View(ars[grep('14206419', ars$Summary),])
# View(ars[grep('33488', ars$Summary),])
# View(ars[ars$LLID_Stream_Lake=='1226500453377' & ars$Pollutant_ID == 2187,])
# View(sul.2012.table[sul.2012.table$STATION =='14152500',])
# View(sul.draft.overlap[sul.draft.overlap$slr_toxics != sul.draft.overlap$slr_do & sul.draft.overlap$STATION %in% sul.2012$STATION,])
# length(which(sul.draft.overlap$STATION %in% sul.2012$STATION))

stations.used<-unique(sul.draft['STATION'])
stations.used$Used<-sapply(stations.used$STATION, function(x) {ifelse(nrow(ars[grep(paste(' ', x, ' ', sep=''), ars$Summary),])>0,1,0)})
stations.not.used<-stations.used[stations.used$Used==0,'STATION']
# View(stations.used[stations.used$Used==0,])
# View(arrange(sul.draft[which(sul.draft$STATION %in% stations.used[stations.used$Used==0,'STATION']),], STATION))
# View(arrange(sul.draft[sul.draft$STATION %in% (stations.not.used[stations.not.used %in% draft.stations.unverified]),], STATION))

# Added 8/8/2014 to append 57 new stations
new.stations<-'E:/GitHub/ToxicsRedo/StationsToLocate/Post_ToxicsRedo_Stations/post_toxicsRedo_stations_final.dbf'
sul.pdt.table<-read.dbf(new.stations, as.is=TRUE)
sul.pdt.table$STATION<-as.character(sul.pdt.table$STATION)
sul.pdt.table<-rename(sul.pdt.table, c('QAQC1'='Init_Loc_Status', 'QAQC2'='Manual_Loc_Status', 'Comments'='Process_Comments', 'LLID'='STREAM_LLID',
                                       'DESCRIPTIO'='DESCRIPTION', 'GIS_Sour_1'='GIS_Source_LAKE', 'HU_6_Name'='HUC_3rd_Field',
                                       'GIS_STREAM'='Stream_Name'))
sul.pdt.table[,names(sul.2010.table)[which(!names(sul.2010.table) %in% names(sul.pdt.table))]]<-NA
sul.pdt.table$DATUM<-'Assumed NAD83'
sul.pdt.table$AGENCY<-'Oregon Department of Environmental Quality'
sul.pdt.table$AGENCY_ID<-'ODEQ'
sul.pdt.table$In_Routed_Dataset<-ifelse(sul.pdt.table$Manual_Loc_Status == 'Potential Digitization', 0, 1)
sul.pdt.table$USE_Final<-ifelse(sul.pdt.table$Manual_Loc_Status == 'Correct', 1, 0)
sul.pdt.table$Year_Added<-ifelse(sul.pdt.table$STATION %in% stations.not.used, '2014', '2012')
# None on tribal lands
sul.pdt.table$TRIBAL<-0
# None are estuary or marine
sul.pdt.table$ESTUARY<-0
sul.pdt.table$MARINE_WATERS=0
sul.pdt.table$Water_Type<-'FW'
sul.pdt.append<-sul.pdt.table[,names(sul.pdt.table) %in% wqa.col.names[,1]]
index.start<-(max(c(sul.2012.table$StationUse_RecordID, sul.2010.table$StationUse_RecordID))+1)
sul.pdt.append$StationUse_RecordID<-index.start:(index.start+56)
sul.pdt.dtypes<-data.frame(Field=names(sul.pdt.append), dtype=unlist(lapply(sul.pdt.append, class)))
dtype.merge<-merge(wqa.col.names, sul.pdt.dtypes, by.x='COLUMN_NAME', by.y='Field')
var.dtypes <- dtype.merge$DATA_TYPE
names(var.dtypes)<- dtype.merge$COLUMN_NAME

#appending didn't work. create master table here, and save
sul.final<-rbind(sul.2010.table, sul.2012.table, sul.pdt.append)
sul.final<-within(sul.final, rm(rownames))
sul.final$RIVER_MILE<-round(as.numeric(sul.final$RIVER_MILE), 2)
sul.final$DEC_LAT<-as.character(round(as.numeric(sul.final$DEC_LAT), 8))
sul.final$DEC_LONG<-as.character(round(as.numeric(sul.final$DEC_LONG), 8))
# wqa<-odbcConnect('WQAssessment')
# sqlSave(wqa, sul.final, tablename = 'StationUseList_08122014', rownames = FALSE)
# sul.check<-sqlFetch(wqa, sqtable = 'StationUseList_08122014')
# sqlDrop(wqa, sqtable = 'StationUseList_08122014')
# odbcCloseAll()



sul.pdt<-rename(sul.pdt.table, c('RIVER_MILE'='RM'))
sul.compare<-merge(sul.draft.do.unique, 
                   merge(sul.draft.toxics.unique,
                         merge(sul.2010,
                               merge(sul.2012, sul.pdt[,c('STATION', 'STREAM_LLID', 'LAKE_LLID', 'RM')], 'STATION', all=TRUE), 
                               'STATION', all=TRUE), 
                         'STATION', all=TRUE), 
                   'STATION', all=TRUE)
sul.compare<-sul.compare[,-grep('slr', names(sul.compare))]
sul.compare[,grep('LLID', names(sul.compare))]<-apply(sul.compare[,grep('LLID', names(sul.compare))], 2, as.character)
sul.compare[,grep('RM', names(sul.compare))]<-apply(sul.compare[,grep('RM', names(sul.compare))], 2, as.numeric)
sul.compare[,grep('RM', names(sul.compare))]<-apply(sul.compare[,grep('RM', names(sul.compare))], 2, round, 1)
sul.compare$e.max<-apply(sul.compare[,grep('RM', names(sul.compare))], 1, max, na.rm=TRUE)-apply(sul.compare[,grep('RM', names(sul.compare))], 1, min, na.rm=TRUE)
View(arrange(sul.compare[sul.compare$e.max>0,c(names(sul.compare)[names(sul.compare) != 'STATION'], 'STATION')], desc(e.max)))
View(arrange(ars[grep(' 35916 ', ars$Summary),], RM1))
# Are there any inconsistencies in which stations should be excluded?
use.but.do.not.use<-stations.not.used[which(stations.not.used %in% c(sul.pdt$STATION[sul.pdt$QAQC2 != 'Remove'], 
                                                  sul.2010.table$STATION[sul.2010.table$USE_Final == 1], sul.2012.table$STATION[sul.2012.table$USE_Final == 1]))]
sul.compare$Final.Comment[which(sul.compare$STATION %in% c(sul.pdt$STATION[sul.pdt$QAQC2 == 'Remove'], sul.2010.table$STATION[sul.2010.table$USE_Final == 0],
                                               sul.2012.table$STATION[sul.2012.table$USE_Final == 0]))]<-'Not Used in Analysis'
sul.compare$Final.RM<-sul.compare$RM_2010
sul.compare$Final.RM[is.na(sul.compare$RM_2010)]<-sul.compare$RM_2012[is.na(sul.compare$RM_2010)]
sul.compare$Final.RM[is.na(sul.compare$Final.RM)]<-sul.compare$RM[is.na(sul.compare$Final.RM)]


sul.compare$Final.RM[sul.compare$STATION == '452637123142501']<-'69.3'
sul.compare$Final.RM[sul.compare$STATION == '14152500']<-'34.8'
sul.compare$Final.RM[sul.compare$STATION == '14153500']<-'28.1'
sul.compare$Final.RM[sul.compare$STATION == '33184']<-'20.5'
sul.compare$Final.RM[sul.compare$STATION == '10768']<-'253.2'
sul.compare$Final.RM[sul.compare$STATION == '10765']<-'234.7'
sul.compare$Final.RM[sul.compare$STATION == '32404']<-'20.6'



sul.draft.compare<-merge(sul.draft, post.toxicsRedo.stations[,c('STATION', 'RIVER_MILE', 'QAQC2')])
View(sul.draft.compare[which(sul.draft.compare$STATION %in% post.toxicsRedo.stations$STATION & sul.draft.compare$QAQC2 != 'Remove'),c('STATION', 'RM_DRAFT', 'RIVER_MILE', 'QAQC2')])

require(RODBC)
require(rgdal)
require(plyr)
options(stringsAsFactors=F)
options('scipen'=100)

#Import all necessary datasets, synchronize column names with wqa dbase column names.

#Primary station list
inshape<-'E:/GitHub/ToxicsRedo/Shapefiles_for_Access'
station.shp<-readOGR(inshape, 'All_stations_final_est_pd')
stations<-station.shp@data
#Rename primary station columns
names(stations)
stations<-rename(stations, c('LLID'='STREAM_LLID', 'DESCRIPTIO'='DESCRIPTION', 'GIS_STREAM'='Stream_Name', 
                             'GIS_Sour_1'='GIS_Source_LAKE', 'HU_6_Name'='HUC_3rd_Field', 'Sal_ppth'='Salinity_Max',
                             'QAQC1'='Init_Loc_Status', 'QAQC2'='Manual_Loc_Status', 'Comments'='Process_Comments',
                             'Est_Commen'='Comments', 'MATRIX'='Water_Type'))

#Station error fix
cr.fix<-readOGR(inshape, 'Station_error_fix')
cr.data<-cr.fix@data
cr.data<-rename(cr.data, c('LLID'='STREAM_LLID', 'DESCRIPTIO'='DESCRIPTION', 'GIS_STREAM'='Stream_Name', 
                             'GIS_Sour_1'='GIS_Source_LAKE', 'HU_6_Name'='HUC_3rd_Field', 'QAQC1'='Init_Loc_Status', 
                             'QAQC2'='Manual_Loc_Status', 'Comments'='Process_Comments'))
# View(stations[stations$STATION %in% cr.data$STATION,sort(names(stations)[names(stations) %in% names(cr.data)])])
# View(cr.data[,sort(names(cr.data))])
stations[stations$STATION %in% cr.data$STATION,sort(names(stations)[names(stations) %in% names(cr.data)])]<-cr.data[,sort(names(cr.data))]


#Add new routed dataset field
stations$WB_Added[is.na(stations$WB_Added)]<-'No review needed'
stations$In_Routed_Dataset[stations$WB_Added !='Remove'] <- 1
stations$In_Routed_Dataset[stations$WB_Added =='Remove'] <- 0

#correct GIS_Source and GIS_Source_LAKE
wb.added<-unique(stations$WB_Added)
stations$GIS_Source[stations$WB_Added %in% wb.added[c(3,6)]]<-"PNW Hydrography 24K"
stations$GIS_Source_LAKE[stations$WB_Added %in% wb.added[c(5,6)]]<-"PNW Hydrography 24K"
stations$STREAM_LLID[stations$STATION == 'NLA06608-0402']<-NA  #It was addressed to LLID: 1241417430803
stations$GIS_Source[stations$STATION == 'NLA06608-0402']<-NA

#Add Karla's Usability comments
accdb<-'//deqhq1/WQAssessment/2012_WQAssessment/2012_WorkingTables.mdb'
con<-odbcConnectAccess2007(accdb)
karla.usability<-sqlFetch(con, 'All_stations_final_est')
odbcCloseAll()
stations<-join(stations, karla.usability[,c('STATION', 'UsabilityComments')], 'STATION', match='first')

#Remove unnecessary fields
remove.fields<-c('HUC_6', 'HUC_8', 'Estuary_20', 'Est_Sal', 'Est_Final', 'WB_Comment', 'WB_Added')
stations<-stations[,!names(stations) %in% remove.fields]

#Remove duplicate stations. There are only three. I'm keeping the ones that were classified 'correct' in the 
#initial QAQC process
dups<-stations[stations$STATION %in% stations[duplicated(stations$STATION),'STATION'],]
ds<-unique(dups[order(dups$STATION),'STATION'])
dstations<-stations[stations$STATION %in% ds,]
dstations.o<-dstations[order(dstations$STATION),]
stations<-stations[!((stations$STATION %in% ds) & (stations$Init_Loc_Status !='Correct')),]

#Designate final use
stations$USE_Final<-NA
stations$USE_Final[(stations$Manual_Loc_Status != 'Remove' & stations$In_Routed_Dataset == 1 & stations$TRIBAL == 0)]<-1
stations$USE_Final[!(stations$Manual_Loc_Status != 'Remove' & stations$In_Routed_Dataset == 1 & stations$TRIBAL == 0)]<-0
stations$USE_Final[grep('Don\'t use', stations$UsabilityComments)] <- 0
unique(stations$USE_Final)

#Gresham Station list

#Edit DATUM column so it's ready for csv_to_shapefile conversion
# merloc<-read.csv('//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/StationstoLocate/mercury_fish_tissue_stations_07142014.csv',
#                  header=T)
# merloc$DATUM2<-gsub("Assumed ", "", merloc$DATUM)
# merloc$DATUM2<-gsub(" ", "", merloc$DATUM2)
# write.csv(merloc, '//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/StationstoLocate/mercury_fish_tissue_stations_07162014.csv',
#           row.names=F)

inshape<-'E:/GitHub/ToxicsRedo/Shapefiles_for_Access'
station.shp<-readOGR(inshape, 'Gresham_Stations')
grest<-station.shp@data
grest<-rename(grest, c('RID'='STREAM_LLID', 'MEAS'='RIVER_FOOT', 'SITE_DESCR'='DESCRIPTION', 'LATITUDE_D'='DEC_LAT',
                       'LONGITUDE_'='DEC_LONG', 'STATION_ID'='STATION', 'GIS_Sour_1'='GIS_Source_LAKE',
                       'HU_6_Name'='HUC_3rd_Field', 'GIS_STREAM'='Stream_Name', 'QAQC1'='Init_Loc_Status', 
                       'QAQC2'='Manual_Loc_Status'))
grest$RIVER_MILE<-round(grest$RIVER_FOOT/5280, 3)
grest$Water_Type<-'FW'
grest$Salinity_Max<-NA
grest$TRIBAL<-0
grest$AGENCY<-'City of Gresham'
grest$AGENCY_ID<-'GRESHAM'
grest$DATUM<-sub(' ', '', grest$LAT_LONG_D)
fields.discard <- names(grest)[!names(grest) %in% names(stations)]
grest<-grest[,!names(grest) %in% fields.discard]
new.fields<-names(stations)[!names(stations) %in% names(grest)]
grest[,new.fields]<-NA
grest$USE_Final<-1
unique(grest$USE_Final)

#Mercury to locate list
inshape<-'E:/GitHub/ToxicsRedo/Shapefiles_for_Access'
station.shp<-readOGR(inshape, 'Mercury_Stations')
merst<-station.shp@data
merst<-rename(merst, c('STATION_ID'='STATION', 'LOCATION_D'='DESCRIPTION', 'Latitude'='DEC_LAT', 
                       'Longitude'='DEC_LONG', 'RID'='STREAM_LLID', 'GIS_Sour_1'='GIS_Source_LAKE',
                       'MEAS'='RIVER_FOOT', 'Matrix'='Water_Type', 'HU_6_Name'='HUC_3rd_Field', 'GIS_STREAM'='Stream_Name', 
                       'QAQC1'='Init_Loc_Status', 'QAQC2'='Manual_Loc_Status'))
merst$RIVER_MILE<-round(merst$RIVER_FOOT/5280, 3)
merst$Salinity_Max<-NA
merst$TRIBAL<-0
merst$AGENCY<-'Oregon Department of Environmental Quality'
merst$AGENCY_ID<-'ODEQ'
names(merst)[!names(merst) %in% names(stations)]
fields.discard <- names(merst)[!names(merst) %in% names(stations)]
merst<-merst[,!names(merst) %in% fields.discard]
new.fields<-names(stations)[!names(stations) %in% names(merst)]
merst[,new.fields]<-NA
merst$USE_Final[merst$Manual_Loc_Status != 'Remove']<-1
merst$USE_Final[merst$Manual_Loc_Status == 'Remove']<-0
unique(merst$USE_Final)

#Mercury not to locate list
merlist<-read.csv('//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/StationstoLocate/stations_to_add_but_not_locate_07142014.csv',
                  header=T)
merlist$'STREAM_LLID'<-substr(merlist$'LLID_Stream_Lake', 1, 13)
merlist$'LAKE_LLID'<-substr(merlist$'LLID_Stream_Lake', 15, 28)
merlist$'Stream_Name'<-unlist(lapply(strsplit(merlist$Stream_Lake_Name, '/'), function(z){z[1]}))
merlist$'LAKE_NAME'<-unlist(lapply(strsplit(merlist$Stream_Lake_Name, '/'), function(z){z[2]}))
merlist<-rename(merlist, c('STATION_KEY'='STATION', 'LOCATION_DESCRIPTION'='DESCRIPTION', 'Latitude'='DEC_LAT',
                           'Longitude'='DEC_LONG', 'Str_RM'='RIVER_MILE'))
merlist$Water_Type<-'FW'
merlist[,c('HUC_3rd_Field', 'Salinity_Max', 'GIS_Source', 'GIS_Source_LAKE')]<-NA
merlist$TRIBAL<-0
merlist$AGENCY<-'Oregon Department of Environmental Quality'
merlist$AGENCY_ID<-'ODEQ'
names(merlist)[!names(merlist) %in% names(stations)]
fields.discard <- names(merlist)[!names(merlist) %in% names(stations)]
merlist<-merlist[,!names(merlist) %in% fields.discard]
new.fields<-names(stations)[!names(stations) %in% names(merlist)]
merlist[,new.fields]<-NA
exclude<-c(23485,24575) #They do not have lat longs associated with them
merlist$USE_Final[!merlist$STATION %in% exclude]<-1
merlist$USE_Final[merlist$STATION %in% exclude]<-0
unique(merlist$USE_Final)

#These need to be mapped, so create a shapefile with them
# merlist_to_map<-SpatialPointsDataFrame(coords = merlist[!is.na(merlist$DEC_LAT), c('DEC_LONG', 'DEC_LAT')],
#                                        proj4string = CRS("+proj=longlat +datum=NAD83"), 
#                                        data = merlist[!is.na(merlist$DEC_LAT),], coords.nrs=c(7,6), 
#                                        match.ID = 'STATION')
# writeOGR(merlist_to_map, 'E:/GitHub/ToxicsRedo/Shapefiles_for_Access', 'merlist_to_map', driver="ESRI Shapefile")

#MORE USGS Station list
inshape<-'E:/GitHub/ToxicsRedo/Shapefiles_for_Access'
station.shp<-readOGR(inshape, 'MORE_Stations')
usgs<-station.shp@data
#Rename MORE USGS column names
usgs$RIVER_MILE<-round(usgs$MEAS/5820, 3)
usgs$Water_Type<-'FW'
usgs$LAKE_LLID<-NA
usgs<-rename(usgs, c('RID'='STREAM_LLID', 'MEAS'='RIVER_FOOT', 'Monitori_1'='DESCRIPTION', 'GIS_Sour_1'='GIS_Source_LAKE',
                         'LatitudeMe'='DEC_LAT', 'LongitudeM'='DEC_LONG', 'site_only'='STATION', 'HU_6_Name'='HUC_3rd_Field',
                         'GIS_STREAM'='Stream_Name', 'QAQC1'='Init_Loc_Status', 'QAQC2'='Manual_Loc_Status',
                         'Horizont_3'='DATUM'))
usgs$Salinity_Max<-NA
usgs$TRIBAL<-0
usgs$AGENCY<-'United States Geological Survey'
usgs$AGENCY_ID<-'USGS'
fields.discard <- names(usgs)[!names(usgs) %in% names(stations)]
usgs<-usgs[,!names(usgs) %in% fields.discard]
new.fields<-names(stations)[!names(stations) %in% names(usgs)]
usgs[,new.fields]<-NA
usgs$USE_Final[usgs$Manual_Loc_Status != 'Remove']<-1
usgs$USE_Final[usgs$Manual_Loc_Status == 'Remove']<-0
unique(usgs$USE_Final)

#Accessing WQAssessment db
wqadb<-odbcConnect('WQAssessment')
wqadbTables<-sqlTables(wqadb, tableType="Table")
stUseTable<-wqadbTables$TABLE_NAME[grep('stationuse', wqadbTables$TABLE_NAME, ignore.case=T)]
stUseList<-sqlFetch(wqadb, "StationUseList")
odbcCloseAll()
stUseList$Water_Type[stUseList$MARINE_WATERS==1]<-'SW'
stUseList$Water_Type[stUseList$ESTUARY==1]<-'ES'
stUseList$Water_Type[is.na(stUseList$Water_Type)]<-'FW'
stUseList$TRIBAL<-0
#Remove stations on tribal lands from 2010 stations
#I did a select by location query in ArcGIS, and copied the station numbers manually
tribal2010<-c(10406,23106,13282,12023,23103,23102,12005,23096,24058,24044,24066,24437)
stUseList$TRIBAL[stUseList$STATION %in% as.character(tribal2010)]<-1
stUseList$AGENCY<-'Oregon Department of Environmental Quality'
stUseList$AGENCY_ID<-'ODEQ'
stUseList[stUseList=='NA']<-NA
new.fields<-names(stations)[!names(stations) %in% names(stUseList)]
stUseList[,new.fields]<-NA
stUseList$USE_Final[stUseList$STATION %in% as.character(tribal2010)]<-0
unique(stUseList$USE_Final)

#Create an empty df for station additions with the same columns as the wqa dbase
stAppend<-stUseList[FALSE,] 
morecols<-names(stAppend)[!names(stAppend) %in% names(stations)]
stations[,morecols]<-NA
grest[,morecols]<-NA
merst[,morecols]<-NA
merlist[,morecols]<-NA
usgs[,morecols]<-NA

stAppend<-rbind(stAppend, 
                rbind(grest[,names(stUseList)],
                      rbind(merst[,names(stUseList)],
                            rbind(merlist[,names(stUseList)], 
                                  rbind(stations[,names(stUseList)], usgs[,names(stUseList)])))))

stAppend$ESTUARY[stAppend$Water_Type=='ES' & !is.na(stAppend$Water_Type)]<-1
stAppend$MARINE_WATERS[stAppend$Water_Type=='SW' & !is.na(stAppend$Water_Type)]<-1
stAppend[stAppend=='NA']<-NA

#Merge all together
stUseList2012<-rbind(stAppend, stUseList)

#Merge the two LASAR duplicates and rebuild sul. 
sul_dups<-stUseList2012[stUseList2012$STATION %in% stUseList2012[duplicated(stUseList2012$STATION),'STATION'],]
ds<-unique(sul_dups[order(sul_dups$STATION),'STATION'])
stAppend[stAppend$STATION==ds[1],is.na(stAppend[stAppend$STATION==ds[1],])]<-stUseList[stUseList$STATION==ds[1],is.na(stAppend[stAppend$STATION==ds[1],])]
stAppend[stAppend$STATION==ds[2],is.na(stAppend[stAppend$STATION==ds[2],])]<-stUseList[stUseList$STATION==ds[2],is.na(stAppend[stAppend$STATION==ds[2],])]
stUseList<-stUseList[!stUseList$STATION %in% ds,]

#But first label 2012 stations
stAppend$Year_Added<-2012
stUseList$Year_Added<-2010

#And assign new StationUse_RecordID
old.record.id.max<-max(stUseList$StationUse_RecordID)
new.record.id.start<-11093
new.record.id.length.out<-nrow(stAppend)-1
new.record.id.end<-12394
new.record.ids<-seq(new.record.id.start,new.record.id.end)
stAppend$StationUse_RecordID<-new.record.ids

stUseList2012<-rbind(stAppend, stUseList)

stUseList2012$RIVER_MILE<-as.numeric(stUseList2012$RIVER_MILE) #Convert 'RIVER_MILE' back to numeric
stUseList2012$LAKE_LLID[stUseList2012$LAKE_LLID==0 & !is.na(stUseList2012$LAKE_LLID)]<-NA 
stUseList2012$RIVER_MILE[is.na(stUseList2012$STREAM_LLID) | stUseList2012$STREAM_LLID==0]<-NA
stUseList2012$Stream_Name[!is.na(stUseList2012$STREAM_LLID) & is.na(stUseList2012$Stream_Name)]<-'Unnamed Stream'
stUseList2012$LAKE_NAME[!is.na(stUseList2012$LAKE_LLID) & is.na(stUseList2012$LAKE_NAME)]<-'Unnamed Lake'
sul2012<-stUseList2012[stUseList2012$USE_Final==1,]


sul2012$STATION[sul2012$STATION %in% cr.data$STATION]
View(stUseList2012[stUseList2012$STATION %in% cr.data$STATION,])
View(stUseList2012[order(stUseList2012$RIVER_MILE),c('Year_Added', 'STATION', 'DESCRIPTION', 'STREAM_LLID', 'LAKE_LLID', 'RIVER_MILE')])
View(sul2012[order(sul2012$RIVER_MILE),c('Year_Added', 'STATION', 'DESCRIPTION', 'STREAM_LLID', 'LAKE_LLID', 'RIVER_MILE')])
View(stations[order(stations$RIVER_MILE),c('STATION', 'DESCRIPTION', 'STREAM_LLID', 'LAKE_LLID', 'RIVER_MILE')])

#Final use tables csv
write.csv(stUseList2012, 'I:/2012_WQAssessment/ToxicsRedo/StationsToLocate/stUseList2012.csv',
          row.names=F)
write.csv(sul2012, 'I:/2012_WQAssessment/ToxicsRedo/StationsToLocate/stUseList2012_Final.csv',
          row.names=F)
wqa<-odbcConnect('WQAssessment')
sqlSave(wqadb, stUseList2012, tablename='StationUseList_2012', rownames=FALSE)
odbcCloseAll()

wqa<-odbcConnect('WQAssessment')
test<-sul[sul$STATION=='36228',c('STATION', 'STREAM_LLID')]
sqlUpdate(wqa, test, 'StationUseList_2012','STATION')
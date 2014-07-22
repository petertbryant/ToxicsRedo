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
# [1] "OBJECTID"   "AGENCY"     "AGENCY_ID"  "STATION"    "DEC_LAT"    "DEC_LONG"   "DESCRIPTIO" "QAQC1"     
# [9] "QAQC2"      "Comments"   "RIVER_MILE" "HUC_6"      "HU_6_Name"  "HUC_8"      "HU_8_Name"  "GIS_STREAM"
# [17] "LAKE_NAME"  "GIS_Source" "GIS_Sour_1" "Estuary_20" "Sal_ppth"   "Est_Sal"    "Est_Final"  "Est_Commen"
# [25] "MATRIX"     "WB_Added"   "WB_Comment" "LLID"       "LAKE_LLID" 
stations<-rename(stations, c('LLID'='STREAM_LLID', 'DESCRIPTIO'='DESCRIPTION', 'GIS_STREAM'='Stream_Name', 
                             'GIS_Sour_1'='GIS_Source_LAKE', 'HU_6_Name'='HUC_3rd_Field', 'Sal_ppth'='Salinity_Max'))
stations$WB_Added[is.na(stations$WB_Added)]<-'No review needed'

#Designate final use
stations$USE_Final<-NA
stations$USE_Final[(stations$QAQC2 != 'Remove' & stations$WB_Added != 'Remove' & stations$TRIBAL == 0)]<-1
stations$USE_Final[!(stations$QAQC2 != 'Remove' & stations$WB_Added != 'Remove' & stations$TRIBAL == 0)]<-0

#Remove duplicate stations. There are only three. I'm keeping the ones that were classified 'correct' in the 
#initial QAQC process
dups<-stations[stations$STATION %in% stations[duplicated(stations$STATION),'STATION'],]
ds<-unique(dups[order(dups$STATION),'STATION'])
dstations<-stations[stations$STATION %in% ds,]
dstations.o<-dstations[order(dstations$STATION),]
stations<-stations[!((stations$STATION %in% ds) & (stations$QAQC1 !='Correct')),]

#correct GIS_Source and GIS_Source_LAKE
wb.added<-unique(stations$WB_Added)
# [1] "No review needed"                        "No Change"                              
# [3] "Added PNW24 WC, LLID, and RM"            "Remove"                                 
# [5] "Added PNW24 WB, and Lake LLID"           "Added PNW24 WB, LLID, RM, and Lake LLID"
# [7] "Added Lake LLID"                         "Added LLID, and RM"
#stations[stations$WB_Added ==wb.added[3],]
stations$GIS_Source[stations$WB_Added %in% wb.added[c(3,6)]]<-"PNW Hydrography 24K"
stations$GIS_Source_LAKE[stations$WB_Added %in% wb.added[c(5,6)]]<-"PNW Hydrography 24K"

View(stations[stations$WB_Added ==wb.added[7],]) #Only one change is needed
stations$STREAM_LLID[stations$STATION == 'NLA06608-0402']<-NA  #It was addressed to LLID: 1241417430803
stations$GIS_Source[stations$STATION == 'NLA06608-0402']<-NA
View(stations[stations$WB_Added ==wb.added[8],])#These are already correct

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
grest$USE_Final<-1

#Rename Gresham column names
names(grest)
# [1] "RID"        "MEAS"       "SITE_DESCR" "LATITUDE_D" "LONGITUDE_" "LAT_LONG_D" "STATION_ID" "QAQC1"     
# [9] "QAQC2"      "HUC_6"      "HU_6_Name"  "HUC_8"      "HU_8_Name"  "LAKE_LLID"  "GIS_STREAM" "LAKE_NAME" 
# [17] "GIS_Source" "GIS_Sour_1" "USE_Final" 

grest<-rename(grest, c('RID'='STREAM_LLID', 'MEAS'='RIVER_FOOT', 'SITE_DESCR'='DESCRIPTION', 'LATITUDE_D'='DEC_LAT',
                       'LONGITUDE_'='DEC_LONG', 'STATION_ID'='STATION', 'GIS_Sour_1'='GIS_Source_LAKE',
                       'HU_6_Name'='HUC_3rd_Field', 'GIS_STREAM'='Stream_Name'))
grest$RIVER_MILE<-round(grest$RIVER_FOOT/5280, 3)
grest$MATRIX<-'FW'
grest$Salinity_Max<-NA
grest$TRIBAL<-0
grest$AGENCY<-'City of Gresham'
grest$AGENCY_ID<-'GRESHAM'

#Mercury to locate list
inshape<-'E:/GitHub/ToxicsRedo/Shapefiles_for_Access'
station.shp<-readOGR(inshape, 'Mercury_Stations')
merst<-station.shp@data

#Rename Mercury column names
names(merst)
# [1] "LOCATION_D" "Latitude"   "Longitude"  "DATUM"      "MEAS"       "RID"        "Matrix"     "STATION_ID" "QAQC1"      "QAQC2"     
# [11] "LAKE_LLID"  "HUC_6"      "HU_6_Name"  "HUC_8"      "HU_8_Name"  "GIS_STREAM" "LAKE_NAME"  "GIS_Source" "GIS_Sour_1"
merst<-rename(merst, c('STATION_ID'='STATION', 'LOCATION_D'='DESCRIPTION', 'Latitude'='DEC_LAT', 
                       'Longitude'='DEC_LONG', 'RID'='STREAM_LLID', 'GIS_Sour_1'='GIS_Source_LAKE',
                       'MEAS'='RIVER_FOOT', 'Matrix'='MATRIX', 'HU_6_Name'='HUC_3rd_Field', 'GIS_STREAM'='Stream_Name'))
merst$RIVER_MILE<-round(merst$RIVER_FOOT/5280, 3)
merst$USE_Final<-NA
merst$USE_Final[merst$QAQC2 != 'Remove']<-1
merst$USE_Final[merst$QAQC2 == 'Remove']<-0
merst$Salinity_Max<-NA
merst$TRIBAL<-0
merst$AGENCY<-'Oregon Department of Environmental Quality'
merst$AGENCY_ID<-'ODEQ'

#Mercury not to locate list
merlist<-read.csv('//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/StationstoLocate/stations_to_add_but_not_locate_07142014.csv',
                  header=T)
names(merlist)
# [1] "STATION_KEY"          "LOCATION_DESCRIPTION" "LLID_Stream_Lake"     "Stream_Lake_Name"     "Str_RM"              
# [6] "Latitude"             "Longitude"            "DATUM"
merlist$'STREAM_LLID'<-substr(merlist$'LLID_Stream_Lake', 1, 13)
merlist$'LAKE_LLID'<-substr(merlist$'LLID_Stream_Lake', 15, 28)
merlist$'Stream_Name'<-unlist(lapply(strsplit(merlist$Stream_Lake_Name, '/'), function(z){z[1]}))
merlist$'LAKE_NAME'<-unlist(lapply(strsplit(merlist$Stream_Lake_Name, '/'), function(z){z[2]}))
merlist<-rename(merlist, c('STATION_KEY'='STATION', 'LOCATION_DESCRIPTION'='DESCRIPTION', 'Latitude'='DEC_LAT',
                           'Longitude'='DEC_LONG', 'Str_RM'='RIVER_MILE'))
merlist$MATRIX<-'FW'
exclude<-c(23485,24575)
merlist$USE_Final[!merlist$STATION %in% exclude]<-1
merlist$USE_Final[merlist$STATION %in% exclude]<-0
merlist[,c('HUC_3rd_Field', 'Salinity_Max', 'GIS_Source', 'GIS_Source_LAKE')]<-NA
merlist$TRIBAL<-0
merlist$AGENCY<-'Oregon Department of Environmental Quality'
merlist$AGENCY_ID<-'ODEQ'

#These need to be mapped, so create a shapefile with them
merlist_to_map<-SpatialPointsDataFrame(coords = merlist[!is.na(merlist$DEC_LAT), c('DEC_LONG', 'DEC_LAT')],
                                       proj4string = CRS("+proj=longlat +datum=NAD83"), 
                                       data = merlist[!is.na(merlist$DEC_LAT),], coords.nrs=c(7,6), 
                                       match.ID = 'STATION')
writeOGR(merlist_to_map, 'E:/GitHub/ToxicsRedo/Shapefiles_for_Access', 'merlist_to_map', driver="ESRI Shapefile")


#MORE USGS Station list
inshape<-'E:/GitHub/ToxicsRedo/Shapefiles_for_Access'
station.shp<-readOGR(inshape, 'MORE_Stations')
usgsst<-station.shp@data

#Rename MORE USGS column names
names(usgsst)
# [1] "RID"        "MEAS"       "Monitori_1" "LatitudeMe" "LongitudeM" "Horizont_3" "site_only"  "QAQC1"      "QAQC2"      "Matrix"    
# [11] "HUC_6"      "HU_6_Name"  "HUC_8"      "HU_8_Name"  "LAKE_LLID"  "GIS_STREAM" "LAKE_NAME"  "GIS_Source" "GIS_Sour_1"
usgsst$RIVER_MILE<-round(usgsst$MEAS/5820, 3)
usgsst$MATRIX<-'FW'
usgsst$LAKE_LLID<-NA
usgsst$USE_Final[usgsst$QAQC2 != 'Remove']<-1
usgsst$USE_Final[usgsst$QAQC2 == 'Remove']<-0
usgsst<-rename(usgsst, c('RID'='STREAM_LLID', 'MEAS'='RIVER_FOOT', 'Monitori_1'='DESCRIPTION', 'GIS_Sour_1'='GIS_Source_LAKE',
                         'LatitudeMe'='DEC_LAT', 'LongitudeM'='DEC_LONG', 'site_only'='STATION', 'HU_6_Name'='HUC_3rd_Field',
                         'GIS_STREAM'='Stream_Name'))
usgsst$Salinity_Max<-NA
usgsst$TRIBAL<-0
usgsst$AGENCY<-'United States Geological Survey'
usgsst$AGENCY_ID<-'USGS'

#Update USE_Final based on Karla's Usability comments
#***These comments need to be included in the final df***
accdb<-'//deqhq1/WQAssessment/2012_WQAssessment/2012_WorkingTables.mdb'
con<-odbcConnectAccess2007(accdb)
karla.usability<-sqlFetch(con, 'All_stations_final_est')
odbcCloseAll()
stations$USE_Final[stations$STATION %in% karla.usability$STATION[grep('Don\'t use', karla.usability$UsabilityComments)]] <- 0


#Accessing WQAssessment db
wqadb<-odbcConnect('WQAssessment')
wqadbTables<-sqlTables(wqadb, tableType="Table")
stUseTable<-wqadbTables$TABLE_NAME[grep('stationuse', wqadbTables$TABLE_NAME, ignore.case=T)]
stUseList<-sqlFetch(wqadb, stUseTable)
#dtype<-sqlQuery(wqadb, 'select * from StationUseList.information_schema.columns;')
stUseList$MATRIX[stUseList$MARINE_WATERS==1]<-'SW'
stUseList$MATRIX[stUseList$ESTUARY==1]<-'ES'
stUseList$MATRIX[is.na(stUseList$MATRIX)]<-'FW'
stUseList$TRIBAL<-0
#Remove stations on tribal lands from 2010 stations
#I did a select by location query in ArcGIS, and copied the station numbers manually
tribal2010<-c(10406,23106,13282,12023,23103,23102,12005,23096,24058,24044,24066,24437)
stUseList$USE_Final[stUseList$STATION %in% as.character(tribal2010)]<-0
stUseList$TRIBAL[stUseList$STATION %in% as.character(tribal2010)]<-1
stUseList$AGENCY<-'Oregon Department of Environmental Quality'
stUseList$AGENCY_ID<-'ODEQ'
stUseList[stUseList=='NA']<-NA


#Create an empty df for station additions with the same columns as the wqa dbase
stAppend<-stUseList[FALSE,] 
# [1] "StationUse_RecordID"     "STATION"                 "DESCRIPTION"             "DEC_LAT"                
# [5] "DEC_LONG"                "RIVER_MILE"              "STREAM_LLID"             "RIVER_MILE_LAKE"        
# [9] "LAKE_LLID"               "LAKE_NAME"               "GIS_Source_LAKE"         "USE_OtherParms"         
# [13] "USE_Final"               "GIS_Source"              "HUC_3rd_Field"           "Bio_Macro_Data"         
# [17] "CASCADE_LAKES"           "MARINE_WATERS"           "STRATIFIED_Natural_Lake" "ESTUARY"                
# [21] "LAKE"                    "MARINE_COASTAL_REC"      "Enterro_Data"            "DHS_Station_Type"       
# [25] "DHS_Station_Type_Num"    "EPA_BEACH_ID"            "BEACH_NAME"              "ELEVATION_FT"           
# [29] "WRD_Basin"               "Comments"                "Reviewed_Permit_Writers" "RIVER_MILE_2004"        
# [33] "LLID_2004"               "USE_Bacteria"            "Salinity_Max"            "Conductivity_Max"       
# [37] "PondResSmall"            "Stream_Name"             "MATRIX"                  "TRIBAL"                 
# [41] "AGENCY"                  "AGENCY_ID"

commonColumns<-intersect(names(stations),
                         intersect(names(grest),
                                   intersect(names(merst), 
                                             intersect(names(stUseList), 
                                                       intersect(names(merlist), names(usgsst))))))
print(commonColumns)
morecols<-setdiff(names(stAppend), commonColumns)
#morecols2<-setdiff(commonColumns, names(stAppend))

stations[,morecols]<-NA
grest[,morecols]<-NA
merst[,morecols]<-NA
merlist[,morecols]<-NA
usgsst[,morecols]<-NA
#stUseList[,morecols]<-NA

stAppend<-rbind(stAppend, 
                rbind(grest[,names(stUseList)],
                      rbind(merst[,names(stUseList)],
                            rbind(merlist[,names(stUseList)], 
                                  rbind(stations[,names(stUseList)], usgsst[,names(stUseList)])))))

stAppend$ESTUARY[stAppend$MATRIX=='ES' & !is.na(stAppend$MATRIX)]<-1
stAppend$MARINE_WATERS[stAppend$MATRIX=='SW' & !is.na(stAppend$MATRIX)]<-1
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
stAppend$STATIONS_2012<-1
stUseList$STATIONS_2012<-0
stUseList2012<-rbind(stAppend, stUseList)
stUseList2012<-join(stUseList2012, karla.usability[,c('STATION', 'UsabilityComments')], 'STATION', 
                    match='first')
stUseList2012$LAKE_LLID[stUseList2012$LAKE_LLID==0 & !is.na(stUseList2012$LAKE_LLID)]<-NA

#Verify tribal lands
# nrow(stUseList2012[stUseList2012$TRIBAL==1,c('TRIBAL', 'USE_Final')])
# stUseList2012[stUseList2012$TRIBAL==1,c('TRIBAL', 'USE_Final')]

#Temporary final use table while I synchronize the rest of the fields
# stUseFinalList2012<-stUseList2012[stUseList2012$USE_Final==1,]
# write.csv(stUseFinalList2012, 'I:/2012_WQAssessment/ToxicsRedo/StationsToLocate/stUseFinalList2012.csv',
#           row.names=F)

#Final use table csv
write.csv(stUseList2012, 'I:/2012_WQAssessment/ToxicsRedo/StationsToLocate/stUseList2012.csv',
          row.names=F)
write.csv(stUseList2012[stUseList2012$USE_Final==1,], 'I:/2012_WQAssessment/ToxicsRedo/StationsToLocate/stUseList2012_Final.csv',
          row.names=F)

#station dfs:
# 1) stations
# 2) grest
# 3) merst
# 4) merlist
# 5) usgsst
# 6) stUseList



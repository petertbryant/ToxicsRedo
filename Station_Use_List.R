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
# [1] "LLID"       "LAKE_LLID"  "AGENCY"     "AGENCY_ID"  "STATION"    "DEC_LAT"    "DEC_LONG"   "DESCRIPTIO" "QAQC1"     
# [10] "QAQC2"      "Comments"   "RIVER_MILE" "HUC_6"      "HU_6_Name"  "HUC_8"      "HU_8_Name"  "GIS_STREAM" "LAKE_NAME" 
# [19] "GIS_Source" "GIS_Sour_1" "Estuary_20" "Sal_ppth"   "Est_Sal"    "Est_Final"  "Est_Commen" "MATRIX"     "WB_Added"  
# [28] "WB_Comment" "TRIBAL"     "DATUM"
stations<-rename(stations, c('LLID'='STREAM_LLID', 'DESCRIPTIO'='DESCRIPTION', 'GIS_STREAM'='STREAM_NAME'))
stations$WB_Added[is.na(stations$WB_Added)]<-'No review needed'

#Designate final use
stations$USE_Final<-NA
stations$USE_Final[(stations$QAQC2 != 'Remove' & stations$WB_Added != 'Remove' & stations$TRIBAL == 0)]<-1
stations$USE_Final[!(stations$QAQC2 != 'Remove' & stations$WB_Added != 'Remove' & stations$TRIBAL == 0)]<-0

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
# [1] "Unique_ID"  "RID"        "MEAS"       "Distance"   "SITE_DESCR" "LATITUDE_D" "LONGITUDE_" "LAT_LONG_S"
# [9] "LAT_LONG_D" "LLID"       "LLID_SOURC" "RIVER_MILE" "RIVER_MI_1" "SITE_ELEVA" "SITE_ELE_1" "SITE_ELE_2"
# [17] "LASAR__ID"  "STATION_ID" "Unique_ID_" "QAQC1"      "QAQC2"

grest<-rename(grest, c('RID'='STREAM_LLID', 'MEAS'='RIVER_FOOT', 'SITE_DESCR'='DESCRIPTION', 'LATITUDE_D'='DEC_LAT',
                       'LONGITUDE_'='DEC_LONG', 'STATION_ID'='STATION', 'RIVER_MILE'='RM_SOURCE'))
grest$RIVER_MILE<-round(grest$RIVER_FOOT/5280, 3)
grest$MATRIX<-'FW'
grest$LAKE_LLID<-NA

#Mercury to locate list
inshape<-'E:/GitHub/ToxicsRedo/Shapefiles_for_Access'
station.shp<-readOGR(inshape, 'Mercury_Stations')
merst<-station.shp@data

#Rename Mercury column names
names(merst)
# [1] "STATION_KE" "LOCATION_D" "Latitude"   "Longitude"  "DATUM"      "DATUM2"     "Unique_ID"  "Comments"  
# [9] "MEAS"       "RID"        "Matrix"     "STATION_ID" "QAQC1"      "QAQC2"      "Distance"   "Unique_ID_"
# [17] "Lake_LLLID"
merst<-rename(merst, c('STATION_ID'='STATION', 'LOCATION_D'='DESCRIPTION', 'Latitude'='DEC_LAT', 
                       'Longitude'='DEC_LONG', 'RID'='STREAM_LLID',
                       'MEAS'='RIVER_FOOT', 'Lake_LLLID'='LAKE_LLID', 'Matrix'='MATRIX'))
merst$RIVER_MILE<-round(merst$RIVER_FOOT/5280, 3)
merst$USE_Final<-NA
merst$USE_Final[merst$QAQC2 != 'Remove']<-1
merst$USE_Final[merst$QAQC2 == 'Remove']<-0

#Mercury not to locate list
merlist<-read.csv('//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/StationstoLocate/stations_to_add_but_not_locate_07142014.csv',
                  header=T)
names(merlist)
# [1] "STATION_KEY"          "LOCATION_DESCRIPTION" "LLID_Stream_Lake"     "Stream_Lake_Name"     "Str_RM"              
# [6] "Latitude"             "Longitude"            "DATUM"
merlist$'STREAM_LLID'<-substr(merlist$'LLID_Stream_Lake', 1, 13)
merlist$'LAKE_LLID'<-substr(merlist$'LLID_Stream_Lake', 15, 28)
merlist<-rename(merlist, c('STATION_KEY'='STATION', 'LOCATION_DESCRIPTION'='DESCRIPTION', 'LATITUDE'='DEC_LAT',
                           'LONGITUDE'='DEC_LONG', 'Str_RM'='RIVER_MILE'))

#MORE USGS Station list
inshape<-'E:/GitHub/ToxicsRedo/Shapefiles_for_Access'
station.shp<-readOGR(inshape, 'MORE_Stations')
usgsst<-station.shp@data

#Rename MORE USGS column names
names(usgsst)
# [1] "Unique_ID"  "RID"        "MEAS"       "Distance"   "Organizati" "Organiza_1" "Monitoring" "Monitori_1"
# [9] "Monitori_2" "Monitori_3" "HUCEightDi" "DrainageAr" "Drainage_1" "Contributi" "Contribu_1" "LatitudeMe"
# [17] "LongitudeM" "SourceMapS" "Horizontal" "Horizont_1" "Horizont_2" "Horizont_3" "VerticalMe" "Vertical_1"
# [25] "VerticalAc" "Vertical_2" "VerticalCo" "Vertical_3" "CountryCod" "StateCode"  "CountyCode" "AquiferNam"
# [33] "FormationT" "AquiferTyp" "Constructi" "WellDepthM" "WellDept_1" "WellHoleDe" "WellHole_1" "site_only" 
# [41] "Unique_ID_" "QAQC1"      "QAQC2" 
usgsst$RIVER_MILE<-round(usgsst$MEAS/5820, 3)
usgsst$MATRIX<-'FW'
usgsst$LAKE_LLID<-NA
usgsst$USE_Final[usgsst$QAQC2 != 'Remove']<-1
usgsst$USE_Final[usgsst$QAQC2 == 'Remove']<-0
usgsst<-rename(usgsst, c('RID'='STREAM_LLID', 'MEAS'='RIVER_FOOT', 'Monitori_1'='DESCRIPTION',
                         'LatitudeMe'='DEC_LAT', 'LongitudeM'='DEC_LONG', 'site_only'='STATION'))

#Update USE_Final based on Karla's Usability comments
#***These comments need to be included in the final df***
accdb<-'//deqhq1/WQAssessment/2012_WQAssessment/2012_WorkingTables.mdb'
con<-odbcConnectAccess2007(accdb)
karla.usability<-sqlFetch(con, 'All_stations_final_est')
odbcCloseAll()
stations$USE_Final[stations$STATION %in% karla.usability$STATION[grep('Don\'t use', karla.usability$UsabilityComments)]] <- 0

#Remove stations on tribal lands from 2010 stations
#I did this with a select by location query in ArcGIS, and copied the station numbers manually
tribal2010<-c(10406,23106,13282,12023,23103,23102,12005,23096,24058,24044,24066,24437)

#Accessing WQAssessment db
wqadb<-odbcConnect('WQAssessment')
wqadbTables<-sqlTables(wqadb, tableType="Table")
stUseTable<-wqadbTables$TABLE_NAME[grep('stationuse', wqadbTables$TABLE_NAME, ignore.case=T)]
stUseList<-sqlFetch(wqadb, stUseTable)
#dtype<-sqlQuery(wqadb, 'select * from StationUseList.information_schema.columns;')

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
# [37] "PondResSmall"            "Stream_Name"

commonColumns<-intersect(names(stations),
                          intersect(names(grest),
                                    intersect(names(merst), 
                                              intersect(names(stUseList), names(usgsst)))))
print(commonColumns)

#Merge all together


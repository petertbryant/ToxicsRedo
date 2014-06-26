library(RODBC)

con.2010 <- odbcConnectAccess('//deqhq1/wqassessment/2010_WQAssessment/Databases/WorkingTables_2010.mdb')
sul2010 <- sqlFetch(con.2010, 'StationUseList_2010')
loc <- "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/ShapeFiles"
shp <- "All_stations_final"
st.new <- readOGR(loc, shp)

write.csv(sul2010, "./Estuary_Analysis/station_2010.csv", row.names=F)

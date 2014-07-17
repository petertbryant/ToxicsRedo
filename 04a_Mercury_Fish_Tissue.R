library(plyr)
library(RODBC)
library(psych)

con <- odbcConnect('WQAssessment')
sul2010 <- sqlFetch(con, 'StationUseList')
odbcCloseAll()

sul2012 <- read.csv('Estuary_Analysis/All_stations_final_est.csv')

hg.ft.all <- read.csv('LASAR_WQP_Mercury_fish_tissue.csv')

hg.ft.all$SAMPLE_DATE <- as.POSIXct(strptime(hg.ft.all$SAMPLE_DATE, format = "%m/%d/%Y"))
hg.ft.all$Result_clean <- as.numeric(hg.ft.all$Result_clean)

hg.ft.all$x <- ifelse(!hg.ft.all$QA_QC_TYPE %in% c('Sample','Sample-Routine'),1,0)

#Manual is easiest here ---- hg.ft.all[hg.ft.all$x == 1,]
hg.ft.fd.resolved <- hg.ft.all[c(100,91,291,289,108,168,217,259,277,289,291,303,318,322,548,687,693),]

hg.ft.wo <- hg.ft.all[hg.ft.all$x != 1,]
hg.ft.all.fixed <- rbind(hg.ft.wo, hg.ft.fd.resolved)

hg.ft.all.grouped <- ddply(hg.ft.all.fixed, .(STATION_KEY, Agency, LOCATION_DESCRIPTION), 
                           summarise, 
                           Geo.Mean = geometric.mean(Result_clean), 
                           samples = length(Result_clean), 
                           SAMPLE_DATE_START = min(SAMPLE_DATE), 
                           SAMPLE_DATE_END = max(SAMPLE_DATE))

hg.ft.all.grouped$exceed <- ifelse(hg.ft.all.grouped$Geo.Mean <= 0.04,0,1)

#for now so we can run the text summary compilation function we will add a dummy Str_RM
hg.ft.all.grouped$Str_RM <- sample(1:100, nrow(hg.ft.all.grouped), replace = TRUE)

hg.ft.all.grouped <- rename(hg.ft.all.grouped, c('STATION_KEY' = 'site_no', 'samples' = 'Valid_n'))

hg.ft.all.grouped$SAMPLE_DATE_START <- strftime(hg.ft.all.grouped$SAMPLE_DATE_START, format = '%m/%d/%Y')

hg.ft.all.grouped$SAMPLE_DATE_END <- strftime(hg.ft.all.grouped$SAMPLE_DATE_END, format = '%m/%d/%Y')

hg.summary <- ddply(hg.ft.all.grouped, .(site_no), text.summary.hg)

#### Station Location ####
hg.w.sul <- merge(hg.ft.all.grouped, sul2010[,c('STATION','USE_OtherParms','USE_Final')], by.x = 'site_no', by.y = 'STATION', all.x = TRUE)

hg.for.mike <- hg.w.sul[is.na(hg.w.sul$USE_OtherParms),]

hg.for.mike <- merge(hg.for.mike, sul2012, by.x = 'site_no', by.y = 'STATION', all.x = TRUE)

hg.for.mike <- hg.for.mike[is.na(hg.for.mike$OBJECTID),]

#This hg data comes from running select lines in the Mercury_Fish_Tissue_Incorporate 
hg.for.mike <- merge(hg.for.mike, hgdata, by = 'site_no', all.x = TRUE)

stations.to.add.to.sul.but.not.locate <- hg.for.mike[!is.na(hg.for.mike$Agency),]

hg.for.mike <- hg.for.mike[is.na(hg.for.mike$Agency),]

hg.for.mike <- hg.for.mike[,colSums(is.na(hg.for.mike))<nrow(hg.for.mike)]

stations.to.add.to.sul.but.not.locate <- stations.to.add.to.sul.but.not.locate[,colSums(is.na(stations.to.add.to.sul.but.not.locate))<nrow(stations.to.add.to.sul.but.not.locate)]

#we need datum which isn't in lasarWeb.
library(foreign)
lasar.check <- read.dbf('//deqlead03/gis_wa/project_working_folders/lasar_stations/lasar_stations/LASAR_Stations_26sept13.dbf')

#sub lasar.check with just those stations we want to locate
lstl.ll <- (lasar.check[lasar.check$STATION_KE %in% hg.for.mike$site_no,])

to.add <- hg.for.mike[!hg.for.mike$site_no %in% lstl.ll$STATION_KE,]

sta <- stations.to.add.to.sul.but.not.locate[,c('site_no','LOCATION_DESCRIPTION','LLID_Stream_Lake','Stream_Lake_Name','Str_RM')]

to.add$LLID_Stream_Lake <- NA
to.add$Stream_Lake_Name <- NA
to.add$Str_RM <- NA

to.add <- to.add[,names(sta)]

stata <- rbind(sta, to.add)

stata.ll <- merge(stata, lasar.check[,c('STATION_KE','Latitude','Longitude','DATUM')], by.x = 'site_no', by.y = 'STATION_KE', all.x = TRUE)

ll.add <- data.frame('STATION_KE' = 'NWPPA', 
                     'LOCATION_D' = 'Northwest Pulp and Paper Association Member Mill',
                     'Latitude' = 46.152501, 
                     'Longitude' = -123.397343, 
                     'DATUM' = 'Assumed NAD83')

lstl.ll <- lstl.ll[,names(ll.add)]

lstl.ll <- rbind(lstl.ll, ll.add)

# write.csv(stata.ll, 'StationstoLocate/stations_to_add_but_not_locate_07142014.csv', row.names = FALSE)
# write.csv(lstl.ll, 'StationstoLocate/mercury_fish_tissue_stations_07142014.csv', row.names = FALSE)


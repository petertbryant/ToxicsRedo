library(plyr)
library(RODBC)
library(psych)

<<<<<<< HEAD
con <- odbcConnect('WQAssessment')
sul2010 <- sqlFetch(con, 'StationUseList_2010')
sul2012 <- sqlFetch(con, 'StationUseList')
=======
ref.con <- odbcConnect('WQAssessment')
sul2012 <- sqlFetch(ref.con, 'StationUseList')
sul2010 <- sqlFetch(ref.con, 'StationUseList_2010')
>>>>>>> ffc1c233efa90ce527ce89d995ab8e55cdd7c8f4
odbcCloseAll()


hg.ft.all <- read.csv('LASAR_WQP_Mercury_fish_tissue.csv')

#Manual is easiest here ---- hg.ft.all[hg.ft.all$x == 1,]
hg.ft.all$x <- ifelse(!hg.ft.all$QA_QC_TYPE %in% c('Sample','Sample-Routine'),1,0)
hg.ft.fd.resolved <- hg.ft.all[c(100,91,291,289,108,168,217,259,277,289,291,303,318,322,548,687,693),]

hg.ft.wo <- hg.ft.all[hg.ft.all$x != 1,]
hg.ft.all.fixed <- rbind(hg.ft.wo, hg.ft.fd.resolved)

#We only want Fillets
hg.ft.all <- hg.ft.all.fixed[grep('[Ff]illet',hg.ft.all.fixed$SAMPLE_MATRIX),]

hg.ft.all$SAMPLE_DATE <- as.POSIXct(strptime(hg.ft.all$SAMPLE_DATE, format = "%m/%d/%Y"))
hg.ft.all$Result_clean <- as.numeric(hg.ft.all$Result_clean)


hg.ft.all.grouped <- ddply(hg.ft.all, .(STATION_KEY, Agency, LOCATION_DESCRIPTION), 
                           summarise, 
                           Geo.Mean = geometric.mean(Result_clean), 
                           Valid_n = length(Result_clean), 
                           SAMPLE_DATE_START = min(SAMPLE_DATE), 
                           SAMPLE_DATE_END = max(SAMPLE_DATE))

hg.ft.all.grouped <- rename(hg.ft.all.grouped, c('STATION_KEY' = 'site_no'))

hg.ft.all.grouped$SAMPLE_DATE_START <- strftime(hg.ft.all.grouped$SAMPLE_DATE_START, format = '%m/%d/%Y')

hg.ft.all.grouped$SAMPLE_DATE_END <- strftime(hg.ft.all.grouped$SAMPLE_DATE_END, format = '%m/%d/%Y')

#### Station Location ####
# hg.w.sul <- merge(hg.ft.all.grouped, sul2010[,c('STATION','USE_OtherParms','USE_Final')], by.x = 'site_no', by.y = 'STATION', all.x = TRUE)
# 
# hg.for.mike <- hg.w.sul[is.na(hg.w.sul$USE_OtherParms),]
# 
# hg.for.mike <- merge(hg.for.mike, sul2012, by.x = 'site_no', by.y = 'STATION', all.x = TRUE)
# 
# hg.for.mike <- hg.for.mike[is.na(hg.for.mike$OBJECTID),]
# 
# #This hg data comes from running select lines in the Mercury_Fish_Tissue_Incorporate 
# hg.for.mike <- merge(hg.for.mike, hgdata, by = 'site_no', all.x = TRUE)
# 
# stations.to.add.to.sul.but.not.locate <- hg.for.mike[!is.na(hg.for.mike$Agency),]
# 
# hg.for.mike <- hg.for.mike[is.na(hg.for.mike$Agency),]
# 
# hg.for.mike <- hg.for.mike[,colSums(is.na(hg.for.mike))<nrow(hg.for.mike)]
# 
# stations.to.add.to.sul.but.not.locate <- stations.to.add.to.sul.but.not.locate[,colSums(is.na(stations.to.add.to.sul.but.not.locate))<nrow(stations.to.add.to.sul.but.not.locate)]
# 
# #we need datum which isn't in lasarWeb.
# library(foreign)
# lasar.check <- read.dbf('//deqlead03/gis_wa/project_working_folders/lasar_stations/lasar_stations/LASAR_Stations_26sept13.dbf')
# 
# #sub lasar.check with just those stations we want to locate
# lstl.ll <- (lasar.check[lasar.check$STATION_KE %in% hg.for.mike$site_no,])
# 
# to.add <- hg.for.mike[!hg.for.mike$site_no %in% lstl.ll$STATION_KE,]
# 
# sta <- stations.to.add.to.sul.but.not.locate[,c('site_no','LOCATION_DESCRIPTION','LLID_Stream_Lake','Stream_Lake_Name','Str_RM')]
# 
# to.add$LLID_Stream_Lake <- NA
# to.add$Stream_Lake_Name <- NA
# to.add$Str_RM <- NA
# 
# to.add <- to.add[,names(sta)]
# 
# stata <- rbind(sta, to.add)
# 
# stata.ll <- merge(stata, lasar.check[,c('STATION_KE','Latitude','Longitude','DATUM')], by.x = 'site_no', by.y = 'STATION_KE', all.x = TRUE)
# 
# ll.add <- data.frame('STATION_KE' = 'NWPPA', 
#                      'LOCATION_D' = 'Northwest Pulp and Paper Association Member Mill',
#                      'Latitude' = 46.152501, 
#                      'Longitude' = -123.397343, 
#                      'DATUM' = 'Assumed NAD83')
# 
# lstl.ll <- lstl.ll[,names(ll.add)]
# 
# lstl.ll <- rbind(lstl.ll, ll.add)

# write.csv(stata.ll, 'StationstoLocate/stations_to_add_but_not_locate_07142014.csv', row.names = FALSE)
# write.csv(lstl.ll, 'StationstoLocate/mercury_fish_tissue_stations_07142014.csv', row.names = FALSE)

#### Check for existing ####
hg.LLID <- merge(hg.ft.all.grouped, sul2012[sul2012$USE_Final == 1,c('STATION','LAKE_LLID','STREAM_LLID','RIVER_MILE')], by.x = 'site_no', by.y = 'STATION')
hg.LLID$LAKE_LLID <- ifelse(hg.LLID$LAKE_LLID %in% c(0,NA,'','<Null>'), NA, hg.LLID$LAKE_LLID)
# hg.LLID$LLID_Stream_Lake <- ifelse(is.na(hg.LLID$LAKE_LLID), 
#                                        hg.LLID$STREAM_LLID,
#                                        ifelse(is.na(hg.LLID$STREAM_LLID),
#                                               hg.LLID$LAKE_LLID,
#                                               paste(hg.LLID$STREAM_LLID, hg.LLID$LAKE_LLID, sep = '/')))
hg.LLID$Str_RM <- as.numeric(hg.LLID$RIVER_MILE)
hg.LLID$Pollutant <- 'Mercury'
hg.LLID$Season <- 'Year Round'

ref.con <- odbcConnect('WQAssessment')
record <- sqlQuery(ref.con, 'SELECT * FROM Assessment_Record')
odbcCloseAll()

#this checks against the ars for existing records but since criteria and sample matrix are not handled well in that the ars 
#we have to go back to the record table to check for sample matrix and criteria matches.
for (i in 1:nrow(hg.LLID)) {
  if (any(ars$LLID_Stream == hg.LLID$STREAM_LLID[i], na.rm = TRUE)) {
    matched.seg <- ars[which(ars$LLID_Stream == hg.LLID$STREAM_LLID[i]),]
  } else {
    matched.seg <- ars[which(ars$LLID_Lake == hg.LLID$LAKE_LLID[i]),]
  }      
  matched.seg.pol <- subset(matched.seg, matched.seg$Pollutant == hg.LLID$Pollutant[i])
  matched.seg.pol.seas <- subset(matched.seg.pol, matched.seg.pol$Season == hg.LLID$Season[i])
  
  if (nrow(matched.seg.pol.seas) > 0) {
    for (j in 1:nrow(matched.seg.pol.seas)) {
      rec.match <- record[record$Record_ID == matched.seg.pol.seas$Record_ID[j],]
      if(hg.LLID$Str_RM[i] >= matched.seg.pol.seas$RM1[j] & 
           hg.LLID$Str_RM[i] <= matched.seg.pol.seas$RM2[j] &
           rec.match$SampleMatrix_ID == 2 & !rec.match$NarCriteria_ID %in% c(9,17)) {
        hg.LLID$SegmentID[i] <- matched.seg.pol.seas$Segment_ID[j]
        hg.LLID$RecordID[i] <- matched.seg.pol.seas$Record_ID[j]
      } 
      else {
        hg.LLID$SegmentID[i] <- NA
        hg.LLID$RecordID[i] <- NA
      }
    } 
  } else {
    hg.LLID$SegmentID[i] <- NA
    hg.LLID$RecordID[i] <- NA
  }
}

#hg.LLID <- cbind(hg.LLID, colsplit(hg.LLID$Stream_Lake_Name, pattern = '/', names = c('Stream_Name', 'Lake_Name')))
#hg.LLID <- cbind(hg.LLID, colsplit(hg.LLID$LLID_Stream_Lake, pattern = '/', names = c('Str_LLID', 'LLID_Lake')))
hg.LLID$Pollutant_ID <- 2260
hg.LLID$Season_ID <- 3

hg.LLID$Str_RM <- round(as.numeric(hg.LLID$Str_RM), 1)
hg.new <- hg.LLID[is.na(hg.LLID$RecordID),]
hg.exist <- hg.LLID[!is.na(hg.LLID$RecordID),]

hg.new.to.group <- hg.new[hg.new$LAKE_LLID %in% c('1223929422233','1223333423868','1227683424103'),]
hg.new.lake.grouped <- ddply(hg.new.to.group, .(LAKE_LLID), 
                         summarise, 
                         Geo.Mean = geometric.mean(Geo.Mean), 
                         Valid_n = length(site_no),
                         SAMPLE_DATE_START = min(SAMPLE_DATE_START),
                         SAMPLE_DATE_END = max(SAMPLE_DATE_END),
                         nStations = length(site_no))
hg.new.wo.singles <- hg.new[!hg.new$LAKE_LLID %in% c('1223929422233','1223333423868','1227683424103'),]

hg.new.wo.singles.summary <- ddply(hg.new.wo.singles, .(STREAM_LLID), text.summary.hg)
hg.new.wo.singles.summary <- rename(hg.new.wo.singles.summary, c('V1' = 'Summary'))
hg.new.wo.singles.summary$LAKE_LLID <- NA
hg.new.wo.singles.summary[hg.new.wo.singles.summary$STREAM_LLID == '1230436438208','LAKE_LLID'] <- '1229146437703'
hg.new.wo.singles.summary[hg.new.wo.singles.summary$STREAM_LLID == '1233218433779','LAKE_LLID'] <- '1232627433773' 
hg.new.wo.singles.summary[hg.new.wo.singles.summary$STREAM_LLID == '1241613435770','LAKE_LLID'] <- '1241456435885'
hg.new.wo.singles.summary[hg.new.wo.singles.summary$STREAM_LLID == '1241746435728','LAKE_LLID'] <- '1241367435617'
hg.new.lake.grouped$Summary <- c('For 16 stations from 09/03/2005 to 10/03/2005, the geometric mean of 0.091 mg/Kg from 16 valid individual fish tissue samples exceeds the 0.040 mg/kg criteria',
                                 'For 8 stations from 09/08/2005 to 09/08/2005, the geometric mean of 0.031 mg/Kg from 8 valid individual fish tissue samples exceeds the 0.040 mg/kg criteria',
                                 'For 8 stations from 09/12/2005 to 09/12/2005, the geometric mean of 0.269 mg/Kg from 8 valid individual fish tissue samples exceeds the 0.040 mg/kg criteria')
hg.new.lake.grouped$STREAM_LLID <- NA
hg.new.lake.grouped[hg.new.lake.grouped$LAKE_LLID == '1223333423868','STREAM_LLID'] <- '1226154424196' 
hg.new.lake.grouped[hg.new.lake.grouped$LAKE_LLID == '1223929422233','STREAM_LLID'] <- '1223412421963'
hg.new.lake.grouped[hg.new.lake.grouped$LAKE_LLID == '1227683424103','STREAM_LLID'] <- '1228061424457'
hg.new.summary <- rbind(hg.new.wo.singles.summary, hg.new.lake.grouped[,c('STREAM_LLID','LAKE_LLID','Summary')])
hg.exist.summary <- ddply(hg.exist, .(RecordID), text.summary.hg)
hg.exist.summary <- rename(hg.exist.summary, c('RecordID' = 'Record_ID'))

newsegs.hg <- hg.new.summary
newsegs.hg$Pollutant <- 'Mercury'
newsegs.hg$Pollutant_ID <- 2260
newsegs.hg$RM1 <- NA
newsegs.hg$RM2 <- NA
newsegs.hg$Status <- NA
newsegs.hg[newsegs.hg$STREAM_LLID == '1193384459144',c('RM1','RM2','Status')] <- c(0,56,'5')
newsegs.hg[newsegs.hg$STREAM_LLID == '1206499457318',c('RM1','RM2','Status')] <- c(0,277.6,'5')
newsegs.hg[newsegs.hg$STREAM_LLID == '1209151456389',c('RM1','RM2','Status')] <- c(0,244.2,'5')
newsegs.hg[newsegs.hg$STREAM_LLID == '1215067457204',c('RM1','RM2','Status')] <- c(0,14.6,'3B')
newsegs.hg[newsegs.hg$STREAM_LLID == '1222619442061',c('RM1','RM2','Status')] <- c(0,9.7,'3B')
newsegs.hg[newsegs.hg$STREAM_LLID == '1229133455196',c('RM1','RM2','Status')] <- c(0,9.8,'3B')
newsegs.hg[newsegs.hg$STREAM_LLID == '1230436438208',c('RM1','RM2','Status')] <- c(7.3,11.9,'5')
newsegs.hg[newsegs.hg$STREAM_LLID == '1230728455698',c('RM1','RM2','Status')] <- c(0,31.5,'3B')
newsegs.hg[newsegs.hg$STREAM_LLID == '1233218433779',c('RM1','RM2','Status')] <- c(2.4,4,'5')
newsegs.hg[newsegs.hg$STREAM_LLID == '1239230455475',c('RM1','RM2','Status')] <- c(0,15.2,'3B')
newsegs.hg[newsegs.hg$STREAM_LLID == '1239378455599',c('RM1','RM2','Status')] <- c(0,7.2,'3B')
newsegs.hg[newsegs.hg$STREAM_LLID == '1240483462464',c('RM1','RM2','Status')] <- c(0,303.9,'5')
newsegs.hg[newsegs.hg$STREAM_LLID == '1240830446097',c('RM1','RM2','Status')] <- c(0,57.5,'3B')
newsegs.hg[newsegs.hg$STREAM_LLID == '1241468435884',c('RM1','RM2','Status')] <- c(0,4.8,'3B')
newsegs.hg[newsegs.hg$STREAM_LLID == '1241613435770',c('RM1','RM2','Status')] <- c(0,4.5,'3B')
newsegs.hg[newsegs.hg$STREAM_LLID == '1241631435695',c('RM1','RM2','Status')] <- c(0,5.4,'3B')
newsegs.hg[newsegs.hg$STREAM_LLID == '1241746435728',c('RM1','RM2','Status')] <- c(0,5,'3B')
newsegs.hg[newsegs.hg$STREAM_LLID == '1226154424196',c('RM1','RM2','Status')] <- c(15.6,17.8,'5')
newsegs.hg[newsegs.hg$STREAM_LLID == '1223412421963',c('RM1','RM2','Status')] <- c(3.1,7.9,'5')
newsegs.hg[newsegs.hg$STREAM_LLID == '1228061424457',c('RM1','RM2','Status')] <- c(3,3.9,'5')
newsegs.hg <- merge(newsegs.hg, LLID.Lakes[,c('Lake_LLID','Lake_Name')], by.x = 'LAKE_LLID', by.y = 'Lake_LLID', all.x = TRUE)
newsegs.hg <- merge(newsegs.hg, LLID.Streams[,c('LLID','NAME')], by.x = 'STREAM_LLID', by.y = 'LLID', all.x = TRUE)
newsegs.hg <- rename(newsegs.hg, c('Lake_Name' = 'LAKE_NAME', 'NAME' = 'Stream_Name'))
newsegs.hg$SampleMatrix_ID <- 2


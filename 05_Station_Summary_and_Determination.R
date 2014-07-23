dma <- dcc.min[!is.na(dcc.min$value),]

dma$day.POSIX <- as.POSIXct(strptime(dma$day, format = '%Y-%m-%d'))

library(dplyr)

#a little inconsistency in agency
dma[dma$Agency == 'EPA National Aquatic Resource Survey Data','Agency'] <- "EPA National Aquatic Resources Survey" 

dma.groups <- group_by(dma, Agency, SampleRegID, SampleAlias, criterianame.x, variable)
dma.summary <- summarise(dma.groups, exceed = sum(exceed*Valid),
                         valid_n = sum(Valid),
                         total_n = length(Valid), 
                         percent.exceed = (exceed/valid_n)*100,
                         min_date = min(day.POSIX),
                         max_date = max(day.POSIX))

#station status determination
dma.summary$cat <- ifelse(dma.summary$exceed >= 2,
                          ifelse(dma.summary$criterianame.x %in% c('Alkalinity','Phosphate Phosphorus'),
                                 '3B',
                                 '5'),
                          ifelse(dma.summary$exceed == 1,
                                 '3B',
                                 ifelse(dma.summary$valid_n >= 5,
                                        '2',
                                        '3')))

#We can pull LLID information back in here
sul2012 <- read.csv('//Deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/StationsToLocate/stUseList2012_Final.csv')
sul2012 <- rename(sul2012, c('MATRIX' = 'Matrix'))
sul2012 <- sul2012[!duplicated(sul2012$STATION),]
dma.summary <- merge(dma.summary, sul2012[,c('STATION','STREAM_LLID','Stream_Name','LAKE_LLID','LAKE_NAME','RIVER_MILE')], by.x = 'SampleRegID', by.y = 'STATION')

#Build the LLID_Stream_Lake field
dma.summary$LAKE_LLID <- ifelse(dma.summary$LAKE_LLID %in% c(0,NA), NA, dma.summary$LAKE_LLID)
dma.summary$LLID_Stream_Lake <- ifelse(is.na(dma.summary$LAKE_LLID), 
                                        dma.summary$STREAM_LLID,
                                        ifelse(is.na(dma.summary$STREAM_LLID),
                                               dma.summary$LAKE_LLID,
                                               paste(dma.summary$STREAM_LLID, dma.summary$LAKE_LLID, sep = '/')))

#Map to Assessment_Pollutant table to get Pollutant ID and start matching to AU's
# unique(dmas.wo.invalid$criterianame.x)[!unique(dmas.wo.invalid$criterianame.x) %in% pollutants$Pollutant]
# 
# pollutants$criterianame.x <- pollutants$Pollutant
# 
# redo.relate <- merge(pollutants, data.frame(criterianame.x = unique(dmas.wo.invalid$criterianame.x)), 
#                      by = 'criterianame.x', all.y = TRUE)
# write.csv(redo.relate, 'Criteria_Pollutant_Lookup.csv', row.names = FALSE)
wt <- odbcConnectAccess('//deqhq1/wqassessment/2012_wqassessment/2012_Workingtables.mdb')
redo.relate <- sqlFetch(wt, 'Criteria_Pollutant_Lookup')
dma.pollutant <- merge(dma.summary, redo.relate[,c('criterianame','Pollutant','Pollutant_ID','Former_Group_Pollutant_Name')], by.x = 'criterianame.x', by.y = 'criterianame', all.x = TRUE)

#for review let's put the tables into access
# rm(list = setdiff(ls(), c('dma', 'dma.summary')))
# access <- odbcConnectAccess('//deqhq1/wqassessment/2012_WQAssessment/2012_WorkingTables.mdb')
# con <- odbcConnect('WQAssessment')
# dma.save <- within(dma, rm('id','Matrix.y','index','day.POSIX'))
# dma.save$relate <- paste(dma.save$SampleRegID, dma.save$criterianame.x)
# sqlSave(con, dma.save, 'IR2012_ToxicsRedo_Data')
# dma.save.pollutant <- within(dma.pollutant, rm(Former_Group_Pollutant_Name))
# dma.save.pollutant$min_date <- strftime(dma.save.pollutant$min_date, format = '%m/%d/%Y')
# dma.save.pollutant$max_date <- strftime(dma.save.pollutant$max_date, format = '%m/%d/%Y')
# dma.save.pollutant$relate <- paste(dma.save.pollutant$SampleRegID, dma.save.pollutant$criterianame.x)
# dma.save.pollutant$RIVER_MILE <- as.numeric(dma.save.pollutant$RIVER_MILE)
# dma.save.pollutant$LAKE_LLID <- as.numeric(dma.save.pollutant$LAKE_LLID)
# sqlSave(con, dma.save.pollutant, 'IR2012_ToxicsRedo_StationSummaries')

#Now let's match to the ars to identify existing records and where new records need to be created
ref.con <- odbcConnect('WQAssessment')
ars <- sqlQuery(ref.con, 'SELECT * FROM Assessment_Report_Summary')
table(ars[ars$Pollutant %in% dma.pollutant$Pollutant,'Season'])
record <- sqlQuery(ref.con, 'SELECT * FROM Assessment_Record')

ars <- merge(ars, record[,c('Record_ID','SampleMatrix_ID')], by = 'Record_ID', all.x = TRUE)

dma.pollutant$RecordID <- NA
dma.pollutant$SegmentID <- NA
dma.pollutant$SampleMatrix_ID <- 1

ars$RM1 <- as.numeric(ars$RM1)
ars$RM2 <- as.numeric(ars$RM2)
ars[ars$Pollutant %in% dma.pollutant$Pollutant & ars$Season == 'Undefined','Season'] <- 'Year Round'
ars[ars$Pollutant == 'Phosphate Phosphorus','Season'] <- 'Year Round'
dma.pollutant$RIVER_MILE <- as.numeric(dma.pollutant$RIVER_MILE)
dma.pollutant[is.na(dma.pollutant$RIVER_MILE),'RIVER_MILE'] <- 0
dma.pollutant$GroupReview <- NA
dma.pollutant$MatrixReview <- NA

dma.pollutant.pre.assign <- dma.pollutant

#then we look to see which segments have stations within their bounds
for (i in 1:nrow(dma.pollutant)) {
  matched.seg <- subset(ars, ars$LLID_Stream_Lake == dma.pollutant$LLID_Stream_Lake[i])
  matched.seg.pol <- subset(matched.seg, matched.seg$Pollutant %in% c(dma.pollutant$Pollutant[i], dma.pollutant$Former_Group_Pollutant_Name[i]))
  #matched.seg.pol.seas <- subset(matched.seg.pol, matched.seg.pol$Season == dma.pollutant$Season[i])
  #matched.seg.pol.seas.crit <- subset(matched.seg.pol.seas, matched.seg.pol.seas$Criteria == dma.pollutant$Criteria[i])
  
  
  if (nrow(matched.seg.pol) > 0) {
    for (j in 1:nrow(matched.seg.pol)) {
      if(dma.pollutant$RIVER_MILE[i] >= matched.seg.pol$RM1[j] & dma.pollutant$RIVER_MILE[i] <= matched.seg.pol$RM2[j]) {
        
        if (matched.seg.pol$Action[j] != 'Added to database') {
          dma.pollutant$SegmentID[i] <- matched.seg.pol$Segment_ID[j]
          if (dma.pollutant$Pollutant[i] == matched.seg.pol$Pollutant[j]) {
            dma.pollutant$RecordID[i] <- matched.seg.pol$Record_ID[j]
          } 
          else {
            dma.pollutant$GroupReview[i] <- 'Check group match'
          }
        }
                
        if (dma.pollutant$SampleMatrix_ID[i] != matched.seg.pol$SampleMatrix_ID[j])
        {
          dma.pollutant$RecordID[i] <- NA
          dma.pollutant$MatrixReview[i] <- 'Sample Matrix Mismatch'
        }
        
      } 
    } 
  } else {
    dma.pollutant$SegmentID[i] <- NA
    dma.pollutant$RecordID[i] <- NA
  }
}

stations.newrecs <- dma.pollutant[is.na(dma.pollutant$RecordID),]
stations.existrecs <- dma.pollutant[!is.na(dma.pollutant$RecordID),]

library(plyr)
require(RODBC)

pHdata<-'I:/2012_WQAssessment/ToxicsRedo/allph.RData'
load(pHdata) #pH
pH$STATION<-pH$SampleRegID
pH$Date<-substr(pH$Sampled, 1, 10)
sul<-sul2012
pH<-pH[!(pH$STATION=='13417' & pH$Date == '2007-03-14' & pH$tResult == 3),]
ph.agg<-ddply(pH, .(STATION, Date), summarize, phmin = min(tResult), phmax=max(tResult))
ph2012<-join(sul[,c('STATION', 'DESCRIPTION','Water_Type')], ph.agg, 'STATION', type='inner')
ph2012<-ph2012[ph2012$Water_Type != 'FW',]
mc<-c(min=7, max=8.5)
ec<-c(min=6.5, max=8.5)
ph2012$phMinStatus[ph2012$Water_Type=='ES']<-ifelse(ph2012$phmin[ph2012$Water_Type=='ES']>ec['max'], 1,
                                                    ifelse(ph2012$phmin[ph2012$Water_Type=='ES']<ec['min'], -1,
                                                           0))
ph2012$phMaxStatus[ph2012$Water_Type=='ES']<-ifelse(ph2012$phmax[ph2012$Water_Type=='ES']>ec['max'], 1,
                                             ifelse(ph2012$phmax[ph2012$Water_Type=='ES']<ec['min'], -1,
                                                    0))
ph2012$phMaxStatus[ph2012$Water_Type=='SW']<-ifelse(ph2012$phmax[ph2012$Water_Type=='SW']>mc['max'], 1,
                                             ifelse(ph2012$phmax[ph2012$Water_Type=='SW']<mc['min'], -1,
                                                    0))
ph2012$phMinStatus[ph2012$Water_Type=='SW']<-ifelse(ph2012$phmin[ph2012$Water_Type=='SW']>mc['max'], 1,
                                                ifelse(ph2012$phmin[ph2012$Water_Type=='SW']<mc['min'], -1,
                                                       0))

ph2012$Outside.Range.Count<-ifelse(ph2012$phMaxStatus == ph2012$phMinStatus, ph2012$phMaxStatus^2,
                                   ph2012$phMaxStatus^2+ph2012$phMinStatus^2)

ph2012$CritMin[ph2012$Water_Type=='ES']<-ec['min']
ph2012$CritMin[ph2012$Water_Type=='SW']<-mc['min']
ph2012$CritMax[ph2012$Water_Type=='ES']<-ec['max']
ph2012$CritMax[ph2012$Water_Type=='SW']<-mc['max']
ph2012$Low.exceed<-ifelse(rowSums(ph2012[,c('phMinStatus', 'phMaxStatus')])<0, 1, 0)
ph2012$High.exceed<-ifelse(rowSums(ph2012[,c('phMinStatus', 'phMaxStatus')])>0, 1, 0)
ph2012.not.meeting<-ph2012[ph2012$Outside.Range.Count>0 & !is.na(ph2012$Outside.Range.Count),!names(ph2012) %in% c('phMinStatus', 'phMaxStatus', 'Outside.Range.Count')]
ph2012.odd.results<-ph2012[ph2012$Outside.Range.Count>1 & !is.na(ph2012$Outside.Range.Count),]
ph2012.sampleDay.different<-ph2012[ph2012$phMaxStatus != ph2012$phMinStatus & !is.na(ph2012$Outside.Range.Count),]
View(pH[pH$STATION %in% ph2012.not.meeting$STATION & pH$Date %in% ph2012.not.meeting$Date,])
View(pH[pH$STATION == '11241' & pH$Date == '2011-03-07',])

ph.summary<-ddply(ph2012, .(STATION, DESCRIPTION), summarize, Outside.Range=sum(Outside.Range.Count), 
                  Low.exceed=sum(Low.exceed), High.exceed=sum(High.exceed),
                  Inside.Range=(length(Outside.Range.Count)-sum(Outside.Range.Count)),
                  n=length(phMaxStatus))
ph.not.meeting<-ph.summary[!is.na(ph.summary$Outside.Range) &ph.summary$Outside.Range > 0,]
ph.not.meeting<-join(ph.not.meeting, ph2012[,c('STATION', 'CritMin', 'CritMax', 'Water_Type')], 'STATION', match='first')

accdb<-'//deqhq1/WQAssessment/2012_WQAssessment/2012_WorkingTables.mdb'
con<-odbcConnectAccess2007(accdb)
sqlSave(con, ph.not.meeting, 'pH_2012')
sqlSave(con, pH, 'pH_2012_data')

test<-sul[sul$STATION=='36228',c('STATION', 'STREAM_LLID')]
sqlUpdate(con, test, 'dbo_StationUseList_2012','STATION')

write.csv(ph.not.meeting, 'I:/2012_WQAssessment/ToxicsRedo/pH2012.csv', row.names=F)
write.csv(pH, 'I:/2012_WQAssessment/ToxicsRedo/pH2012_data.csv', row.names=F)

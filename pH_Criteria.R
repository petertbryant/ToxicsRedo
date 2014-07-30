library(plyr)
require(RODBC)
require(lubridate)


#Get sul from Stations_Use_List.R
pHdata<-'I:/2012_WQAssessment/ToxicsRedo/allph.RData'
load(pHdata) #pH
pH$STATION<-pH$SampleRegID
pH$Date<-as.Date(substr(pH$Sampled, 1, 10))
pH$Season<-ifelse(month(pH$Date) %in% 6:9, 'S', 'FWS')
pH$stationSeason<-paste(pH$STATION, pH$Season, sep='')
pH$Date<-as.character(pH$Date)
sul<-sul2012
pH<-pH[!(pH$STATION=='13417' & pH$Date == '2007-03-14' & pH$tResult == 3),] #Remove outlier
ph.agg<-ddply(pH, .(STATION, Date), summarize, phmin = min(tResult), phmax=max(tResult))
ph2012<-join(sul[,c('STATION', 'DESCRIPTION','Water_Type')], ph.agg, 'STATION', type='inner')
ph2012<-ph2012[ph2012$Water_Type != 'FW',]
ph2012$Season<-ifelse(month(ph2012$Date) %in% 6:9, 'S', 'FWS')
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

ph.allyear<-ddply(ph2012, .(STATION, DESCRIPTION), summarize, Outside.Range=sum(Outside.Range.Count), 
                  Low.exceed=sum(Low.exceed), High.exceed=sum(High.exceed),
                  Inside.Range=(length(Outside.Range.Count)-sum(Outside.Range.Count)),
                  n=length(phMaxStatus), pcnt.exceed=round(sum(Outside.Range.Count)*100/length(phMaxStatus), 1))
ph.ay.not.meeting<-ph.allyear[!is.na(ph.allyear$Outside.Range) & ph.allyear$Outside.Range > 0,]
ph.ay.not.meeting<-arrange(join(ph.ay.not.meeting, ph2012[,c('STATION', 'CritMin', 'CritMax', 'Water_Type')], 
                                'STATION', match='first'),
                           desc(Outside.Range))
# ph.ay.not.meeting$Cat<-ifelse((ph.ay.not.meeting$n >= 10 & ph.ay.not.meeting$pcnt.exceed < 10) |
#                                 (ph.ay.not.meeting$n %in% 5:9 & ph.ay.not.meeting$Outside.Range == 0), '2',
#                               ifelse((ph.ay.not.meeting$n >= 10 & ph.ay.not.meeting$pcnt.exceed >= 10) |
#                                        (ph.ay.not.meeting$n %in% 5:9 & ph.ay.not.meeting$Outside.Range >= 2), '5',
#                                      ifelse((ph.ay.not.meeting$n < 5 & ph.ay.not.meeting$Outside.Range >= 2) |
#                                               (ph.ay.not.meeting$n %in% 5:9 & ph.ay.not.meeting$Outside.Range >= 2), '5',)))


ph.seasonal<-ddply(ph2012, .(STATION, DESCRIPTION, Season), summarize, Outside.Range=sum(Outside.Range.Count), 
                  Low.exceed=sum(Low.exceed), High.exceed=sum(High.exceed),
                  Inside.Range=(length(Outside.Range.Count)-sum(Outside.Range.Count)),
                  n=length(phMaxStatus), pcnt.exceed=round(sum(Outside.Range.Count)*100/length(phMaxStatus), 1))
ph.s.not.meeting<-ph.seasonal[!is.na(ph.seasonal$Outside.Range) & ph.seasonal$Outside.Range > 0,]
ph.s.not.meeting<-arrange(join(ph.s.not.meeting, ph2012[,c('STATION', 'CritMin', 'CritMax', 'Water_Type')], 
                               'STATION', match='first'),
                          Season, desc(Outside.Range))
ph.s.not.meeting$stationSeason<-paste(ph.s.not.meeting$STATION, ph.s.not.meeting$Season, sep='')


accdb<-'//deqhq1/WQAssessment/2012_WQAssessment/2012_WorkingTables.mdb'
con<-odbcConnectAccess2007(accdb)
# sqlSave(con, ph.not.meeting, 'pH_2012')
sqlSave(con, pH, 'pH_2012_data')
sqlSave(con, ph.ay.not.meeting, 'pH_2012_AllYear')
sqlSave(con, ph.s.not.meeting, 'pH_2012_Seasonal')

test<-sul[sul$STATION=='36228',c('STATION', 'STREAM_LLID')]
sqlUpdate(con, test, 'dbo_StationUseList_2012','STATION')

write.csv(ph.not.meeting, 'I:/2012_WQAssessment/ToxicsRedo/pH2012.csv', row.names=F)
write.csv(pH, 'I:/2012_WQAssessment/ToxicsRedo/pH2012_data.csv', row.names=F)

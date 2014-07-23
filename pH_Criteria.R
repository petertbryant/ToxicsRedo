library(plyr)

pHdata<-'I:/2012_WQAssessment/ToxicsRedo/allph.RData'
load(pHdata) #pH
pH$STATION<-pH$SampleRegID
sul<-stUseList2012[stUseList2012$USE_Final==1,]
ph.agg<-ddply(pH, .(STATION), summarize, phmin = min(tResult), phmax=max(tResult))
ph2012<-join(sul, ph.agg, 'STATION', type='inner')
mc<-c(min=7, max=8.5)
ec<-c(min=6.5, max=8.5)
ph2012$phStatus[ph2012$MATRIX=='ES']<-ifelse(ph2012$phmax[ph2012$MATRIX=='ES']>ec['max'], 'Over',
                                             ifelse(ph2012$phmax[ph2012$MATRIX=='ES']<ec['min'], 'Under',
                                                    'Meets'))
ph2012$phStatus[ph2012$MATRIX=='SW']<-ifelse(ph2012$phmax[ph2012$MATRIX=='SW']>mc['max'], 'Over',
                                             ifelse(ph2012$phmax[ph2012$MATRIX=='SW']<mc['min'], 'Under',
                                                    'Meets'))
ph2012$phStatus[ph2012$MATRIX=='FW']<-'Freshwater'
unique(ph2012$phStatus)
View(ph2012[ph2012$phStatus == 'Over',])

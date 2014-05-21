#Does this work?
#There are many duplicate stations. Let's see how many there are and how much overlapping data they have.

#Some sampling sites are labeled "Test Submission". Investigate these.
mli<-levels(as.factor(wqp.data$MonitoringLocationIdentifier))
mli.test<-mli[grep("NARSTEST", mli)]
mli.test.len<-sapply(mli.test, function(x){nrow(wqp.data[wqp.data[[1]]==x,])})
summary(mli.test.len)
#Test submissions have no more than 8 rows of data associated with them

#There seem to be "real" submissions at the location of each test submission.
#See how much data is associated with them.

mli.real<-paste("NARS_WQX", substr(mli.test, 9, 18), sep="")
mli.real.len<-sapply(mli.real, function(x){nrow(wqp.data[wqp.data[[1]]==x,])})
summary(mli.real.len)

#Check for duplicate stations by comparing unique combinatios of Datum, Lat, and Lon. 
col.nums<-sapply(c("Latitude", "Longitude", "HorizontalCoordinate"), grep, names(not.usgs))
not.usgs$spatial.id<-paste(not.usgs[[18]], not.usgs[[13]], not.usgs[[12]],sep="_")

not.usgs.dups<-not.usgs[not.usgs$spatial.id %in% not.usgs[duplicated(not.usgs$spatial.id),'spatial.id'],
                        c('MonitoringLocationIdentifier','spatial.id')]

ndups<-split(not.usgs.dups, as.factor(not.usgs.dups$spatial.id))
nrow(ndups)

#Create new df with duplicate station data, and remove duplicate rows.
wqp.dup.data<-wqp.data[wqp.data[['MonitoringLocationIdentifier']] %in% not.usgs.dups[['MonitoringLocationIdentifier']],]
#Assign unique ids for independent measurements. This field will be used to identify duplicates
wqp.dup.data$unique.id<-gsub("\\s", "", paste(wqp.dup.data[['ActivityStartDate']], wqp.dup.data[['ActivityDepthHeightMeasureMeasureValue']], 
                                              wqp.dup.data[['ProjectIdentifier']], wqp.dup.data[['SampleCollectionMethodMethodIdentifier']], 
                                              wqp.dup.data[['SampleCollectionEquipmentName']], wqp.dup.data[['CharacteristicName']], 
                                              wqp.dup.data[['ResultSampleFractionText']], wqp.dup.data[['ResultMeasureValue']], 
                                              wqp.dup.data[['ResultMeasureMeasureUnitCode']], wqp.dup.data[['ResultStatusIdentifier']], 
                                              wqp.dup.data[['ResultValueTypeName']], wqp.dup.data[['ResultCommentText']], sep="_"))


wqp.dup.data.lst<-lapply(ndups, function(x){wqp.dup.data[wqp.dup.data[['MonitoringLocationIdentifier']] %in% x[['MonitoringLocationIdentifier']],]})
lst.nrow<-sapply(wqp.dup.data.lst, nrow)
summary(lst.nrow)
lapply(wqp.dup.data.lst, function(x){duplicated(x$unique.id)})
wqp.dup.data.lst[['WGS84_-122.95999999999999_45.484720000000003']]$'MonitoringLocationIdentifier'
wqp.dup.data.lst[['WGS84_-122.95999999999999_45.484720000000003']]$'unique.id'


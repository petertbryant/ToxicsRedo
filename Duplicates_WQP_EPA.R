#Check for duplicate stations by comparing unique combinations of Datum, Lat, and Lon. 
col.nums<-sapply(c("Latitude", "Longitude", "HorizontalCoordinate"), grep, names(not.usgs))
not.usgs$spatial.id<-paste(not.usgs[[18]], not.usgs[[13]], not.usgs[[12]],sep="_")

#Excracts the location identifier and spatial id for each row that has a duplicate spatial location
not.usgs.dups<-not.usgs[not.usgs$spatial.id %in% not.usgs[duplicated(not.usgs$spatial.id),'spatial.id'],
                        c('MonitoringLocationIdentifier','spatial.id')]

#Splits df of spatial dupliates into a list of each spatially unique location
ndups<-split(not.usgs.dups, as.factor(not.usgs.dups$spatial.id))
length(ndups)

#Create new df with duplicate station data, and remove duplicate rows.

#Assign temporary unique id to every row in the complete dataset
wqp.data$Temp.id<-paste("tempID", seq(1:nrow(wqp.data)), sep="_")

#Subset complete dataset so I'm only working with duplicated rows
wqp.dup.data<-wqp.data[wqp.data[['MonitoringLocationIdentifier']] %in% not.usgs.dups[['MonitoringLocationIdentifier']],]

#Assign unique ids for independent measurements. This field will be used to identify duplicate wq data
wqp.dup.data$unique.meas.id<-gsub("\\s", "", paste(wqp.dup.data[['ActivityStartDate']], wqp.dup.data[['ActivityDepthHeightMeasureMeasureValue']], 
                                              wqp.dup.data[['ProjectIdentifier']], wqp.dup.data[['SampleCollectionMethodMethodIdentifier']], 
                                              wqp.dup.data[['SampleCollectionEquipmentName']], wqp.dup.data[['CharacteristicName']], 
                                              wqp.dup.data[['ResultSampleFractionText']], wqp.dup.data[['ResultMeasureValue']], 
                                              wqp.dup.data[['ResultMeasureMeasureUnitCode']], wqp.dup.data[['ResultStatusIdentifier']], 
                                              wqp.dup.data[['ResultValueTypeName']], wqp.dup.data[['ResultCommentText']], sep="_"))

#For each spatially duplicated location, pull all the data for that location
wqp.dup.data.lst<-lapply(ndups, function(x){wqp.dup.data[wqp.dup.data[['MonitoringLocationIdentifier']] %in% x[['MonitoringLocationIdentifier']],]})

#How many rows of data does each location have?
lst.nrow<-sapply(wqp.dup.data.lst, nrow)
summary(lst.nrow)
lst.duplicated<-lapply(wqp.dup.data.lst, function(x){duplicated(x$unique.meas.id)})
#How many duplicates need removal?
length(unlist(lst.duplicated)[unlist(lst.duplicated)])

# wqp.dup.data.lst[['WGS84_-122.95999999999999_45.484720000000003']]$'MonitoringLocationIdentifier'
# wqp.dup.data.lst[['WGS84_-122.95999999999999_45.484720000000003']]$'unique.meas.id'
# lst.duplicated[['WGS84_-122.95999999999999_45.484720000000003']]

#remove duplicates
rows.to.remove<-unlist(sapply(wqp.dup.data.lst, function(x){x$Temp.id[which(duplicated(x$unique.meas.id))]}))
wqp.data.dups.removed<-wqp.data[-which(wqp.data$Temp.id %in% rows.to.remove),]
wqp.data2<-wqp.data.dups.removed

#Check results

#Subset modified dataset so I'm only working with duplicated rows
wqp.dup.data2<-wqp.data2[wqp.data2[['MonitoringLocationIdentifier']] %in% not.usgs.dups[['MonitoringLocationIdentifier']],]

#Assign unique ids for independent measurements. This field will be used to identify duplicate wq data
wqp.dup.data2$unique.meas.id<-gsub("\\s", "", paste(wqp.dup.data2[['ActivityStartDate']], wqp.dup.data2[['ActivityDepthHeightMeasureMeasureValue']], 
                                                   wqp.dup.data2[['ProjectIdentifier']], wqp.dup.data2[['SampleCollectionMethodMethodIdentifier']], 
                                                   wqp.dup.data2[['SampleCollectionEquipmentName']], wqp.dup.data2[['CharacteristicName']], 
                                                   wqp.dup.data2[['ResultSampleFractionText']], wqp.dup.data2[['ResultMeasureValue']], 
                                                   wqp.dup.data2[['ResultMeasureMeasureUnitCode']], wqp.dup.data2[['ResultStatusIdentifier']], 
                                                   wqp.dup.data2[['ResultValueTypeName']], wqp.dup.data2[['ResultCommentText']], sep="_"))

#For each spatially duplicated location, pull all the modified data for that location
wqp.dup.data2.lst<-lapply(ndups, function(x){wqp.dup.data2[wqp.dup.data2[['MonitoringLocationIdentifier']] %in% x[['MonitoringLocationIdentifier']],]})

#How many rows of data does each location have?
lst.nrow2<-sapply(wqp.dup.data2.lst, nrow)
summary(lst.nrow2)
lst.duplicated2<-lapply(wqp.dup.data2.lst, function(x){duplicated(x$unique.meas.id)})
#How many duplicates need removal?
length(unlist(lst.duplicated2)[unlist(lst.duplicated2)])

lst.duplicated[['NAD83_-124.40000000000001_43.100000000000001']]
wqp.dup.data.lst[['NAD83_-124.40000000000001_43.100000000000001']]
wqp.dup.data2.lst[['NAD83_-124.40000000000001_43.100000000000001']]

#Define new station names
new.stations<-merge(data.frame(x=unique(not.usgs$MonitoringLocationIdentifier)), data.frame(x=unique(wqp.data2$MonitoringLocationIdentifier)))

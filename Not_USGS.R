options(stringsAsFactors = FALSE)

#wqp.stations <- read.csv('//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/wqpstations_postQC_05192014.csv')
not.usgs <- wqp.stations[-grep('USGS',wqp.stations$MonitoringLocationIdentifier),]
# write.csv(not.usgs, 
#           '//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/Not_USGS_wqpstations_postQC_05192014.csv',
#           row.names=F)

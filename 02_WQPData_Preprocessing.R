#This file is to pre process the WQP data to handle flags, NA's, duplicates, and to make sure we've got everything we should

library(RODBC)

options(stringsAsFactors = FALSE)

con <- odbcConnect('WQAssessment')

wqp.data <- sqlFetch(con, 'WQPData_05022014')
wqp.stations <- sqlFetch(con, 'WQPStations_05052014')
st <- sqlFetch(con, 'WQPQueryStatus_05052014')

str(wqp.data)

#There seem to be a lot of NAs in this field. 
table(wqp.data$ResultMeasureValue, useNA = "always") #Need to discern if they are ND's or what
table(wqp.data[is.na(wqp.data$ResultMeasureValue),'MeasureQualifierCode'],useNA = 'always') #No qualifiers associated
wqp.data <- wqp.data[!is.na(wqp.data$ResultMeasureValue),]

#looks like there's some groundwater that crept in
wqp.data <- wqp.data[wqp.data$ActivityMediaSubdivisionName != 'Groundwater',]

#look at who collected these data
table(wqp.data$OrganizationFormalName)

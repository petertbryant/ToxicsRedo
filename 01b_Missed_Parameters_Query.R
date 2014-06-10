library(RCurl)
library(XML)
library(RODBC)
library(stringr)

options(stringsAsFactors = FALSE)

source('//deqhq1/wqassessment/2012_wqassessment/toxicsredo/wqpquery_functions.R')

#### Define state to query ####
#you can use the wqp.domain.get if you want outside Oregon but here is the Oregon code
Oregon <- 'US%3A41'

#### Define site types to query ####
#wqp.siteTypes <- WQP.domain.get('Sitetype')

#Using the wqp.siteTypes enter the values you want to query for in siteType.
#Separate each value with the URL encoded semi-colon '%3B'. For values with commas use the URL encoded value '%2C+'
siteType = 'Estuary%3BOcean%3BStream%3BStream%3BLake%2C+Reservoir%2C+Impoundment'

#### Define sample media to query ####
#wqp.sampleMedia <- WQP.domain.get('Samplemedia')

#Separate each value you want to query with the URL encoded semi-colon '%3B'.
phos.sampleMedia <- 'Water'
hg.sampleMedia <- 'Biological%3bBiological+Tissue'

#### Define characteristics to query ####
#First get the list of Characteristic names from the WQP. These names are consistent with EPA's SRS. 
#wqp.characteristics <- WQP.domain.get('Characteristicname')

#wqp.characteristics[grep('[Pp]hosphate',wqp.characteristics$value),]
phos <- c('Orthophosphate',
          'Orthophosphate as P',
          'Phosphate',
          'Phosphate-phosphorus',
          'Phosphate-phosphorus as P',
          'Polyphosphate',
          'Polyphosphate as P')

#wqp.characteristics[grep('[Mm]ercury',wqp.characteristics$value),]
hg.wqp <- c('Mercury','Methylmercury(1+)')

#### Define start and end date ####
#The expected format is mm-dd-yyyy
startDate <- '01-01-2000'
endDate <- '12-31-2011'


tmp.stations <- wqp.station.query(stateCode = Oregon, 
                                  siteType = siteType, 
                                  sampleMedia = phos.sampleMedia, 
                                  characteristicName = phos, 
                                  startDate = startDate, 
                                  endDate = endDate)


#### LASAR Query ####

#LASAR phosphate keys
phos <- c('Orthophosphate as P' = '10970','Orthophosphate as PO4' = '5232')

#Calcium, magnesium and hardness
hard <- c('Calcium' = '776','Total Recoverable Hardness as Calcium Carbonate (Calculated)' = '9676',
          'Hardness as Calcium Carbonate' = '9680', 'Total Hardness as Calcium Carbonate (Calculated)' = '9683',
          'Magnesium' = '1488', 'Hardness as CaCO3' = '5188')

#pH
ph <- c('pH' = '1332')


#mercury in fish tissue
hg <- c('Mercury' = '1507', 'Methyl Mercury' = '6464')

#surface water sample matrices
sw <- c('Surface water', 'Bay/Estuary/Ocean', 'Canal', 'Reservoir', 'Lake',
 'Ditch/Pond/Culvert/Drain')

#fish tissue matrices
ft <- c('Tissue, Fish','Whole body','Fillet, with skin','Fillet, no skin')

#Used in the next function
get.cases <- function(chk.values) {
  ## Checks for non-numeric values in the vector "chk.values", which should
  ## be a character vector. A data.frame is returned with the non-numeric
  ## values (cases) and the number of occurrences for each case. If there
  ## are olnly numeric values in the input vectore, the entries in the 
  ## data.frame returned are "No non-numeric values found" for the case
  ## and NA for the count
  ## Created by Kevin Brannan
  ## Version 1.0.0.09.20.2012
  tmp.cases <- chk.values[grep("[^0-9.]",chk.values)][!duplicated(chk.values[grep("[^0-9.]",chk.values)])]
  if(length(tmp.cases) > 0){
    tmp.cases.report <- data.frame(Case = tmp.cases,Count=as.numeric(NA))
    for(ii in 1:length(tmp.cases)){
      tmp.cases.report$Count[ii] <- length(grep(tmp.cases.report$Case[ii],chk.values))
    }
  } else{
    tmp.cases.report <- data.frame("No non-numeric values found",NA)
    names(tmp.cases.report) <- c("Case","Count")
  }
  return(tmp.cases.report)
}

#put it into a function so i don't have to repeat it three times ###Need to take out join on analytical method for mercury to work. Probably 
#affects all my previous queries too! Shucks!
lasar.query <- function(parms, sampleMatrix, stations = c()) { 

ifelse(length(stations) == 0,
       query <- paste("SELECT pr.[PARAMETER_KEY],
  pr.[Result],
  p.[NAME],
  pm.[ABBREVIATION],
  pm.ABBREVIATION + p.[NAME] as 'Name.full',
  s.[SAMPLE_DATE],
  s.[SAMPLE_TIME],
  s.[STATION_KEY],
  sn.[LOCATION_DESCRIPTION],
  sm.SAMPLE_MATRIX,
  pr.METHOD_DETECTION_LIMIT,
  pr.METHOD_REPORTING_LIMIT,
  q.QA_QC_TYPE,
  st.STATUS,
  u.UNIT,
  ss.SUBPROJECT_NAME,
  am.METHOD
FROM [LASAR].[dbo].[PARAMETER_RESULT] pr JOIN [LASAR].[dbo].[PARAMETER] p on 
  pr.PARAMETER_KEY = p.PARAMETER_KEY JOIN
  [LASAR].[dbo].[PARAMETER_MODIFIER] pm on 
  pr.PARAMETER_PREFIX_1 = pm.MODIFIER_KEY JOIN
  [LASAR].dbo.ANALYTICAL_METHOD am on
  pr.ANALYTICAL_METHOD_KEY = am.ANALYTICAL_METHOD_KEY JOIN 
  [LASAR].[dbo].[SAMPLE] s on
  pr.SAMPLE_KEY = s.SAMPLE_KEY JOIN
  [LASAR].dbo.XLU_QA_QC_TYPE q on
  q.QA_QC_TYPE_KEY = pr.QA_QC_TYPE JOIN
  [LASAR].dbo.XLU_STATUS st on
  st.XLU_STATUS_KEY = pr.QA_QC_STATUS JOIN
  [LASAR].dbo.UNIT u on
  u.UNIT_KEY = pr.UNIT_KEY JOIN 
  [LASAR].dbo.STATION sn on
  sn.STATION_KEY = s.STATION_KEY JOIN 
  [LASAR].dbo.SAMPLING_SUBPROJECT ss on 
  ss.SAMPLING_SUBPROJECT_KEY = s.SAMPLING_SUBPROJECT_KEY JOIN
  [LASAR].[dbo].[SAMPLE_MATRIX] sm on 
  sm.SAMPLE_MATRIX_KEY = pr.SAMPLE_MATRIX_KEY
WHERE s.SAMPLE_DATE > '2000-01-01 00:00:00.000' and 
  s.SAMPLE_DATE < '2011-12-31 00:00:00.000' and
  s.STATION_KEY != '10000' and 
  st.STATUS in ('A','A+','B') and 
  sm.SAMPLE_MATRIX in (", paste(sampleMatrix, collapse = "','"), ") and
  p.Name in (", paste(parms, collapse="','"), ") 
Order by s.STATION_KEY, s.SAMPLE_DATE;", sep = "'"),
       query <- paste("SELECT pr.[PARAMETER_KEY],
  pr.[Result],
  p.[NAME],
  pm.[ABBREVIATION],
  pm.ABBREVIATION + p.[NAME] as 'Name.full',
  s.[SAMPLE_DATE],
  s.[SAMPLE_TIME],
  s.[STATION_KEY],
  sn.[LOCATION_DESCRIPTION],
  sm.SAMPLE_MATRIX,
  pr.METHOD_DETECTION_LIMIT,
  pr.METHOD_REPORTING_LIMIT,
  q.QA_QC_TYPE,
  st.STATUS,
  u.UNIT,
  ss.SUBPROJECT_NAME,
  am.METHOD
FROM [LASAR].[dbo].[PARAMETER_RESULT] pr JOIN [LASAR].[dbo].[PARAMETER] p on 
  pr.PARAMETER_KEY = p.PARAMETER_KEY JOIN
  [LASAR].[dbo].[PARAMETER_MODIFIER] pm on 
  pr.PARAMETER_PREFIX_1 = pm.MODIFIER_KEY JOIN
  [LASAR].dbo.ANALYTICAL_METHOD am on
  pr.ANALYTICAL_METHOD_KEY = am.ANALYTICAL_METHOD_KEY JOIN 
  [LASAR].[dbo].[SAMPLE] s on
  pr.SAMPLE_KEY = s.SAMPLE_KEY JOIN
  [LASAR].dbo.XLU_QA_QC_TYPE q on
  q.QA_QC_TYPE_KEY = pr.QA_QC_TYPE JOIN
  [LASAR].dbo.XLU_STATUS st on
  st.XLU_STATUS_KEY = pr.QA_QC_STATUS JOIN
  [LASAR].dbo.UNIT u on
  u.UNIT_KEY = pr.UNIT_KEY JOIN 
  [LASAR].dbo.STATION sn on
  sn.STATION_KEY = s.STATION_KEY JOIN 
  [LASAR].dbo.SAMPLING_SUBPROJECT ss on 
  ss.SAMPLING_SUBPROJECT_KEY = s.SAMPLING_SUBPROJECT_KEY JOIN
  [LASAR].[dbo].[SAMPLE_MATRIX] sm on 
  sm.SAMPLE_MATRIX_KEY = pr.SAMPLE_MATRIX_KEY
WHERE s.SAMPLE_DATE > '2000-01-01 00:00:00.000' and 
  s.SAMPLE_DATE < '2011-12-31 00:00:00.000' and
  s.STATION_KEY != '10000' and 
  s.STATION_KEY in (", paste(stations, collapse = "','"), ") and
  st.STATUS in ('A','A+','B') and 
  sm.SAMPLE_MATRIX in (", paste(sampleMatrix, collapse = "','"), ") and
  p.Name in (", paste(parms, collapse="','"), ") 
Order by s.STATION_KEY, s.SAMPLE_DATE;", sep = "'"))
  
con <- odbcConnect('LASAR')
tmp <- sqlQuery(con, query)
odbcCloseAll()

tmp$Result <- str_trim(tmp$Result)
report <- get.cases(tmp$Result)

lst.split <- strsplit(as.character(report$Case), split = ' ')
for (i in 1:length(lst.split)){
  lst.split[[i]][1] <- ifelse(substr(lst.split[[i]][1],1,1) == '<',substr(lst.split[[i]][1],2,nchar(lst.split[[i]][1])),lst.split[[i]][1])
  report$Sub[i] <- ifelse(is.na(as.numeric(str_trim(lst.split[[i]][1]))), 
                          ifelse(substr(str_trim(lst.split[[i]][1]),1,1) == '<','ND',NA),
                          as.numeric(lst.split[[i]][1]))
}

# report[report$Case == '< 0.5','Sub'] <- 0.5
# report[report$Case == '< 10 Est','Sub'] <- 10
# report[report$Case == '< 10','Sub'] <- 10
# report[report$Case == '<0.001est','Sub'] <- 0.001
# report[report$Case == '<0.003est','Sub'] <- 0.003
# report[report$Case == '<0.002est','Sub'] <- 0.002
# report[report$Case == '<0.004est','Sub'] <- 0.004
# report[report$Case == '>0.6','Sub'] <- 0.6  
# report[report$Case == '>1','Sub'] <- 1

Result_clean <- sub.cases(tmp$Result, report) 
tmp <- cbind(tmp, Result_clean)
tmp <- tmp[!is.na(tmp$Result_clean),]

return(tmp)

}

phos.lasar <- lasar.query(names(phos))
ammon.lasar <- lasar.query(parms = 'Ammonia as N', sampleMatrix = sw)
ph.lasar <- lasar.query(parms = c('pH'), 
                        sampleMatrix = sw, 
                        stations = c(unique(ammon.lasar$STATION_KEY),unique(lasar.og[lasar.og$NAME == 'Pentachlorophenol','STATION_KEY'])))
temp.lasar <- lasar.query(parms = 'Temperature', sampleMatrix = sw, stations = unique(ammon.lasar$STATION_KEY))
hg.ft.lasar <- lasar.query(parms = names(hg), sampleMatrix = ft)

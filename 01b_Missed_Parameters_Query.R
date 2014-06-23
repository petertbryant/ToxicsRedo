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
sampleMedia <- 'Water'

#### Define characteristics to query ####
#First get the list of Characteristic names from the WQP. These names are consistent with EPA's SRS. 
wqp.characteristics <- WQP.domain.get('Characteristicname')

#wqp.characteristics[grep('[Ss]alinity|[Cc]onducti',wqp.characteristics$value),]
consal <- URLencode.PTB(paste(c('Conductivity','Salinity','Specific conductivity'),collapse=';'))

#wqp.characteristics[grep('[Pp]hosphate',wqp.characteristics$value),]
phos <- c('Orthophosphate as P',
          'Phosphate-phosphorus',
          'Phosphate-phosphorus as P',
          'Polyphosphate as P')

#wqp.characteristics[grep('[Mm]ercury',wqp.characteristics$value),]
hg.wqp <- c('Mercury','Methylmercury(1+)')

#### Define start and end date ####
#The expected format is mm-dd-yyyy
startDate <- '01-01-2000'
endDate <- '12-31-2011'

#Pull in stations to query for salinity and conductivity
consal.stations.file <- read.csv('Estuary_Analysis/stations_needing_salinity_data.csv', stringsAsFactors = FALSE)
consal.stations <- wqp.stations[wqp.stations$site_only %in% consal.stations.file$STATION,'MonitoringLocationIdentifier']
consal.stations <- URLencode.PTB(paste(consal.stations,collapse=";"))

#Query for conductivity and salinity
tmp.consal.data <- wqp.data.query.by.station(stateCode = Oregon, 
                                       siteType = siteType, 
                                       sampleMedia = sampleMedia, 
                                       characteristicName = consal, 
                                       startDate = startDate, 
                                       endDate = endDate,
                                       siteid = consal.stations)
write.csv(tmp.consal.data, 'Estuary_Analysis/WQP_consal.csv', row.names = FALSE)

#There are no ammonia data that made it past the QC checks from the WQP.

#Query for phosphorus data
tmp.phos.stations <- wqp.station.query(stateCode = Oregon, 
                                       siteType = siteType, 
                                       sampleMedia = phos.sampleMedia, 
                                       characteristicName = phos, 
                                       startDate = startDate, 
                                       endDate = endDate)

tmp.phos.data <- wqp.data.query(stateCode = Oregon, 
                                siteType = siteType, 
                                sampleMedia = phos.sampleMedia, 
                                characteristicName = phos, 
                                startDate = startDate, 
                                endDate = endDate)

#Due to prior QC review this is the only organization in this set that we want to keep data for
tmp.phos.stations <- tmp.phos.stations[tmp.phos.stations$OrganizationFormalName == 'National Park Service Water Resources Division',]
tmp.phos.data <- tmp.phos.data[tmp.phos.data$OrganizationFormalName == 'National Park Service Water Resources Division',]

#Query for mercury in fish tissue
tmp.hg.stations <- wqp.station.query(stateCode = Oregon, 
                                     siteType = siteType, 
                                     sampleMedia = hg.sampleMedia, 
                                     characteristicName = paste(hg.wqp,collapse=';'), 
                                     startDate = startDate, 
                                     endDate = endDate)

tmp.hg.data <- wqp.data.query(stateCode = Oregon, 
                              siteType = siteType, 
                              sampleMedia = hg.sampleMedia, 
                              characteristicName = paste(hg.wqp,collapse=';'), 
                              startDate = startDate, 
                              endDate = endDate)

tmp.hg.data <- tmp.hg.data[tmp.hg.data$SubjectTaxonomicName %in% c('Oncorhynchus clarkii', 'Oncorhynchus mykiss gairdnerii', 'Cottus', 'Cottus perplexus'),]
tmp.hg.data <- tmp.hg.data[tmp.hg.data$ResultDetectionConditionText != 'Detected Not Quantified',]

#### LASAR Query ####

#LASAR phosphate keys
#phos <- c('Orthophosphate as P' = '10970','Orthophosphate as PO4' = '5232')

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
                        ss.SUBPROJECT_NAME
                      FROM [LASAR].[dbo].[PARAMETER_RESULT] pr JOIN [LASAR].[dbo].[PARAMETER] p on 
                        pr.PARAMETER_KEY = p.PARAMETER_KEY JOIN
                        [LASAR].[dbo].[PARAMETER_MODIFIER] pm on 
                        pr.PARAMETER_PREFIX_1 = pm.MODIFIER_KEY JOIN
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
                        ss.SUBPROJECT_NAME
                        FROM [LASAR].[dbo].[PARAMETER_RESULT] pr JOIN [LASAR].[dbo].[PARAMETER] p on 
                        pr.PARAMETER_KEY = p.PARAMETER_KEY JOIN
                        [LASAR].[dbo].[PARAMETER_MODIFIER] pm on 
                        pr.PARAMETER_PREFIX_1 = pm.MODIFIER_KEY JOIN
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

return(tmp)

}

#Used in create.sub.table
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

#This is used to take the sub table and create a clean Result column
sub.cases <- function(data.in,sub.table){
  ## Replaces non-numeric values of data.in with the correspoinding elements
  ## in sub.table. The sub.table dataframe should be generated using the 
  ## get.cases function
  ## Created by Kevin Brannan
  ## Version 1.0.0.09.20.2012
  for(ii in 1:length(sub.table$Sub)){
    sub.index <- data.in == sub.table$Case[ii]  #grep(sub.table$Case[ii],data.in, fixed = TRUE)
    print(paste("Number of sub for ", sub.table$Case[ii], " is ",sub.table$Sub[ii],sep=""))
    if(length(sub.index)> 0){
      data.in[data.in == sub.table$Case[ii]] <- as.character(sub.table$Sub[ii])
      rm(sub.index)
    }
  }
  return(data.in)
}

#I do a little extra processing to make the use of get.cases easier
create.sub.table <- function(result_column) {
  result_column <- str_trim(result_column)
  report <- get.cases(result_column)
  
  lst.split <- strsplit(as.character(report$Case), split = ' ')
  for (i in 1:length(lst.split)){
    lst.split[[i]][1] <- ifelse(substr(lst.split[[i]][1],1,1) == '<',substr(lst.split[[i]][1],2,nchar(lst.split[[i]][1])),lst.split[[i]][1])
    report$Sub[i] <- ifelse(is.na(as.numeric(str_trim(lst.split[[i]][1]))), 
                            ifelse(substr(str_trim(lst.split[[i]][1]),1,1) == '<','ND',NA),
                            as.numeric(lst.split[[i]][1]))
  } 
  
  return(report)
}

#I do a little extra processing to make the use of sub.cases easier
make.clean <- function(tmp, report) {
  Result_clean <- sub.cases(tmp$Result, report) 
  tmp <- cbind(tmp, Result_clean)
  tmp <- tmp[!is.na(tmp$Result_clean),]
}

#Pulling pH for Ammonia and pentachlorophenol site specific criteria. 6/12/14 Karla noted that we will also want to pull
#pH data for Marine and estuary sites in repsonse to comments regarding ocean acidification
ph.lasar <- lasar.query(parms = c('pH'), 
                        sampleMatrix = sw, 
                        stations = unique(lasar[lasar$NAME %in% c('Ammonia as N', 'Pentachlorophenol'),'STATION_KEY']))
ph.report <- create.sub.table(ph.lasar$Result)
ph.lasar <- make.clean(ph.lasar, ph.report)

#Pulling temperature for ammonia site specific criteria
temp.lasar <- lasar.query(parms = 'Temperature', sampleMatrix = sw, stations = unique(lasar[lasar$NAME %in% c('Ammonia as N'),'STATION_KEY']))
temp.report <- create.sub.table(temp.lasar$Result)
temp.lasar <- make.clean(temp.lasar, temp.report)

#pulling mercury fish tissue data statewide
hg.ft.lasar <- lasar.query(parms = names(hg), sampleMatrix = ft)
hg.ft.lasar.report <- create.sub.table(hg.ft.lasar$Result)
hg.ft.lasar <- make.clean(hg.ft.lasar, hg.ft.lasar.report)

#conductivity and salinity
consal.lasar <- lasar.query(parms = c('Conductivity', 'Salinity'), sampleMatrix = sw, stations = consal.stations.file[nchar(consal.stations.file$STATION) == 5,'STATION'])
consal.report <- create.sub.table(consal.lasar$Result)
consal.lasar <- make.clean(consal.lasar, consal.report)
write.csv(consal.lasar, 'Estuary_Analysis/LASAR_consal.csv', row.names = FALSE)

#Save this complete lasar dataset to the database to make it easier for later analyses to run. Mostly
#so we don't have to re-run the queries of the lasar database every time.
# wq <- odbcConnect('WQAssessment')
# lasar.to.save <- lasar
# lasar.to.save$SAMPLE_DATE <- as.character(lasar.to.save$SAMPLE_DATE)
# lasar.to.save$SAMPLE_TIME <- substr(as.character(lasar.to.save$SAMPLE_TIME),12,19)
# sqlSave(wq, lasar.to.save, tablename = 'LASAR_Toxics_Query_wAddOns_06122014', rownames = FALSE)
# rm(lasar.to.save)
# odbcCloseAll()

#So it looks like I screwed up and when I went to add criteria names I didn't include the temperature and pH I had queried from lasar
#Here i query that table back in, add ph.lasar and temp.lasar to it and save it back to the database with a new name to use in the data prep
wq <- odbcConnect('WQAssessment')
lasar <- sqlFetch(wq, 'LASAR_Toxics_Query_wCriteriaNames_06132014')
#Add pH and temperature to the complete lasar dataset
ph.lasar$criterianame <- 'pH'
ph.lasar <- rename(ph.lasar, c('Name.full' = 'Namefull'))
ph.lasar$SAMPLE_DATE <- as.character(ph.lasar$SAMPLE_DATE)
ph.lasar$SAMPLE_TIME <- substr(as.character(ph.lasar$SAMPLE_TIME),12,19)
temp.lasar$criterianame <- 'Temperature'
temp.lasar <- rename(temp.lasar, c('Name.full' = 'Namefull'))
temp.lasar$SAMPLE_DATE <- as.character(temp.lasar$SAMPLE_DATE)
temp.lasar$SAMPLE_TIME <- substr(as.character(temp.lasar$SAMPLE_TIME),12,19)
lasar <- rbind(lasar, ph.lasar, temp.lasar)
lasar$Result_clean <- as.numeric(lasar$Result_clean)
# lasar.to.save <- lasar
# sqlSave(wq, lasar.to.save, tablename = 'LASAR_Toxics_Query_wcriterianame_wAddOns_06232014', rownames = FALSE)
# rm(lasar.to.save)
# odbcCloseAll()

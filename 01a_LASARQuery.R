library(RODBC)
library(xlsx)
library(plyr)
library(stringr)
library(reshape2)
library(foreign)

options(stringsAsFactors = FALSE, scipen = 100)

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


#set up to query LASAR from deqlead-lims
con <- odbcConnect('LASAR')

#Pull in the compiled criteria table used for the Toxics Monitoring prgram
source('//deqlead01/wqm/TOXICS_2012/Data/R/criteria.R')

#build out query
#we need to get lasar names
all.parameters <- sqlFetch(con, 'PARAMETER')
deq.pollutants <- criteria.values.melted.applicable[criteria.values.melted.applicable$variable %in% 
                                                      c('Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism',
                                                        'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only',
                                                        'Table 30 Toxic Substances - Freshwater Acute',
                                                        'Table 30 Toxic Substances - Freshwater Chronic',
                                                        'Table 30 Toxic Substances - Saltwater Acute',
                                                        'Table 30 Toxic Substances - Saltwater Chronic'),]
deq.pollutants <- deq.pollutants[!duplicated(deq.pollutants$Pollutant),]
lasar.names <- data.frame('Pollutant' = all.parameters[all.parameters$NAME %in% deq.pollutants$Pollutant,'NAME'], 'lasar.name' = all.parameters[all.parameters$NAME %in% deq.pollutants$Pollutant,'NAME'])
lasar.names.match <- merge(data.frame('Pollutant' = deq.pollutants[,'Pollutant']), lasar.names, by = 'Pollutant', all.x = TRUE)

lasar.names.match[grep('Dissolved',lasar.names.match$Pollutant),'lasar.name'] <- gsub(', Dissolved', "", lasar.names.match[grep('Dissolved',lasar.names.match$Pollutant),'Pollutant'])
lasar.names.match[grep('Total recoverable',lasar.names.match$Pollutant),'lasar.name'] <- gsub(', Total recoverable', "", lasar.names.match[grep(', Total recoverable',lasar.names.match$Pollutant),'Pollutant'])

#View(arrange(all.parameters[grep('ichlorobromo', all.parameters$NAME),],NAME))
#all.parameters[which(all.parameters$CAS_NUMBER == 541731),]

lasar.names.match[is.na(lasar.names.match$lasar.name),]

rename.vector <- c("Alkalinity" = "Alkalinity as Calcium Carbonate",
                   "Arsenic, Total inorganic" = "Arsenic",
                   "Azinphos methyl" = "Azinphos Methyl",
                   "Chloronaphthalene 2" = "2-Chloronaphthalene",
                   "Chlorodibromomethane" = "Dibromochloromethane",
                   "Chloroethyl Ether bis 2"= "Bis(2-chloroethyl)ether",
                   "Chloroisopropyl Ether bis 2" = "Bis(2-chloroisopropyl) ether",
                   "Chloromethyl ether, bis" = "bis(Chloromethyl)ether",
                   "Chlorophenol 2" = "2-Chlorophenol",
                   "Chlorophenoxy Herbicide (2,4,5,-TP)" = "Silvex",
                   "Chromium (Hex)" = 'Chromium, hexavalent',
                   "Di-n-butyl Phthalate" = "di-n-Butylphthalate",
                   "Dichlorobenzene(m) 1,3" = "1,3-Dichlorobenzene",
                   "Dichlorobenzene(o) 1,2" = "1,2-Dichlorobenzene",
                   "Dichlorobenzene(p) 1,4" = "1,4-Dichlorobenzene",
                   "Dichlorobenzidine 3,3'" = "3,3`-Dichlorobenzidine",
                   "Dichlorobromomethane" = "Bromodichloromethane",
                   "Dichloroethane 1,2" = "1,2-Dichloroethane",
                   "Dichloroethylene 1,1" = "1,1-Dichloroethylene",
                   "Dichloroethylene trans 1,2" = "trans-1,2-Dichloroethylene",
                   "Dichlorophenol 2,4" = "2,4-Dichlorophenol",
                   "Dichloropropane 1,2" = "1,2-Dichloropropane",
                   "Dichloropropene 1,3" = "1,3-Dichloropropene",
                   "Dimethylphenol 2,4" = "2,4-Dimethylphenol",
                   "Dinitrophenol 2,4" = "2,4-Dinitrophenol",
                   "Dinitrophenols" = "Dinitrophenol",
                   "Dioxin (2,3,7,8-TCDD)" = "2,3,7,8-TCDD",
                   "Diphenylhydrazine 1,2" = "1,2-Diphenylhydrazine",
                   "Endosulfan" = "5-Norbornene-2, 3-dimethanol, 1,4,5,6,7,7-hexachloro cyclic sulfite",
                   "Endosulfan Sulfate" = "Endosulfan sulfate",
                   "Heptachlor Epoxide" = "Heptachlor epoxide",
                   "Hexachlorocyclo-hexane-Technical" = "HCH",
                   "Methyl Bromide" = "Bromomethane",
                   "Methyl-4,6-dinitrophenol 2" = "2-Methyl-4,6-dinitrophenol",
                   "Nitrates" = "Nitrate/nitrite as N",
                   "Nitrosamines"  = "Nitrosamine, NOS",
                   "Nitrosodi-n-propylamine, N" = "n-Nitroso-di-n-propylamine",
                   "Nitrosodibutylamine, N" = "n-Nitroso-di-n-butylamine",
                   "Nitrosodiethylamine, N" = "n-Nitrosodiethylamine",
                   "Nitrosodimethylamine, N" = "n-Nitrosodimethylamine",
                   "Nitrosodiphenylamine, N" = "n-Nitrosodiphenylamine",
                   "Nitrosopyrrolidine, N" = "n-Nitrosopyrrolidine",
                   "Phosphorus Elemental" = "Phosphorus",
                   "Sulfide Hydrogen Sulfide" = "Hydrogen Sulfide",
                   "Tetrachlorobenzene, 1,2,4,5-" = "1,2,4,5-Tetrachlorobenzene",
                   "Tetrachloroethane 1,1,2,2" = "1,1,2,2-Tetrachloroethane",
                   "Toxaphene" = "Toxaphene (technical)",
                   "Trichlorobenzene 1,2,4" = "1,2,4-Trichlorobenzene",
                   "Trichloroethane 1,1,2" = "1,1,2-Trichloroethane",
                   "Trichlorophenol 2,4,6" = "2,4,6-Trichlorophenol",
                   "Trichlorophenol, 2, 4, 5-" = "2,4,5-Trichlorophenol")

lasar.names.match$lasar.name <- ifelse(is.na(lasar.names.match$lasar.name),lasar.names.match$Pollutant,lasar.names.match$lasar.name)

lasar.names.match$lasar.name <- mapvalues(lasar.names.match$lasar.name, from = names(rename.vector), to = rename.vector)

to.add <- data.frame('Pollutant' = c('Alkalinity', 'Arsenic, Total inorganic', 'Arsenic, Total inorganic', "Azinphos methyl", 
                                     "Azinphos methyl", "Azinphos methyl","Chloroethyl Ether bis 2","Chlorophenoxy Herbicide (2,4,5,-TP)",
                                     'Chromium, Dissolved',"Dichlorobenzidine 3,3'","Dichloroethane 1,2", "Dichloroethylene 1,1",
                                     "Dichloroethylene trans 1,2", "Dichloropropene 1,3","Methyl-4,6-dinitrophenol 2","Nitrates",
                                     "Nitrates","Nitrosodibutylamine, N","Nitrosodimethylamine, N","Nitrosopyrrolidine, N",
                                     "Phosphorus Elemental"),
                     'lasar.name' = c('Carbonate Alkalinity as Calcium Carbonate', 'Arsenic, ion (As3+)', 'Arsenic, ion (As5+)',
                                      'Azinophos Methyl','Guthion','Guthion (Azinphosmethyl)','Bis(2-Chloroethyl) ether','2,4,5-TP (Silvex)',
                                      'Chromium, trivalent','1,1`-Biphenyl-4,4`-diamine, 3,3`-dichloro-',"Ethylene dichloride", 
                                      "1,1-Dichloroethene", "trans-1,2-Dichloroethene", 'Propene, 1,3-dichloro-','4,6-Dinitro-2-methylphenol',
                                      "Nitrate", "Nitrate as N",'1-Butanamine, N-butyl-N-nitroso-',"Methanamine, N-methyl-N-nitroso",
                                      "Pyrrole, tetrahydro-N-nitroso-", "Total Phosphorus"))

lasar.names.match <- rbind(lasar.names.match, to.add)

aroclors <- all.parameters[grep('[Aa]roclor',all.parameters$NAME),'NAME']
pcbs <- all.parameters[grep('[Pp][Cc][Bb]',all.parameters$NAME),'NAME']

parameters <- paste("'",paste(c(aroclors, pcbs, unique(lasar.names.match$lasar.name),'Ammonia as N','Orthophosphate as P'),collapse="','"),"'",sep="")

#process: select out lasar names that are different and fall in deq.pollutants. remove deq.pollutant name. add in lasar name. pass to query.

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
               sm.SAMPLE_MATRIX in ('Surface water', 'Bay/Estuary/Ocean', 'Canal', 'Reservoir', 'Lake',
               'Ditch/Pond/Culvert/Drain') and
               p.Name in (", parameters, ") 
               Order by s.STATION_KEY, s.SAMPLE_DATE;")

lasar <- sqlQuery(con, query)

lasar$Result <- str_trim(lasar$Result)
report <- get.cases(lasar$Result)

lst.split <- strsplit(as.character(report$Case), split = ' ')
for (i in 1:length(lst.split)){
  lst.split[[i]][1] <- ifelse(substr(lst.split[[i]][1],1,1) == '<',substr(lst.split[[i]][1],2,nchar(lst.split[[i]][1])),lst.split[[i]][1])
  report$Sub[i] <- ifelse(is.na(as.numeric(str_trim(lst.split[[i]][1]))), 
                          ifelse(substr(str_trim(lst.split[[i]][1]),1,1) == '<','ND',NA),
                          as.numeric(lst.split[[i]][1]))
}

report[report$Case == '< 0.5','Sub'] <- 0.5
report[report$Case == '< 10 Est','Sub'] <- 10
report[report$Case == '< 10','Sub'] <- 10
report[report$Case == '<0.001est','Sub'] <- 0.001
report[report$Case == '<0.003est','Sub'] <- 0.003
report[report$Case == '<0.002est','Sub'] <- 0.002
report[report$Case == '<0.004est','Sub'] <- 0.004
report[report$Case == '>0.6','Sub'] <- 0.6  
report[report$Case == '>1','Sub'] <- 1
report[report$Case == '<1..5','Sub'] <- 1.5
report[report$Case == '< 6000','Sub'] <- 6000
report[report$Case == '< 19','Sub'] <- 19
report[report$Case == '< 18','Sub'] <- 18
report[report$Case == '< 19est','Sub'] <- 19
report[report$Case == '< 20','Sub'] <- 20
report[report$Case == '<0.005(LOD)','Sub'] <- 0.005
report[report$Case == '0.006J','Sub'] <- 0.006
report[report$Case == '<20est','Sub'] <- 20
report[report$Case == '<40est','Sub'] <- 40
report[report$Case == '<80est','Sub'] <- 80

Result_clean <- sub.cases(lasar$Result, report) 
lasar <- cbind(lasar, Result_clean)
lasar <- lasar[!is.na(lasar$Result_clean),]

lasar.stations <- unique(lasar[,c('STATION_KEY','LOCATION_DESCRIPTION')])

lasar$criteria.name <- mapvalues(lasar$NAME, from = lasar.names.match$lasar.name, to  = lasar.names.match$Pollutant)

lasar$ABBREVIATION <- gsub('R','r',lasar$ABBREVIATION)

lasar$test <- ifelse(lasar$ABBREVIATION %in% c('Dissolved', 'Total recoverable'),paste(lasar$NAME, ", ", lasar$ABBREVIATION, sep = ''),lasar$NAME)
# 
# wq <- odbcConnect('WQAssessment')
# lasar.to.save <- lasar
# lasar.to.save$SAMPLE_DATE <- as.character(lasar.to.save$SAMPLE_DATE)
# lasar.to.save$SAMPLE_TIME <- substr(as.character(lasar.to.save$SAMPLE_TIME),12,19)
# sqlSave(wq, lasar.to.save, tablename = 'LASAR_Toxics_Query_06112014', rownames = FALSE)
# rm(lasar.to.save)

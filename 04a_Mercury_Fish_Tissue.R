library(plyr)
library(RODBC)

con <- odbcConnect('WQAssessment')
sul2010 <- sqlFetch(con, 'StationUseList')
odbcCloseAll()

hg.ft.all <- read.csv('LASAR_WQP_Mercury_fish_tissue.csv')

hg.ft.all$SAMPLE_DATE <- as.POSIXct(strptime(hg.ft.all$SAMPLE_DATE, format = "%Y-%m-%d"))
hg.ft.all$Result_clean <- as.numeric(hg.ft.all$Result_clean)

hg.ft.all$x <- ifelse(!hg.ft.all$QA_QC_TYPE %in% c('Sample','Sample-Routine'),1,0)

#Manual is easiest here ---- hg.ft.all[hg.ft.all$x == 1,]
hg.ft.fd.resolved <- hg.ft.all[c(100,91,291,289,108,168,217,259,277,289,291,303,318,322,548,687,693),]

hg.ft.wo <- hg.ft.all[hg.ft.all$x != 1,]
hg.ft.all.fixed <- rbind(hg.ft.wo, hg.ft.fd.resolved)

hg.ft.all.grouped <- ddply(hg.ft.all.fixed, .(STATION_KEY, LOCATION_DESCRIPTION), 
                           summarise, 
                           geomean = geometric.mean(Result_clean), 
                           samples = length(Result_clean), 
                           SAMPLE_DATE_START = min(SAMPLE_DATE), 
                           SAMPLE_DATE_END = max(SAMPLE_DATE))

hg.w.sul <- merge(hg.ft.all.grouped, sul2010[,c('STATION','USE_OtherParms','USE_Final')], by.x = 'STATION_KEY', by.y = 'STATION', all.x = TRUE)

hg.for.mike <- hg.w.sul[is.na(hg.w.sul$USE_OtherParms),]

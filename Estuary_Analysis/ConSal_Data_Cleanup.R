library(reshape)

options(stringsAsFactors = F)
workspace = "E:/GitHub/ToxicsRedo/Estuary_Analysis"
setwd(workspace)

#Import the two csv files with conductivity and salinity data
in_wqp_consal = 'E:/GitHub/ToxicsRedo/Estuary_Analysis/WQP_consal.csv'
in_lasar_consal = 'E:/GitHub/ToxicsRedo/Estuary_Analysis/LASAR_consal.csv'
wqp_consal = read.csv(in_wqp_consal, header=T)[c('CharacteristicName', 'MonitoringLocationIdentifier', 'ResultMeasureValue', 'ResultMeasure.MeasureUnitCode')]
lasar_consal = read.csv(in_lasar_consal, header=T)[c('NAME', 'STATION_KEY', 'Result_clean', 'UNIT')]
names(wqp_consal) = c('Name', 'Station', 'Value', 'Units')
names(lasar_consal) = c('Name', 'Station', 'Value', 'Units')

#Clean up station names and units
wqp_consal <- wqp_consal[!is.na(wqp_consal$Value),]
wqp_stations = wqp_consal['Station']
wqp_stations = sapply(wqp_stations, function(x){substr(x,10, nchar(x))})
wqp_consal['Station'] = wqp_stations
lasar_consal[lasar_consal$Units == unique(lasar_consal$Units)[1], 'Units'] <- 'uS/cm'

#Merge stations
consal = rbind(lasar_consal, wqp_consal)

# #Test unit conversion of mS/cm to uS/cm
# consal_units <- consal
# consal_units$Value[consal_units$Units == 'mS/cm']<-consal_units$Value[consal_units$Units == 'mS/cm']*1000
# boxplot(Value ~ Units, consal[consal$Units %in% c('mS/cm', 'uS/cm'),], main="Uncorrected Units")
# boxplot(Value ~ Units, consal_units[consal_units$Units %in% c('mS/cm', 'uS/cm'),], main='Corrected Units')

#Based on numeric, visual, and geographic analysis, it appears that the conductivity units 'mS/cm' are a 
#typographical error and should be 'uS/cm'. 

#Change 'mS/cm' to 'uS/cm'
consal$Units[consal$Units=='mS/cm']<-'uS/cm'

#Define salinity conversion function
salinity<-function(conductivity){
  c = conductivity/53087
  k1 = 0.0120
  k2 = -0.2174
  k3 = 25.3283
  k4 = 13.7714
  k5 = -6.4788
  k6 = 2.5842
  
  return(k1 + k2*sqrt(c) + k3*c + k4*c^(3/2) + k5*c^2 + k6*c^(5/2))
}

#Use Salinity function to convert conductance to salinity so we have one metric to work with.
consal$Value[consal$Units=='uS/cm']<-salinity(consal$Value[consal$Units=='uS/cm'])
consal$Units[consal$Units=='uS/cm']<-'ppth'
consal$Name[consal$Name=='Conductivity']<-'Salinity'
consal_max = cast(consal, Station~Name, max, value = 'Value')
names(consal_max)[2]<-'Sal_ppth'
is.na(consal_max) <- do.call(cbind,lapply(consal_max, is.infinite))

#output stations to temporary csv file.
out_file = 'E:/GitHub/ToxicsRedo/Estuary_Analysis/temp_csv.csv'
write.csv(consal_max, out_file, row.names=F)

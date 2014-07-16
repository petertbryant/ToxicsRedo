dma <- dcc.min[!is.na(dcc.min$value),]

dma$day.POSIX <- as.POSIXct(strptime(dma$day, format = '%Y-%m-%d'))

library(dplyr)

dma.groups <- group_by(dma, SampleRegID, SampleAlias, criterianame.x)
dma.summary <- summarise(dma.groups, exceed = sum(exceed*Valid),
                         valid_n = sum(Valid),
                         total_n = length(Valid), 
                         percent.exceed = (exceed/valid_n)*100,
                         min_date = min(day.POSIX),
                         max_dat = max(day.POSIX))

#station status determination
dma.summary$cat <- ifelse(dma.summary$exceed >= 2,
                          ifelse(dma.summary$criterianame.x %in% c('Alkalinity','Phosphate Phosphorus'),
                                 '3B',
                                 '5'),
                          ifelse(dma.summary$exceed == 1,
                                 '3B',
                                 ifelse(dma.summary$valid_n >= 5,
                                        '2',
                                        '3')))

#We want to keep these in so we can have the insufficient data persist
#dmas.wo.invalid <- dma.summary[dma.summary$valid_n != 0,]

unique(dmas.wo.invalid$criterianame.x)[!unique(dmas.wo.invalid$criterianame.x) %in% pollutants$Pollutant]

pollutants$criterianame.x <- pollutants$Pollutant

redo.relate <- merge(pollutants, data.frame(criterianame.x = unique(dmas.wo.invalid$criterianame.x)), 
                     by = 'criterianame.x', all.y = TRUE)
write.csv(redo.relate, 'Criteria_Pollutant_Lookup.csv', row.names = FALSE)


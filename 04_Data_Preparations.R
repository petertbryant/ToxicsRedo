library(plyr)

View(arrange(criteria.values.melted.applicable.nonnum[grepl(', Total',criteria.values.melted.applicable.nonnum$Pollutant) & 
                                                        criteria.values.melted.applicable.nonnum$variable %in% deq.pollutants$variable,],Pollutant))
View(arrange(criteria.values.melted.applicable.nonnum[grepl('issolv',criteria.values.melted.applicable.nonnum$Pollutant) & 
                                                        criteria.values.melted.applicable.nonnum$variable %in% deq.pollutants$variable,],Pollutant))


#bring the two data sets into one now, lasar and wqp.data to feed into Toxics Analysis
names(lasar)
names(wqp.data)

#Remove Bed Sediment samples
wqp.data <- wqp.data[wqp.data$ResultSampleFractionText != 'Bed Sediment',]
wqp.data$adid <- paste(wqp.data$CharacteristicName, wqp.data$ActivityStartDate)


# str(data.wo.void)
# 'data.frame':  163574 obs. of  15 variables:
#   $ Analyte         : chr  "1-Methylphenanthrene" "1-Methylphenanthrene" "1-Methylphenanthrene" "1-Methylphenanthrene" ...
# $ Project         : chr  "Mid Coast" "South Coast" "Owyhee" "North Coast" ...
# $ SampleRegID     : int  37399 28303 10730 13654 37399 10990 20434 12962 11047 34309 ...
# $ SampleAlias     : chr  "Umpqua River at Discovery Center Docks" "Elk Creek at ODFW Hatchery" "Owyhee River at Rome (Hwy.95)" "Necanicum R at 12th Street approach" ...
# $ Sampled         : chr  "11/19/2013 9:45" "10/28/2013 13:00" "9/23/2013 9:25" "12/2/2013 14:25" ...
# $ SampleType      : chr  "Grab Sample" "Grab Sample" "Grab Sample" "Grab Sample" ...
# $ Matrix          : chr  "Estuary" "River/Stream" "River/Stream" "Estuary" ...
# $ tResult         : num  0 0 0 0 0 0 0 0 0 0 ...
# $ tMRL            : num  5.93 5.89 5.93 5.37 5.36 5.86 6.15 5.42 5.95 5.91 ...
# $ Unit            : chr  "ng/L" "ng/L" "ng/L" "ng/L" ...
# $ SpecificMethod  : chr  "EPA 8270D" "EPA 8270D" "EPA 8270D" "EPA 8270D" ...
# $ Status          : chr  "A" "A" "B" "B" ...
# $ chem.group      : chr  "Combustion By-Products" "Combustion By-Products" "Combustion By-Products" "Combustion By-Products" ...
# $ Detect.nondetect: num  0 0 0 0 0 0 0 0 0 0 ...
# $ code            : chr  "1-Methylphenanthrene 37399 11/19/2013 9:45" "1-Methylphenanthrene 28303 10/28/2013 13:00" "1-Methylphenanthrene 10730 9/23/2013 9:25" "1-Methylphenanthrene 13654 12/2/2013 14:25" ...

#Need to figure out how to make criteria.name/Name.full/Analyte match up right.
#lasar$criteria.name.....

#Make a date-time field
lasar$Sampled <- paste(as.character(lasar$SAMPLE_DATE), substr(as.character(lasar$SAMPLE_TIME),12,19))

#see how much diff there is with method_detection_limit and method_reporting_limit
View(lasar[which(lasar$METHOD_DETECTION_LIMIT != lasar$METHOD_REPORTING_LIMIT),])

#figure out some naming issues
#start with metals that had total instead of dissolved, total recoverable
unique(lasar[lasar$ABBREVIATION == 'Total','NAME'])
#For metals we want to see Total recoverable instead of Total. But there are a handful of parameters with Total that aren't metals.
total.to.recoverable <- c('Mercury','Copper','Zinc','Nickel','Lead','Selenium','chromium','Iron','Barium','Thallium','Manganese','Silver','Antimony','Cadmium','Cyanide')
lasar[lasar$NAME %in% total.to.recoverable,'ABBREVIATION'] <- 'Total recoverable'
lasar$test <- ifelse(lasar$ABBREVIATION %in% c('Dissolved', 'Total recoverable'),paste(lasar$NAME, ", ", lasar$ABBREVIATION, sep = ''),lasar$NAME)
lasar$adid <- paste(lasar$NAME, lasar$Sampled)

lasar <- rename(lasar, c('Result' = 'tResult', 
                         'criteria.name' = 'Analyte', 
                         'STATION_KEY' = 'SampleRegID',
                         'LOCATION_DESCRIPTION' = 'SampleAlias',
                         'QA_QC_TYPE' = 'SampleType',
                         'Result_clean' = 'tResult',
                         'METHOD_REPORTING_LIMIT' = 'tMRL',
                         'UNIT' = 'Unit',
                         'METHOD' = 'SpecificMethod',
                         'STATUS' = 'Status'))

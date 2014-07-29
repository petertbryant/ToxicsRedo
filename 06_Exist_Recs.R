ref.con <- odbcConnect('WQAssessment')
status <- sqlFetch(ref.con, 'Assessment_Status')

existars <- ars[ars$Record_ID %in% stations.existrecs$RecordID, c('Listing_Status', 'Record_ID','Action','PreviousAction','PreviousStatus','ListingYear','Pollutant')]

stations.existrecs <- merge(stations.existrecs,existars,by.x='RecordID',by.y='Record_ID')

status.count.2012 <- table(stations.existrecs$RecordID, stations.existrecs$cat)

#creates an empty list to write 2012 assessment statuses to
status.2012 <- list()

#this steps through the segmentation methodology for determining 2012 status
for (i in 1:nrow(status.count.2012)) {
  
  status.2012$Record_ID[[i]] <- rownames(status.count.2012)[i]
  
  if (status.count.2012[i,4] > 0) {
    status.2012$Status[[i]] <- 'Cat 5: Water quality limited, 303(d) list, TMDL needed'
  } else if (status.count.2012[i,1] > 0) {
    status.2012$Status[[i]] <- 'Cat 2: Attaining some criteria/uses'
  } else if (status.count.2012[i,3] > 0) {
    status.2012$Status[[i]] <- 'Cat 3B: Potential concern'
  } else
    status.2012$Status[[i]] <- 'Cat 3: Insufficient data'
  
}

#rm(i, status.count.2012)

#this converts the 2012 statuses from a list to a dataframe
status.2012 <- do.call("cbind",status.2012)  
status.2012 <- as.data.frame(status.2012, stringsAsFactors = F)
status.2012$Record_ID <- as.integer(as.character(status.2012$Record_ID))
status.2012 <- arrange(status.2012, Record_ID)

#orders and combines 2010 status with 2012 status for ease of comparison
status.comparison <- merge(existars, status.2012, by = 'Record_ID')
status.comparison$Status_2010 <- gsub('Previous Status: ','',status.comparison$PreviousStatus)
status.comparison <- rename(status.comparison, c('Listing_Status' = 'Draft_2012', 'Status' = 'Status_2012'))

#resolves issue of historical category determinations (e.g. 303d vs Cat5)
status.comparison$Status_2010 <- factor(status.comparison$Status_2010)
levels(status.comparison$Status_2010) <- revalue(levels(status.comparison$Status_2010), 
                                                 c('303(d)' = "Cat 5: Water quality limited, 303(d) list, TMDL needed", 
                                                   "Attaining" = "Cat 2: Attaining some criteria/uses", 
                                                   "Insufficient data" = "Cat 3: Insufficient data", 
                                                   "Potential concern" = "Cat 3B: Potential concern", 
                                                   "TMDL approved" = "Cat 4A: Water quality limited, TMDL approved"))
status.comparison$Status_2010 <- as.character(status.comparison$Status_2010)

#goes through Appendix 1 table comparing 2012 status to 2010 status to make a final
#determination on 2012 status
for (i in 1:nrow(status.comparison)) {
  if (!is.na(status.comparison$Status_2010[i])){
    if (status.comparison$Status_2010[i] == "Cat 5: Water quality limited, 303(d) list, TMDL needed") {
      
      status.comparison$Status_Final[i] <- "Cat 5: Water quality limited, 303(d) list, TMDL needed"
      
    } else if (status.comparison$Status_2010[i] == 'Cat 4A: Water quality limited, TMDL approved' &
                 (status.comparison$Status_2012[i] == "Cat 3: Insufficient data" |
                    status.comparison$Status_2012[i] == "Cat 3B: Potential concern")) {
      
      status.comparison$Status_Final[i] <- 'Cat 4A: Water quality limited, TMDL approved'
      
    } else if (status.comparison$Status_2012[i] == "Cat 5: Water quality limited, 303(d) list, TMDL needed" &
                 (status.comparison$Status_2010[i] == "Cat 3: Insufficient data" |
                    status.comparison$Status_2010[i] == "Cat 3B: Potential concern" |
                    status.comparison$Status_2010[i] == 'Cat 2: Attaining some criteria/uses')) {
      
      status.comparison$Status_Final[i] <- "Cat 5: Water quality limited, 303(d) list, TMDL needed"
      
    } else if (status.comparison$Status_2012[i] == "Cat 5: Water quality limited, 303(d) list, TMDL needed" &
                 status.comparison$Status_2010[i] == 'Cat 4A: Water quality limited, TMDL approved') {
      
      status.comparison$Status_Final[i] <- 'Cat 4A: Water quality limited, TMDL approved'
      
    } else if (status.comparison$Status_2012[i] == 'Cat 2: Attaining some criteria/uses' &
                 (status.comparison$Status_2010[i] == "Cat 3: Insufficient data" |
                    status.comparison$Status_2010[i] == "Cat 3B: Potential concern" |
                    status.comparison$Status_2010[i] == 'Cat 2: Attaining some criteria/uses')) {
      
      status.comparison$Status_Final[i] <- 'Cat 2: Attaining some criteria/uses'
      
    } else if (status.comparison$Status_2012[i] == 'Cat 3: Insufficient data' &
                 status.comparison$Status_2010[i] == "Cat 2: Attaining some criteria/uses"  ) {
      
      status.comparison$Status_Final[i] <- "Cat 2: Attaining some criteria/uses"  
      
    } else if (status.comparison$Status_2012[i] == 'Cat 3: Insufficient data' &
                 status.comparison$Status_2010[i] == "Cat 3B: Potential concern"  ) {
      
      status.comparison$Status_Final[i] <- "Cat 3B: Potential concern"  
      
    } else if (status.comparison$Status_2012[i] == "Cat 3B: Potential concern" &
                 status.comparison$Status_2010[i] == "Cat 2: Attaining some criteria/uses"  ) {
      
      status.comparison$Status_Final[i] <- "Cat 3B: Potential concern" 
      
    } else if (status.comparison$Status_2012[i] == "Cat 3B: Potential concern" &
                 status.comparison$Status_2010[i] == 'Cat 3: Insufficient data'  ) {
      
      status.comparison$Status_Final[i] <- "Cat 3B: Potential concern"
      
    } else if (status.comparison$Status_2010[i] == 'Cat 4B: Water quality limited, other control measures') {
      
      status.comparison$Status_Final[i] <- "Cat 4B: Water quality limited, other control measures"
      
    }
    
    else
      
      status.comparison$Status_Final[i] <- as.character(status.comparison$Status_2012[i])
    
    if (status.comparison$Status_2012[i] == 'Cat 2: Attaining some criteria/uses' &
          status.comparison$Status_2010[i] == "Cat 5: Water quality limited, 303(d) list, TMDL needed"){
      
      status.comparison$Status_Final[i] <- "Potentially Delistable"
      
    } 
  } else if (status.comparison$Draft_2012[i] == 'Cat 4A: Water quality limited, TMDL approved') {
    status.comparison$Status_Final[i] <- status.comparison$Draft_2012[i]
  } else {
    status.comparison$Status_Final[i] <- status.comparison$Status_2012[i]
  }
  
}

#There is a change to the interpretation of the Mercury TMDL on the Willamette and was changed from 4a to 5 in the draft
#We want to preserve that change
status.comparison[status.comparison$Record_ID == 17174,'Status_Final'] <- status.comparison[status.comparison$Record_ID == 17174,'Draft_2012']

#Updates 2012 Action, Action ID and Status
status.comparison.same <- status.comparison[which(status.comparison$Status_2010 == status.comparison$Status_Final),]
status.comparison.same$Action <- 'No status change'
status.comparison.same$Action_ID <- '13'

status.comparison.diff <- status.comparison[which(status.comparison$Status_2010 != status.comparison$Status_Final & status.comparison$Action != 'Added to database'),]
status.comparison.diff[which(status.comparison.diff$Status_Final == "Cat 2: Attaining some criteria/uses"),'Action'] <- 'Status modification - Attaining criteria/uses'
status.comparison.diff[which(status.comparison.diff$Status_Final == "Cat 2: Attaining some criteria/uses"),'Action_ID'] <- '2'
status.comparison.diff[which(status.comparison.diff$Status_Final == "Cat 3B: Potential concern"),'Action'] <- 'Status modification - Potential concern'
status.comparison.diff[which(status.comparison.diff$Status_Final == "Cat 3B: Potential concern"),'Action_ID'] <- '1'
status.comparison.diff[which(status.comparison.diff$Status_Final == "Cat 5: Water quality limited, 303(d) list, TMDL needed"),'Action'] <- 'Status modification - Added to 303(d) list'
status.comparison.diff[which(status.comparison.diff$Status_Final == "Cat 5: Water quality limited, 303(d) list, TMDL needed"),'Action_ID'] <- '10'

status.comparison.na <- status.comparison[is.na(status.comparison$Status_2010),]
status.comparison.na$Action_ID <- 14

status.comparison.added <- status.comparison[status.comparison$Action == 'Added to database',]
status.comparison.added$Action_ID <- 7

status.comparison <- rbind(status.comparison.same, status.comparison.diff, status.comparison.na, status.comparison.added)

status.comparison <- arrange(status.comparison, Record_ID)
status.comparison.final <- status.comparison[,c('Record_ID', 'Status_Final', 'Action', 'Action_ID')]

#Pulls together the summary field for the existing segments. 
stations.existrecs$exceed <- round(as.numeric(stations.existrecs$exceed), 1)
stations.existrecs$total_n <- round(as.numeric(stations.existrecs$total_n), 1)
stations.existrecs$RIVER_MILE <- round(as.numeric(stations.existrecs$RIVER_MILE), 1)

stations.existrecs$value <- ifelse(suppressWarnings(is.na(as.numeric(stations.existrecs$value))),stations.existrecs$value,paste(stations.existrecs$value, "µg/L"))

stations.existrecs.summary <- ddply(stations.existrecs, .(RecordID), text.summary)
stations.existrecs.summary$RecordID <- as.integer(stations.existrecs.summary$RecordID)
stations.existrecs.summary <- arrange(stations.existrecs.summary, RecordID) 
stations.existrecs.summary <- rename(stations.existrecs.summary, c('RecordID' = 'Record_ID'))

#Take the existing records and update the summary field
# ars <- rename(ars, c('Pollutant_ID.x' = 'Pollutant_ID', 'Season_ID.x' = 'Season_ID'))
# ars <- within(ars, rm(Segment_ID.y, Pollutant_ID.y, Season_ID.y, SampleMatrix_ID, NarCriteria_ID, NumCriteria_ID, YearAdded, recTimeStamp))
existsegs <- ars[ars$Record_ID %in% status.comparison$Record_ID,]
#existsegs$Summary <- as.character(existsegs$Summary)
existsegs <- arrange(existsegs, Record_ID)
existsegs[existsegs$Action == 'Added to database','Summary'] <- ''
existsegs$Summary <- gsub('^.*\r\n\r\n','',existsegs$Summary)
existsegs <- merge(existsegs, stations.existrecs.summary, by = 'Record_ID')
existsegs$V1 <- paste('2012 Data:', existsegs$V1, sep = '\r\n')
existsegs$Summary <- paste(existsegs$V1, existsegs$Summary, sep = '\r\n\r\n')
existsegs <- within(existsegs, rm(V1))


existsegs <- merge(existsegs, status.comparison.final, by = 'Record_ID')
#existsegs$PreviousStatus <- paste('Previous Status:', existsegs$Listing_Status)
existsegs$Listing_Status <- existsegs$Status_Final
#existsegs$PreviousAction <- paste('Previous Action:', existsegs$Action.x)
#existsegs$ListingYear <- paste('Previous Assessment Year:', existsegs$AssessmentYear)
existsegs <- within(existsegs, rm(Status_Final, Action.x, Action_ID.x))
existsegs <- rename(existsegs, c('Action.y' = 'Action', 'Action_ID.y' = 'Action_ID'))
existsegs <- merge(existsegs, status, by = 'Listing_Status', all.x = TRUE)
existsegs <- within(existsegs, rm(Status_ID.x))
existsegs <- rename(existsegs, c('Status_ID.y' = 'Status_ID'))

#criteria name
existsegs <- merge(existsegs, unique(stations.existrecs[,c('RecordID','variable')]), by.x = 'Record_ID', by.y = 'RecordID', all.x = TRUE)
existsegs[existsegs$variable == 'EPA Benchmark','variable'] <- existsegs[existsegs$variable == 'EPA Benchmark','Criteria']
existsegs$Criteria <- existsegs$variable
existsegs <- within(existsegs, rm(variable))
existsegs$Criteria <- gsub('( -.*)','',existsegs$Criteria)
#still need to update numeric criteria ID



#Assessment IDs need updating as does assessment year but that must be done in a later script.
library(RODBC)
ref.con <- odbcConnect('WQAssessment')
LLID.Streams <- sqlQuery(ref.con, 'SELECT * FROM LookupStreams_LLID')
LLID.Lakes <- sqlQuery(ref.con, 'SELECT * FROM LookupLakes_LLID')
segments <- sqlQuery(ref.con, 'SELECT * FROM Assessment_Segment')

LLID.Streams$RM_MIN <- round(as.numeric(LLID.Streams$RM_MIN), 1)
LLID.Streams$RM_MAX <- round(as.numeric(LLID.Streams$RM_MAX), 1)
LLID.Streams.sub <- LLID.Streams[,c('LLID','NAME','RM_MIN','RM_MAX')]

#A spot fix to make it consistent with an existing segment
#LLID.Streams.sub[LLID.Streams.sub$LLID == '1231108446399','RM_MAX'] <- 78.0

stations.newrecs$value <- ifelse(suppressWarnings(is.na(as.numeric(stations.newrecs$value))),stations.newrecs$value,paste(stations.newrecs$value, "µg/L"))
#stations.newrecs <- merge(stations.newrecs, sul2012[,c('STATION','Matrix')], by.x = 'SampleRegID', by.y = 'STATION', all.x = TRUE)

#this looks to see if there are any segments on the LLID that don't capture the 
#stations evaluated
stations.newrecs$code <- paste(stations.newrecs$LLID_Stream_Lake, 
                                      stations.newrecs$Pollutant_ID)

ars.wo <- ars[ars$SampleMatrix_ID == 1,]
ars.wo$code <- paste(ars.wo$LLID_Stream_Lake, ars.wo$Pollutant_ID)
LLIDs.w.snt <- ars.wo[ars.wo$code %in% stations.newrecs$code,]
snt.w.LLIDs <- stations.newrecs[stations.newrecs$code %in% LLIDs.w.snt$code,] 
#snt.w.LLIDs.NOTtable20 <- snt.w.LLIDs[snt.w.LLIDs$Category %in% c('HHWO', 'HHO', 'MCL'),]
#snt.w.LLIDs <- snt.w.LLIDs[snt.w.LLIDs$Category == 'FWCHRONIC',]


#This steps through the snt.w.LLIDs data frame. That is those stations that lie on LLIDs that already have a 
#segment created on part of the LLID for that pollutant but no segment exists that encompassess those stations.
#The code manually creates those segments for the stations to map to by filling in the gap with a new segment.
#The result of this sourcing is a data frame called newsegs.exceptions
source('07a_2012_IR_New_Recs_Exceptions.R')

#This pulls in LLID info into our unmatched stations

snt.no.LLID <- stations.newrecs[!stations.newrecs$code %in% LLIDs.w.snt$code,]
#snt.no.LLID <- rbind(snt.no.LLID, snt.w.LLIDs.NOTtable20)
snt.no.LLID <- merge(snt.no.LLID, LLID.Streams.sub, by.x = 'STREAM_LLID', by.y = 'LLID', all.x = TRUE)

snt.no.LLID$exceed <- as.numeric(snt.no.LLID$exceed)
snt.no.LLID$total_n <- as.numeric(snt.no.LLID$total_n)

#this makes a table with counts of categories by LLID + Pollutant. no ordering is preserved. the rownames
#are the LLID + Pollutant code. tox.mixed.cat means that there are cat 2 and cat 5 for that code
#tox.single.cat contains those codes that have only 2 or only 5 (and insuff. data)
tox.cat.count.code <- as.data.frame.matrix(table(snt.no.LLID$code, snt.no.LLID$cat))
tox.mixed.cat.count <- tox.cat.count.code[tox.cat.count.code$'5' != 0 & tox.cat.count.code$'2' != 0,]
tox.single.cat <- tox.cat.count.code[!rownames(tox.cat.count.code) %in% rownames(tox.mixed.cat.count),]

#This subsets the tox.single.cat table so we can process and know that each subset will have the same
#status assignment
tox.cat2 <- tox.single.cat[tox.single.cat$'2' > 0,]
tox.cat5 <- tox.single.cat[tox.single.cat$'5' > 0,]
tox.cat3 <- tox.single.cat[tox.single.cat$'5' == 0 & tox.single.cat$'2' == 0 & tox.single.cat$'3B' == 0,]
tox.cat3B <- tox.single.cat[tox.single.cat$'5' == 0 & tox.single.cat$'2' == 0 & tox.single.cat$'3B' > 0,]

#The following section uses the 'process' function defined at the top of this file. It takes
#as input a table with code by counts of number of stations with each status and references
#each station's information in order to concatenate the pieces into a Summary field
tox.cat2 <- process(tox.cat2, snt.no.LLID)
tox.cat3 <- process(tox.cat3, snt.no.LLID)
tox.cat3B <- process(tox.cat3B, snt.no.LLID)
tox.cat5 <- process(tox.cat5, snt.no.LLID)

# tox.cat2$code <- paste(tox.cat2$LLID_Stream_Lake, tox.cat2$Pollutant_ID)
# tox.cat3$code <- paste(tox.cat3$LLID_Stream_Lake, tox.cat3$Pollutant_ID)
# tox.cat3B$code <- paste(tox.cat3B$LLID_Stream_Lake, tox.cat3B$Pollutant_ID)
# tox.cat5$code <- paste(tox.cat5$LLID_Stream_Lake, tox.cat5$Pollutant_ID)
# 
# View(arrange(tox.cat2[tox.cat2$code %in% tox.cat2[duplicated(tox.cat2$code),'code'],],code))
# View(arrange(tox.cat3[tox.cat3$code %in% tox.cat3[duplicated(tox.cat3$code),'code'],],code))
#Not currently an issue for them
#View(arrange(tox.cat3B[tox.cat3B$code %in% tox.cat3B[duplicated(tox.cat3B$code),'code'],],code))
#View(arrange(tox.cat5[tox.cat5$code %in% tox.cat5[duplicated(tox.cat5$code),'code'],],code))

#Since we did the subsetting above this allows us to wholesale apply the status assignments
tox.cat2$Status <- '2'
tox.cat3$Status <- '3'
tox.cat3B$Status <- '3B'
tox.cat5$Status <- '5'

#Currrently, these don't seem to be an issue
# #now we have to deal with those mixed.cat situations. probably have to do this one by hand
# tox.mixed.cat<- snt.no.LLID[snt.no.LLID$code %in% rownames(tox.mixed.cat.count),]
# tox.mixed.cat$Str_RM <- round(as.numeric(tox.mixed.cat$Str_RM), 1)
# tox.mixed.cat<- arrange(tox.mixed.cat, Str_LLID, Pollutant, Str_RM)
# 
# #this for loop splits the segments and adds the summary field. it only works if the first station
# #is cat 5 and all upstream sites are cat 2 or vice versa but if there is any switching along the way
# #definitely will not work. 
# newsegs <- data.frame('LLID_Stream_Lake' = character(), 'Str_name' = character(), 'Str_LLID' = character(), 
#                       'LAKE_LLID' = character(),'LAKE_NAME' = character(),'RM_MIN' = character(), 
#                       'RM_MAX' = character(), 'crit_val'= character(),'Segment_ID' = character(), 
#                       'Status' = character(), 'Record_ID' = character(), 'Pollutant' = character(),
#                       'Pollutant_ID' = character(), 'Summary' = character(), stringsAsFactors = F)
# 
# for (i in 1:length(unique(tox.mixed.cat$code))) {
#   sub <- tox.mixed.cat[tox.mixed.cat$code == unique(tox.mixed.cat$code)[i],]
#   sub$seg <- NA
#   sub.3 <- sub[(sub$cat == '3' | sub$cat == '3B'),]
#   sub <- sub[!(sub$cat == '3' | sub$cat == '3B'),]
#   seg <- subset(sub[1,], select = c('code','LLID_Stream_Lake','Str_name','Str_LLID','LAKE_LLID','LAKE_NAME','Pollutant', 'Pollutant_ID','RM_MIN', 'RM_MAX','crit_val'))
#   
#   for (j in 1:(nrow(sub)-1)) {
#     if (j == 1) {
#       if (sub$cat[j] != sub$cat[j+1]) {
#         if (sub$cat[j] == 5) {
#           sub$seg[j] = 1L
#           sub$seg[j+1] = 2L
#           ds.seg <- seg
#           ds.seg$RM_MAX[j] <- sub$Str_RM[j+1]
#           ds.seg$Status[j] <- '5'
#           us.seg <- seg
#           us.seg$RM_MIN[j] <- sub$Str_RM[j+1]
#           us.seg$Status[j] <- '2'
#         }else {
#           sub$seg[j] = 1L
#           sub$seg[j+1] = 2L
#           ds.seg <- seg
#           ds.seg$RM_MAX[j] <- sub$Str_RM[j] + (sub$Str_RM[j+1]-sub$Str_RM[j])/2 
#           ds.seg$Status <- '2'
#           us.seg <- seg
#           us.seg$RM_MIN[j] <- ds.seg$RM_MAX[j]
#           us.seg$Status <- '5'
#         }
#       }else if (sub$cat[j] == sub$cat[j+1]) {
#         sub$seg[j] = 1L
#         sub$seg[j+1] = 1L
#       } 
#     }
#     else {
#       if (sub$cat[j] != sub$cat[j+1]) {
#         sub$seg[j+1] = j 
#       }else if (sub$cat[j] == sub$cat[j+1]) {
#         sub$seg[j+1] = sub$seg[j]
#       }
#     }
#   }
#   
#   if (nrow(sub.3) == 1) {
#     sub.3$seg <- 1
#     sub <- rbind(sub.3, sub)
#   }
#   
#   sub$combo <- paste(sub$code, sub$seg)
#   sub.summary <- ddply(sub, .(combo), text.summary)
#   
#   #ds.seg$Status <- '5'
#   ds.seg$combo <- paste(ds.seg$code, '1')
#   #us.seg$Status <- '2'
#   us.seg$combo <- paste(us.seg$code, '2')
#   
#   subsegs <- rbind(ds.seg, us.seg)
#   
#   subsegs <- merge(subsegs, sub.summary, by = 'combo', all.y = T)
#   subsegs <- within(subsegs, rm(combo))
#   
#   newsegs <- rbind(newsegs, subsegs)
#   
# }


#now let's put all these new segments together for the toxics
# newsegs <- within(newsegs, rm(code))
# newsegs <- rbind(newsegs, tox.cat2, tox.cat3, tox.cat3B, tox.cat5)
newsegs <- rbind(tox.cat2, tox.cat3, tox.cat3B, tox.cat5)
newsegs <- rename(newsegs, c('V1'='Summary', 'RM_MIN' = 'RM1', 'RM_MAX' = 'RM2'))
newsegs <- rbind(newsegs, newsegs.exceptions)

#Pull the criteria back in so we can fill in the Criteria and NumericCriteria ID columns later on
stations.newrecs$variable <- gsub('( -.*)','',stations.newrecs$variable)

newsegs$code <- paste(newsegs$Pollutant, newsegs$LLID_Stream_Lake)
stations.newrecs$code <- paste(stations.newrecs$Pollutant, stations.newrecs$LLID_Stream_Lake)
newsegs <- merge(newsegs, unique(stations.newrecs[,c('code','variable')]), by = 'code', all.x = TRUE)
newsegs[newsegs$variable == 'EPA Benchmark','variable'] <- unique(existsegs[grep('[Bb]enchmark',existsegs$Criteria),'Criteria'])
newsegs$Criteria <- newsegs$variable
newsegs <- within(newsegs, rm(variable))

#fleshing out the newsegs.tox fields to match the newsegs.do
newsegs$Season <- 'Year Round'
newsegs$Season_ID <- 3
#newsegs <- within(newsegs, rm(crit_val))
newsegs$Stream_Lake_Name <- ifelse(is.na(newsegs$LAKE_NAME), 
                                       newsegs$Stream_Name,
                                       ifelse(is.na(newsegs$Stream_Name),
                                              newsegs$LAKE_NAME,
                                              paste(newsegs$Stream_Name, newsegs$LAKE_NAME, sep = '/')))
newsegs <- rename(newsegs, c('LAKE_LLID' = 'LLID_Lake', 'LAKE_NAME' = 'Lake_Name'))


#put the newsegs.tox and the newsegs.do together now
#newsegs <- rbind(newsegs.tox, newsegs.do)

#check for existing segmentIDs
newsegs$RM1 <- round(as.numeric(newsegs$RM1), 1)
newsegs$RM2 <- round(as.numeric(newsegs$RM2), 1)
newsegs$segcheck <- paste(newsegs$LLID_Stream_Lake, newsegs$RM1, newsegs$RM2)
segments$RM1 <- round(as.numeric(segments$RM1), 1)
segments$RM2 <- round(as.numeric(segments$RM2), 1)
segments$LLID_Stream_Lake <- ifelse(is.na(segments$LLID_Lake),
                                    segments$LLID_Stream,
                                    paste(segments$LLID_Stream, segments$LLID_Lake, sep = '/'))
segments$segcheck <- paste(segments$LLID_Stream_Lake, segments$RM1, segments$RM2)
segments.sub <- segments[segments$Current_Segment == 1,c('segcheck', 'Segment_ID')]
newsegs <- merge(newsegs, segments.sub, by = 'segcheck', all.x = TRUE)

#add in fish tissue data
source('//deqhq1/wqassessment/2012_WQAssessment/Segmentation/R_scripts/Mercury_Fish_Tissue_Incorporate.R')
newsegs <- rbind(newsegs, newsegs.hg)

#filling in the segment id we only want to consider unique LLID, RM1, RM2 combinations 
unique.newsegs <- data.frame('segcheck' = unique(newsegs[is.na(newsegs$Segment_ID),'segcheck'], stringsAsFactors = F))

new.segment.id <- max.segment.id + 1
for (i in 1:nrow(unique.newsegs)) {
  unique.newsegs$Segment_ID[i] <- new.segment.id
  new.segment.id <- new.segment.id + 1
}

newsegs <- merge(newsegs, unique.newsegs, by = 'segcheck', all.x = TRUE)
newsegs.x <- newsegs[is.na(newsegs$Segment_ID.y),]
newsegs.x <- rename(newsegs.x, c('Segment_ID.x' = 'Segment_ID'))
newsegs.x <- within(newsegs.x, rm(Segment_ID.y))
newsegs.y <- newsegs[is.na(newsegs$Segment_ID.x),]
newsegs.y <- rename(newsegs.y, c('Segment_ID.y' = 'Segment_ID'))
newsegs.y <- within(newsegs.y, rm(Segment_ID.x))
newsegs <- rbind(newsegs.x, newsegs.y)
newsegs$Segment_ID <- as.integer(newsegs$Segment_ID)

#then we add the record id
new.record.id <- max.record.id + 1
for (i in 1:nrow(newsegs)) {
  newsegs$Record_ID[i] <- new.record.id
  new.record.id <- new.record.id + 1
}

#Adding in the HUC information (this works for when there are two or less segments on an LLID but I have a feeling it may not if there are more than two)
newsegs$LLID_Stream <- newsegs$Str_LLID
newsegs <- within(newsegs, rm(Str_LLID))

names(luHUC4)[names(luHUC4) == 'Stream_LLID'] <- 'LLID_Stream'

newsegs$code <- paste(newsegs$LLID_Stream_Lake, newsegs$Pollutant_ID, newsegs$RM1, newsegs$RM2)

newsegs.HUC4 <- merge(newsegs, luHUC4, by = 'LLID_Stream')

newsegs.HUC4.Code <- ddply(newsegs.HUC4, .(code), function(x) {ifelse(length(unique(x$HUC_4th_Field)) == 1, x$HUC_4th_Field, paste(x$HUC_4th_Field, collapse = ';'))})
names(newsegs.HUC4.Code) <- c('code', 'HUC_4th_Code')
newsegs.HUC4.Name <- ddply(newsegs.HUC4, .(code), function(x) {ifelse(length(unique(x$HUC_4th_Name)) == 1, x$HUC_4th_Name, paste(x$HUC_4th_Name, collapse = ';'))})
names(newsegs.HUC4.Name) <- c('code', 'HUC_4th_Name')

newsegs <- merge(newsegs, newsegs.HUC4.Code, by = 'code', all.x = TRUE)
newsegs <- merge(newsegs, newsegs.HUC4.Name, by = 'code', all.x = TRUE)

#There is a new stream that is on the PNW 24k and not in the luHUC4 table. It's Derford Creek which is a trib to the
#North Santiam river so we'll just do a single update here
#newsegs[newsegs$Stream_Lake_Name == 'Derford Creek','HUC_4th_Code'] <- 17090005
#newsegs[newsegs$Stream_Lake_Name == 'Derford Creek','HUC_4th_Name'] <- 'North Santiam'

#This only captures streams or lakes on streams. We have five lakes that we need to get HUC Fourth info for too.

newsegs.lakes <- newsegs[is.na(newsegs$HUC_4th_Name),]
lakes.to.update <- LLID.Lakes[LLID.Lakes$Lake_LLID %in% unique(newsegs.lakes$LLID_Lake),c('Lake_LLID','Lake_Name', 'Stream_LLID','HUC_3rd_Name', 'HUC_4th_Field', 
                                                                                          'HUC_4th_Name', 'Stream_RM_Min', 'Stream_RM_Max')]
names(lakes.to.update) <- c('LLID_Lake', 'Lake_Name', 'LLID_Stream', 'HUC_3rd_Name', 'HUC_4th_Code', 'HUC_4th_Name', 'RM1', 'RM2')
lakes.to.update$RM1 <- round(lakes.to.update$RM1, 1)
lakes.to.update$RM2 <- round(lakes.to.update$RM2, 1)
lakes.to.update[is.na(lakes.to.update$RM1),c('RM1', 'RM2')] <- 0
lakes.to.update <- merge(lakes.to.update, newsegs.lakes, by = 'LLID_Lake', all.x = TRUE)
lakes.to.update <- within(lakes.to.update, rm(HUC_4th_Name.y, HUC_4th_Code.y, LLID_Stream.y, RM1.y, RM2.y, Lake_Name.y))
lakes.to.update <- rename(lakes.to.update, c('Lake_Name.x' = 'Lake_Name','LLID_Stream.x' = 'LLID_Stream', 'HUC_4th_Code.x' = 'HUC_4th_Code', 
                                             'HUC_4th_Name.x' = 'HUC_4th_Name', 'RM1.x' = 'RM1', 'RM2.x' = 'RM2'))
newsegs$HUC_3rd_Name <- NA
newsegs <- newsegs[!is.na(newsegs$HUC_4th_Name),]
newsegs <- rbind(newsegs, lakes.to.update)

#### Filling in HUC 3rd Name for the streams
#we are going to get the HUC3 code from the epa's HUC 4 code field so first we need to separate all the HUC 4 codes
newsegs.split <- colsplit(newsegs$HUC_4th_Code, pattern = ',|;', names = c('A','B','C','D','E'))
newsegs.split <- as.data.frame(lapply(newsegs.split, as.character))

#now that they are separate we can get the HUC3 code out by taking the first six characters
for (i in names(newsegs.split)) {
  newsegs.split[,i] <- substr(newsegs.split[,i], start = 1, stop = 6)
}

#this is so we can keep track of where they came from
newsegs.split$id <- row.names(newsegs.split)

#now we need to map these to codes to names cause that's what we need in the 'Answer Table'
meltedHUC3 <- melt(newsegs.split, id.vars = 'id', na.rm = T)
meltedHUC3 <- meltedHUC3[nchar(meltedHUC3$value) != 0,]
HUCstoUSE <- unique(luHUC3[luHUC3$HUC_3rd_Field %in% levels(factor(meltedHUC3$value)), c('HUC_3rd_Field', 'HUC_3rd_Name')])
HUCstoUSE <- HUCstoUSE[order(HUCstoUSE$HUC_3rd_Field),]
meltedHUC3$value <- factor(meltedHUC3$value)
levels(meltedHUC3$value) <- HUCstoUSE$HUC_3rd_Name

#now that we have the names we can format it to fill in the epa table
HUC3Named <- dcast(meltedHUC3, id ~ variable)

#this brings them back together in a single field
for (i in 1:nrow(HUC3Named)) {
  HUC3Named$pasted[i] <- paste(na.omit(unique(as.character(HUC3Named[i,2:6]))), collapse = ', ')
}

#this cleans up the data frame and orders it to match the epa table
HUC3Named <- within(HUC3Named, rm(A,B,C,D,E))
HUC3Named$id <- as.numeric(HUC3Named$id)
HUC3Named <- arrange(HUC3Named, id)

#and here is the update. the logic here preserves HUC3 names that came from the LLID Lake table
newsegs$HUC_3rd_Name <- ifelse(is.na(newsegs$HUC_3rd_Name), yes = HUC3Named$pasted, no = newsegs$HUC_3rd_Name)

#### Now we can do the status and status id ####
newsegs$Status <- factor(newsegs$Status)
levels(newsegs$Status) <- revalue(levels(newsegs$Status), c('2' = 'Cat 2:  Attaining some criteria/uses',
                                                            '3' = 'Cat 3:  Insufficient data',
                                                            '3B' = 'Cat 3B:  Potential concern',
                                                            '5' = 'Cat 5: Water quality limited, 303(d) list, TMDL needed'))
newsegs <- rename(newsegs, c('Status' = 'Listing_Status'))
newsegs$Listing_Status <- as.character(newsegs$Listing_Status)
newsegs <- merge(newsegs, status, by = 'Listing_Status', all.x = TRUE)

#### Now the Action and Action ID ####
#i don't know yet which one to use for 3 and 3B so this code is just a framework#
newsegs$Action <- 'Added to database'
newsegs$Action_ID <- '7'
#newsegs[newsegs$Status_ID == 17,c('Action')] <- c('New Cat 5: 303(d) listing') 
#newsegs[newsegs$Status_ID == 17,c('Action_ID')] <- c('12')
#newsegs[newsegs$Status_ID == 12,c('Action')] <- c('New Cat 2:  Attaining')
#newsegs[newsegs$Status_ID == 12,c('Action_ID')] <- c('15')
#newsegs[newsegs$Status_ID %in% c(13,19),c('Action')] <- c('Added to database')
#newsegs[newsegs$Status_ID %in% c(13,19),c('Action_ID')] <- c('7')

#### Filling in the other fields in the Assessment Report Summary ####
newsegs$Beach_Name <- NA
newsegs$EPA_Beach_ID <- NA

#Filling in the miles info
newsegs$Miles <- paste(newsegs$RM1, 'to', newsegs$RM2)
newsegs$SegmentMiles <- round(newsegs$RM2 - newsegs$RM1, 1)

#These assessment pieces are just placeholders and will get filled in when combined with the existsegs
newsegs$Assessment_ID <- NA
newsegs$AssessmentYear <- 2012
newsegs$AssessmentDate <- Sys.Date()
newsegs$TMDLInfo <- ''

#Pulling in AffectedUses from the assessment report summary
#for the toxics
criteria <- read.csv('//DEQHQ1/wqassessment/2012_WQAssessment/Segmentation/Data_used/Pollutants_complete.csv', stringsAsFactors = F)
criteria <- within(criteria, rm(Category, crit_val))
newsegs <- merge(newsegs, criteria, by = 'Pollutant', all.x = TRUE)

#for the DO will come later

#There are no narrative criteria for Toxics or Dissolved Oxygen so we don't have to worry about that (except for Beryllium)
#We do have to map the criteria to Table 20 or Table 40 which is pretty straightforward
newsegs[newsegs$Criteria %in% c('HHO', 'HHWO'),'Criteria'] <- 'Table 40 Human Health Criteria for Toxic Pollutants'
newsegs[newsegs$Criteria == 'FWCHRONIC','Criteria'] <- 'Table 20 Toxic Substances'
numcrit <- numcrit[,c('Criteria', 'NumericCriteria_ID')]
newsegs <- merge(newsegs, numcrit, by = 'Criteria', all.x = TRUE)
newsegs[newsegs$Pollutant == 'Beryllium','NumericCriteria_ID'] <- 9
newsegs[newsegs$Pollutant == 'Beryllium','Criteria'] <- narcrit[narcrit$NarrativeCriteria_ID == 9,'NarrativeText']

#making sure the fish tissue mercury affected use is human health
newsegs[newsegs$Pollutant == 'Mercury' & newsegs$Criteria == 'Table 40 Human Health Criteria for Toxic Pollutants','AffectedUses'] <- 'Human Health'

#This fills in the Year for listings in this cycle
#newsegs$ListingYear <- ifelse(newsegs$Status_ID == 17, 2012, '')
newsegs$ListingYear <- 'Previous Assessment Year: NA'

#These are new records so they don't have Previous Status or Action and comments don't need to be filled in
newsegs$PreviousStatus <- ''
newsegs$PreviousAction <- ''
newsegs$Comments <- ''

newsegs$Record_ID <- unlist(newsegs$Record_ID)

#Let's add '2012 Data:' to the Summary text here
newsegs$Summary <- paste('2012 Data: ',newsegs$Summary, sep = '\r\n')

#Removing some intermediate variables
newsegs <- within(newsegs, rm(code, segcheck))
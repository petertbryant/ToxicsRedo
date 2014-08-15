allrecs <- rbind(existsegs, newsegs)

max.assess.id <- max(ars$Assessment_ID)
nextid <- max.assess.id[1] + 1
for (i in 1:nrow(allrecs)) {
  allrecs$Assessment_ID[i] <- nextid
  nextid <- nextid + 1
}
allrecs$Assessment_ID <- unlist(allrecs$Assessment_ID)
allrecs$AssessmentYear <- 2012
allrecs$AssessmentDate <- Sys.Date()
allrecs[allrecs$Criteria == 'Table 30 Toxic Substances','Criteria'] <- 'Table 30 Aquatic Life Criteria for Toxic Pollutants'

ars.slim <- ars[!ars$Record_ID %in% allrecs$Record_ID,]
ars.slim[which(ars.slim$Action != 'No 2010 action'),'PreviousAction'] <- paste('Previous Action:', ars.slim[which(ars.slim$Action != 'No 2010 action'),'Action'])
ars.slim[which(ars.slim$AssessmentYear == 2012),'PreviousAction'] <- NA
ars.slim[which(ars.slim$AssessmentYear != 2012),'Action'] <- 'No Action'
ars.slim[which(ars.slim$AssessmentYear != 2012),'Action_ID'] <- 21
ars.slim <- ars.slim[ars.slim$Listing_Status != 'Inactive',]
ars.slim <- within(ars.slim, rm(code))

ars.2012 <- rbind(ars.slim, allrecs)

#Ensuring RM are all to 1 decimal place and the miles field reflects that
ars.2012$RM1 <- round(ars.2012$RM1, 1)
ars.2012$RM2 <- round(ars.2012$RM2, 1)
ars.2012$Miles <- paste(ars.2012$RM1, 'to', ars.2012$RM2)

#ars.2012 status alignment
ars.2012[ars.2012$Listing_Status == "Cat 2:  Attaining some criteria/uses",'Listing_Status'] <- "Cat 2: Attaining some criteria/uses"
ars.2012[ars.2012$Listing_Status == "Cat 2: Attaining some criteria/uses",'Status_ID'] <- 12
ars.2012[ars.2012$Listing_Status == "Cat 3:  Insufficient data",'Listing_Status'] <- "Cat 3: Insufficient data"
ars.2012[ars.2012$Listing_Status == "Cat 3: Insufficient data",'Status_ID'] <- 13
ars.2012[ars.2012$Listing_Status == "Cat 3B:  Potential concern",'Listing_Status'] <- "Cat 3B: Potential concern"
ars.2012[ars.2012$Listing_Status == "Cat 3B: Potential concern",'Status_ID'] <- 19

#associating stations to records
# for (i in 1:nrow(stations.newrecs)) {
#   if (any(newsegs$LLID_Stream == stations.newrecs$STREAM_LLID[i], na.rm = TRUE)) {
#     matched.seg <- newsegs[which(newsegs$LLID_Stream == stations.newrecs$STREAM_LLID[i]),]
#   } else {
#     matched.seg <- newsegs[which(newsegs$LLID_Lake == stations.newrecs$LAKE_LLID[i]),]
#   }  
#   
#   matched.seg.pol <- subset(matched.seg, matched.seg$Pollutant %in% stations.newrecs$Pollutant[i])
#   #matched.seg.pol.seas <- subset(matched.seg.pol, matched.seg.pol$Season == stations.newrecs$Season[i])
#   #matched.seg.pol.seas.crit <- subset(matched.seg.pol.seas, matched.seg.pol.seas$Criteria == stations.newrecs$Criteria[i])
#   matched.seg.pol <- plyr::arrange(matched.seg.pol, desc(SampleMatrix_ID))
#   
#   if (nrow(matched.seg.pol) > 0) {
#     if (!any(is.na(matched.seg.pol$LLID_Stream))) {
#       if (!any(is.na(matched.seg.pol$RM1))) {
#         for (j in 1:nrow(matched.seg.pol)) {
#           if(stations.newrecs$RIVER_MILE[i] >= matched.seg.pol$RM1[j] & stations.newrecs$RIVER_MILE[i] <= matched.seg.pol$RM2[j]) {
#             stations.newrecs$RecordID[i] <- matched.seg.pol$Record_ID[j]
#           } 
#         }
#       }
#     } else {
#       stations.newrecs$RecordID[i] <- matched.seg.pol$Record_ID[1] 
#     }
#   }
# }
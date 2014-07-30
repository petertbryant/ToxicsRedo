#Working through the snt.w.LLIDs there are 35 unique LLID + Pollutant combinations that need updating and this code goes through them
#The first scenario is when there is an existing record that ends on the stream below where we have new stations. Instead of stepping through
#each case to discern if the existing segment should be extended, a new segment will be created from the end of the existing segment and extend
#to the headwaters

to.deal.with <- unique(snt.w.LLIDs$code)
snt.w.LLIDs$exceed <- round(as.numeric(snt.w.LLIDs$exceed), 1)
snt.w.LLIDs$total_n <- round(as.numeric(snt.w.LLIDs$total_n), 1)
snt.w.LLIDs <- plyr::arrange(snt.w.LLIDs, RIVER_MILE)
ars$code <- paste(ars$LLID_Stream, ars$Pollutant_ID)

newsegs <- data.frame('STREAM_LLID' = character(), 'NAME' = character(),'SegmentID' = character(), 
                      'RecordID' = character(), 'Pollutant' = character(), 'value' = character(),
                      'Pollutant_ID' = character(), 'Summary' = character(), 'LLID_Stream_Lake' = character(),
                      'LAKE_LLID' = character(), 'LAKE_NAME' = character(),
                      'Stream_Name' = character(), stringsAsFactors = F) 

for (i in 1:length(to.deal.with)) {
  stations.of.interest <- snt.w.LLIDs[snt.w.LLIDs$code == to.deal.with[i],]
  ars.of.interest <- ars[ars$code == to.deal.with[i],]
  #Add if else here for LLID match. There are only STReam LLIDs in this set so we don't have to make it smarter.
  LLID.of.interest <- LLID.Streams[LLID.Streams$LLID == substr(to.deal.with[i], 1, 13),]
  segments.of.interest <- segments[which(segments$LLID_Stream == substr(to.deal.with[i], 1, 13)),]
  
  Summary.text <- ddply(stations.of.interest, .(code), text.summary)[,2]
  
  seg.of.interest <- stations.of.interest[1,c('STREAM_LLID', 'SegmentID', 'RecordID', 'Pollutant', 'Pollutant_ID','Stream_Name','value')] #'LLID_Stream_Lake',
                                                                                                                                          #'LAKE_LLID', 'LAKE_NAME', 
  seg.of.interest$NAME <- ars.of.interest[1,'Stream_Lake_Name']
  seg.of.interest$Summary <- Summary.text
  
  seg.of.interest$Status <- ifelse(all.equal(stations.of.interest$cat, stations.of.interest$cat),
                                     stations.of.interest$cat[1],
                                     ifelse(any(stations.of.interest$cat == 3),
                                            ifelse(all(stations.of.interest[stations.of.interest$cat != 3,'cat'] == 2,
                                                       '2',
                                                       ifelse(all(stations.of.interest[stations.of.interest$cat != 3,'cat'] == '3B'),
                                                              '3B',
                                                              ifelse(all(stations.of.interest[stations.of.interest$cat != 3,'cat'] == '5'),
                                                                     '5',
                                                                     'mixed'))))))
  
  newsegs <- rbind(newsegs, seg.of.interest)
}

newsegs$RM1 <- NA
newsegs$RM2 <- NA

segments$RM1 <- as.numeric(segments$RM1)
segments$RM2 <- as.numeric(segments$RM2)
<<<<<<< HEAD
i <- 32
cat3.tdw <- unique(newsegs[newsegs$Status == '3',c('STREAM_LLID','Pollutant_ID')])
View(ars[which(ars$LLID_Stream == cat3.tdw[i,1] & ars$Pollutant_ID == cat3.tdw[i,2]),])
View(arrange(segments[which(segments$LLID_Stream == substr(cat3.tdw[i,1], 1, 13)),],RM1,RM2))
arrange(stations.newrecs[which(stations.newrecs$STREAM_LLID == cat3.tdw[i,1] & stations.newrecs$Pollutant_ID == cat3.tdw[i,2]),],RIVER_MILE)
LLID.Streams.sub[LLID.Streams.sub$LLID == cat3.tdw[i,'STREAM_LLID'],]
=======
i <- 5
cat2.tdw <- unique(newsegs[newsegs$Status == '2',c('LLID_Stream','Pollutant_ID')])
View(ars[which(ars$LLID_Stream == cat2.tdw[i,1] & ars$Pollutant_ID == cat2.tdw[i,2]),])
View(arrange(segments[which(segments$LLID_Stream == substr(cat3.tdw[i,1], 1, 13)),],RM1,RM2))
arrange(stations.newrecs[stations.newrecs$LLID_Stream == cat2.tdw[i,1] & stations.newrecs$Pollutant_ID == cat2.tdw[i,2],],RIVER_MILE)
>>>>>>> 07936b7a19af8dddb31a98a0916e56cda1745de8

#### These are all the simple ones where we don't have to do any additional cutting ####
newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '2182'),c('RM1', 'RM2')] <- c('24.8', '186.6')
newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '2181'),c('RM1', 'RM2')] <- c('24.8', '186.6')
newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '2183'),c('RM1', 'RM2')] <- c('24.8', '186.6')
newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '12697'),c('RM1', 'RM2')] <- c('24.8', '186.6')
<<<<<<< HEAD
newsegs[which(newsegs$STREAM_LLID == '1169731440585' & newsegs$Pollutant_ID %in% c('2227','2189'),c('RM1','RM2')] <- c('67.0', '186.1')

#Mike's edits
newsegs[which(newsegs$STREAM_LLID == '1226500453377' & newsegs$Pollutant_ID == '2182'),c('RM1', 'RM2')] <- c('44.7', '80.7')
newsegs[which(newsegs$STREAM_LLID == '1238576461676' & newsegs$Pollutant_ID == '2206'),c('RM1', 'RM2')] <- c('8.5', '27.5')
newsegs[which(newsegs$STREAM_LLID == '1240483462464' & newsegs$Pollutant_ID == '2207'),c('RM1', 'RM2')] <- c('142.0', '303.9')
newsegs[which(newsegs$STREAM_LLID == '1227161452842' & newsegs$Pollutant_ID == '2207'),c('RM1', 'RM2')] <- c('35.4', '61.7')
newsegs[which(newsegs$STREAM_LLID == '1234460432681' & newsegs$Pollutant_ID == '2208'),c('RM1', 'RM2')] <- c('52.0', '105.6')
newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '2250'),c('RM1', 'RM2')] <- c('24.8', '186.6')
newsegs[which(newsegs$STREAM_LLID == '1226500453377' & newsegs$Pollutant_ID == '2250'),c('RM1', 'RM2')] <- c('44.7', '80.7')
newsegs[which(newsegs$STREAM_LLID == '1227161452842' & newsegs$Pollutant_ID == '2217'),c('RM1', 'RM2')] <- c('35.4', '61.7')
newsegs[which(newsegs$STREAM_LLID == '1238951456889' & newsegs$Pollutant_ID == '2217'),c('RM1', 'RM2')] <- c('22.0', '120.0')
newsegs[which(newsegs$STREAM_LLID == '1231973440631' & newsegs$Pollutant_ID == '2218'),c('RM1', 'RM2')] <- c('0.0', '3.9')
newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '2219'),c('RM1', 'RM2')] <- c('24.8', '186.6')
newsegs[which(newsegs$STREAM_LLID == '1231973440631' & newsegs$Pollutant_ID == '2225'),c('RM1', 'RM2')] <- c('0.0', '3.9')
newsegs[which(newsegs$STREAM_LLID == '1226500453377' & newsegs$Pollutant_ID == '2183'),c('RM1', 'RM2')] <- c('44.7', '80.7')
newsegs[which(newsegs$STREAM_LLID == '1230233440232' & newsegs$Pollutant_ID == '2187'),c('RM1', 'RM2')] <- c('31.3', '38.8')
newsegs[which(newsegs$STREAM_LLID == '1227161452842' & newsegs$Pollutant_ID == '2187'),c('RM1', 'RM2')] <- c('35.4', '61.7')
newsegs[which(newsegs$STREAM_LLID == '1226500453377' & newsegs$Pollutant_ID == '2190'),c('RM1', 'RM2')] <- c('44.7', '80.7')
newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '2189'),c('RM1', 'RM2')] <- c('72.0', '186.6')
newsegs[which(newsegs$STREAM_LLID == '1227161452842' & newsegs$Pollutant_ID == '2189'),c('RM1', 'RM2')] <- c('35.4', '61.7')
newsegs[which(newsegs$STREAM_LLID == '1169731440585' & newsegs$Pollutant_ID == '2189'),c('RM1', 'RM2')] <- c('67.0', '186.1')
newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '2193'),c('RM1', 'RM2')] <- c('0.0', '119.7')
newsegs[which(newsegs$STREAM_LLID == '1231973440631' & newsegs$Pollutant_ID == '2193'),c('RM1', 'RM2')] <- c('0.0', '3.9')
newsegs[which(newsegs$STREAM_LLID == '1227161452842' & newsegs$Pollutant_ID == '2227'),c('RM1', 'RM2')] <- c('35.4', '61.7')
newsegs[which(newsegs$STREAM_LLID == '1226500453377' & newsegs$Pollutant_ID == '2227'),c('RM1', 'RM2')] <- c('44.7', '80.7')
newsegs[which(newsegs$STREAM_LLID == '1169731440585' & newsegs$Pollutant_ID == '2227'),c('RM1', 'RM2')] <- c('67.0', '186.1')
newsegs[which(newsegs$STREAM_LLID == '1226500453377' & newsegs$Pollutant_ID == '2228'),c('RM1', 'RM2')] <- c('44.7', '80.7')
newsegs[which(newsegs$STREAM_LLID == '1231973440631' & newsegs$Pollutant_ID == '2230'),c('RM1', 'RM2')] <- c('0.0', '3.9')
newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '2239'),c('RM1', 'RM2')] <- c('24.8', '186.6')
newsegs[which(newsegs$STREAM_LLID == '1226500453377' & newsegs$Pollutant_ID == '2239'),c('RM1', 'RM2')] <- c('44.7', '80.7')
newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '2240'),c('RM1', 'RM2')] <- c('24.8', '186.6')
newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '2243'),c('RM1', 'RM2')] <- c('24.8', '186.6')
newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '2248'),c('RM1', 'RM2')] <- c('24.8', '186.6')
newsegs[which(newsegs$STREAM_LLID == '1224573455551' & newsegs$Pollutant_ID == '2248'),c('RM1', 'RM2')] <- c('0.0', '4.5') # This was originally an LLID/LAKE_LLID
newsegs[which(newsegs$STREAM_LLID == '1231445452258' & newsegs$Pollutant_ID == '2255'),c('RM1', 'RM2')] <- c('18.1', '61.7')
newsegs[which(newsegs$STREAM_LLID == '1227161452842' & newsegs$Pollutant_ID == '2255'),c('RM1', 'RM2')] <- c('35.4', '61.7')
newsegs[which(newsegs$STREAM_LLID == '1227161452842' & newsegs$Pollutant_ID == '2257'),c('RM1', 'RM2')] <- c('35.4', '61.7')
newsegs[which(newsegs$STREAM_LLID == '1238951456889' & newsegs$Pollutant_ID == '2257'),c('RM1', 'RM2')] <- c('22.0', '120.0')
newsegs[which(newsegs$STREAM_LLID == '1226500453377' & newsegs$Pollutant_ID == '2258'),c('RM1', 'RM2')] <- c('44.7', '80.7')
newsegs[which(newsegs$STREAM_LLID == '1240483462464' & newsegs$Pollutant_ID == '2260'),c('RM1', 'RM2')] <- c('142.0', '303.9')
newsegs[which(newsegs$STREAM_LLID == '1231445452258' & newsegs$Pollutant_ID == '2260'),c('RM1', 'RM2')] <- c('18.1', '61.7')
newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '2261'),c('RM1', 'RM2')] <- c('24.8', '186.6')
newsegs[which(newsegs$STREAM_LLID == '1231445452258' & newsegs$Pollutant_ID == '2265'),c('RM1', 'RM2')] <- c('18.1', '61.7')
newsegs[which(newsegs$STREAM_LLID == '1239194460155' & newsegs$Pollutant_ID == '2266'),c('RM1', 'RM2')] <- c('0.2', '4.0')

=======
newsegs[which(newsegs$STREAM_LLID == '1169731440585' & newsegs$Pollutant_ID %in% c('2227','2189')),c('RM1','RM2')] <- c('67.0', '186.1')
>>>>>>> 07936b7a19af8dddb31a98a0916e56cda1745de8

# #This Willamette River one has a segment in the middle so we need to create two new segments. One above and one below.
newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '2258'),c('RM1', 'RM2', 'Status')] <- c('24.8', '108', '2')
newrow <- newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '2258'),]
newrow$Summary <- gsub('\r\n','',strsplit(newsegs[which(newsegs$STREAM_LLID == '1227618456580' & newsegs$Pollutant_ID == '2258'),'Summary'], split = ';')[[1]][4])
newsegs[which(newsegs$STREAM_LLID == '1227618456580' & 
          newsegs$Pollutant_ID == '2258'),'Summary'] <- paste(strsplit(newsegs[which(newsegs$STREAM_LLID == '1227618456580' & 
                                                                                newsegs$Pollutant_ID == '2258'),'Summary'], split = ';')[[1]][1:3], 
                                                             collapse = ';')
newrow[,c('RM1', 'RM2', 'Status')] <- c('148.8', '186.6', '2')
newsegs <- rbind(newsegs, newrow)

# #### These are the ones where we have to apply the segmentation logic ####
# #The farthest upstream station is attaining while the stations from 44.7 to 72.9 (the RM of the attaining station) are 5,5,3,3,3,5 
# #(in order from downstream to upstream). According to the segmentation methodology because no segment is defined for this reach we make 
# #a segment of status 5 up to the next station that shows a different status (the station at RM 72.9). So the result is two new segments
# #one from 44.7 to 72.9 and the other from 72.9 to 80.7 (the headwaters)
# newsegs[which(newsegs$Str_LLID == '1226500453377' & newsegs$Pollutant_ID == '2187'),c('RM1', 'RM2', 'Status')] <- c('44.7', '72.9', '5')
# newrow <- newsegs[which(newsegs$Str_LLID == '1226500453377' & newsegs$Pollutant_ID == '2187'),]
# newrow$Summary <- strsplit(newsegs[which(newsegs$Str_LLID == '1226500453377' & newsegs$Pollutant_ID == '2187'),'Summary'], split = ';')[[1]][7]
# newsegs[which(newsegs$Str_LLID == '1226500453377' & 
#           newsegs$Pollutant_ID == '2187'),'Summary'] <- paste(strsplit(newsegs[which(newsegs$Str_LLID == '1226500453377' & 
#                                                                                 newsegs$Pollutant_ID == '2187'),'Summary'], 
#                                                                       split = ';')[[1]][1:6], collapse = ';')
# newrow[,c('RM1', 'RM2', 'Status')] <- c('72.9', '80.7', '2')
# newsegs <- rbind(newsegs, newrow)
# 
# #This Malheur River one has a segment in the middle so we need to create two new segments. One above and one below.
# newsegs[which(newsegs$Str_LLID == '1169731440585' & newsegs$Pollutant_ID == '2255'),c('RM1', 'RM2', 'Status')] <- c('0', '49', '3')
# newrow <- newsegs[which(newsegs$Str_LLID == '1169731440585' & newsegs$Pollutant_ID == '2255'),]
# newrow$Summary <- strsplit(newsegs[which(newsegs$Str_LLID == '1169731440585' & newsegs$Pollutant_ID == '2255'),'Summary'], split = ';')[[1]][5]
# newsegs[which(newsegs$Str_LLID == '1169731440585' & 
#           newsegs$Pollutant_ID == '2255'),'Summary'] <- paste(strsplit(newsegs[which(newsegs$Str_LLID == '1169731440585' & 
#                                                                                 newsegs$Pollutant_ID == '2255'),'Summary'], split = ';')[[1]][1:4], 
#                                                              collapse = ';')
# newrow[,c('RM1', 'RM2', 'Status')] <- c('126.8', '185.9', '3')
# newsegs <- rbind(newsegs, newrow)
# 
# #This one has two attaining stations and then two cat 5 stations bookending two insufficient data stations and another attaining station upstream
# newsegs[which(newsegs$Str_LLID == '1226500453377' & newsegs$Pollutant_ID == '2257'),c('RM1', 'RM2', 'Status')] <- c('44.7', '55.9', '2')
# newrow1 <- newsegs[which(newsegs$Str_LLID == '1226500453377' & newsegs$Pollutant_ID == '2257'),]
# newrow2 <- newsegs[which(newsegs$Str_LLID == '1226500453377' & newsegs$Pollutant_ID == '2257'),]
# newrow1$Summary <- paste(strsplit(newsegs[which(newsegs$Str_LLID == '1226500453377' & 
#                                           newsegs$Pollutant_ID == '2257'),'Summary'], 
#                                   split = ';')[[1]][3:6], collapse = ';')
# newrow2$Summary <- strsplit(newsegs[which(newsegs$Str_LLID == '1226500453377' & newsegs$Pollutant_ID == '2257'),'Summary'], split = ';')[[1]][7]
# newsegs[which(newsegs$Str_LLID == '1226500453377' & 
#           newsegs$Pollutant_ID == '2257'),'Summary'] <- paste(strsplit(newsegs[which(newsegs$Str_LLID == '1226500453377' & 
#                                                                               newsegs$Pollutant_ID == '2257'),'Summary'], 
#                                                                       split = ';')[[1]][1:2], collapse = ';')
# newrow1[,c('RM1', 'RM2', 'Status')] <- c('55.9', '72.9', '5')
# newrow2[,c('RM1', 'RM2', 'Status')] <- c('72.9', '80.7', '2')
# newsegs <- rbind(newsegs, newrow1)
# newsegs.exceptions <- rbind(newsegs, newrow2)

newsegs.exceptions <- within(newsegs, rm(SegmentID, RecordID, NAME))

#rm(list = setdiff(ls()[sapply(ls(),function(n){!is.function(get(n))})], 
#   c('newsegs.exceptions', 'stations.newrecs.toxics', 'stations.newrecs.do', 
#     'stations.existrecs', 'stations.newrecs', 'ars', 
#     'LLID.Streams', 'LLID.Streams.sub','segments', 'LLID.Lakes', 'LLIDs.w.snt', 'newsegs.do',
#     'luHUC4','max.segment.id', 'max.record.id', 'luHUC3', 'status', 'numcrit', 'stations.all','narcrit','ars.waterOnly')))

#rm(to.deal.with, snt.w.LLIDs, newrow1, newrow2, newsegs, stations.of.interest, ars.of.interest, LLID.of.interest, segments.of.interest,
#   Summary.text, seg.of.interest)



#### These are all the reference tables you need to determine how to break up segments 
#### and to make sure you're not differing from existing segments by 0.1
#(ars[ars$LLID_Stream_Lake == '1240483462464' & ars$Pollutant_ID == '2285',setdiff(names(ars), c('Summary'))])
#snt.w.LLIDs[snt.w.LLIDs$Str_LLID == '1240483462464' & snt.w.LLIDs$Pollutant_ID == '2285',]
#LLID.Streams[LLID.Streams$LLID == '1240483462464',]
#segments[segments$LLID_Stream_Lake == '1240483462464',]

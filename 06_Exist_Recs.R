existars <- ars[ars$Record_ID %in% stations.existrecs$RecordID, c('Listing_Status', 'Record_ID','Action','PreviousAction','ListingYear')]

stations.existrecs <- merge(stations.existrecs,existars,by.x='RecordID',by.y='Record_ID')

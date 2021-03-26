
## Cleaned Locs - Calculate NN ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data w/ rdm points
# Outputs: Cleaned collar data + NNID + NNdist 

### Packages ----
libs <- c( 'ggplot2', 'rgdal',  'data.table',
          'spatsoc', 'igraph','devtools', 'asnipe')
lapply(libs, require, character.only = TRUE)

#devtools::install_github('ropensci/spatsoc')
#install_local("/Users/quinnwebber/Downloads/data.table-1.12.6.zip")

### Input raw data ----
DT <- readRDS("output/location-data/2-clean-all-rdm.RDS")

## extract year
DT$Year <- as.factor(DT$Year)
DT[,JDate := yday(t1_)]

# Temporal grouping
DT <- group_times(DT, datetime = 't1_', threshold = '5 minutes')
DT <- DT[order(DT$timegroup),]
DT$timegroupStart <- DT$timegroup
DT$timegroupEnd <- DT$timegroupStart + 1
DT[,c("timegroup") := NULL]

DT$IDYr <- as.factor(paste(DT$ANIMAL_ID, DT$Year, sep = "_"))
DT$IDYrTimeIter <- as.factor(paste(DT$IDYr, DT$timegroupEnd, DT$iter, sep = "_"))
#####################################################
############# CALCULDATE DISTANCE ##################
####################################################

## Nearest neighbor at end step
edistEnd <- edge_dist(DT = DT, id = 'IDYr', coords = c('x2_', 'y2_'),
                   timegroup = 'timegroupEnd', threshold = 45000, returnDist = TRUE, 
                   splitBy = c("iter", "Year"))
colnames(edistEnd)[6] <- "EndDist"
edistEnd <- edistEnd[!is.na(edistEnd$EndDist)]

edistEnd <- edistEnd[edistEnd[,.I[which.min(EndDist)],by=. (ID1, timegroupEnd, iter)][['V1']]]
edistEnd$IDYrTimeIter <- as.factor(paste(edistEnd$ID1, 
                                         edistEnd$timegroupEnd, 
                                         edistEnd$iter, sep = "_"))
edistEnd[,c("ID1", "timegroupEnd", "iter") := NULL]
colnames(edistEnd)[2] <- "EndNN_ID"


## Nearest neighbor at starting step
edistStart <- edge_dist(DT = DT, id = 'IDYr', coords = c('x1_', 'y1_'),
                   timegroup = 'timegroupStart', threshold = 45000, returnDist = TRUE, 
                   splitBy = c("iter", "Year"))
colnames(edistStart)[6] <- "StartDist"
edistStart <- edistStart[!is.na(edistStart$StartDist)]

edistStart <- edistStart[edistStart[,.I[which.min(StartDist)],by=. (ID1, timegroupStart, iter)][['V1']]]
edistStart$IDYrTimeIter <- as.factor(paste(edistStart$ID1, 
                                           edistStart$timegroupStart, 
                                           edistStart$iter, sep = "_"))
edistStart[,c("ID1", "timegroupStart", "iter", "Year") := NULL]
colnames(edistStart)[1] <- "StartNN_ID"

edist <- merge(edistStart, edistEnd, by = "IDYrTimeIter")

edist[,c("Year") := NULL]

DT <- merge(DT, edist, by = "IDYrTimeIter")

DT[, c("IDYrTimeIter", "minutes", "timegroup") := NULL]

###### GENERATE NETWORKS FROM RANDOM POINTS ######
DT <- group_pts(
  DT,
  threshold = 50,
  splitBy = c('Year', 'iter'),
  timegroup = 'timegroupEnd',
  id = 'ANIMAL_ID',
  coords = c('x2_', 'y2_')
)

DT$groupEnd <- DT$group

DT$IDYrIter <- as.factor(paste(DT$IDYr, DT$iter, sep = "_"))

DT[, c("group") := NULL]

saveRDS(DT, "output/3-rdm-locs-NN.RDS")




## Cleaned Locs - Calculate Annual Community Assignment ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data w/o rdm points
# Outputs: Individual annual community assignment

### Packages ----
libs <- c('data.table','rgdal', 'ggplot2',
          'spatsoc', 'igraph', 'asnipe')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
DT <- readRDS("output/location-data/1-clean-all.RDS")
DT <- DT[season == "winter"]

DT[, .N, by = "IDYr"]


###### GENERATE NETWORKS FOR OBSERVED DATA ######

DT <- group_times(DT, datetime = 'datetime', threshold = '5 minutes')


DT <- group_pts(
  DT,
  threshold = 50,
  splitBy = c('lagYear'),
  timegroup = 'timegroup',
  id = 'ANIMAL_ID',
  coords = c('EASTING', 'NORTHING')
)

DT[, .N, by = "IDYr"]

ggplot(DT) +
  geom_point(aes(EASTING, NORTHING, color = ANIMAL_ID)) +
  facet_wrap(~lagYear)

#### Calculate network metrics ####
source("functions/dynamic_network2.R")

mods <- dynamic_network(DT, id = 'ANIMAL_ID', 
                        by = c('lagYear'))
saveRDS(mods, "output/6-network-stats.RDS")

mods$ID1 <- mods$ID
mods$Year[mods$lagYear == "Year1"] <- "2017"
mods$Year[mods$lagYear == "Year2"] <- "2018"
mods$Year[mods$lagYear == "Year3"] <- "2019"
mods$IDLagYr <- as.factor(paste(mods$ID1, mods$lagYear, sep = "_"))
mods2 <- mods[,c("IDLagYr", "membership")]

#### merge community assignment to DT file ####

DT2 <- merge(mods2, DT, by = "IDLagYr")

saveRDS(DT2, "output/location-data/7-locs-mod.RDS")

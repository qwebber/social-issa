
## Cleaned Locs - Calculate community assortment ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: list of random networks, DT random metrics, observed igraphs for each year
# Outputs: 

### Packages ----
libs <- c('data.table', 'spatsoc', 'asnipe', 'igraph',
          'reshape2', 'assortnet', 'ggplot2')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
DT <- readRDS("output/1-clean-all.RDS")

DT <- DT[season == "winter"]

DT <- group_times(DT, datetime = 'datetime', threshold = '5 minutes')


DT <- group_pts(
  DT,
  threshold = 50,
  splitBy = c('lagYear'),
  timegroup = 'timegroup',
  id = 'ANIMAL_ID',
  coords = c('EASTING', 'NORTHING')
)

## Network statistics
obs <- readRDS("output/6-network-stats.RDS")

## split dataset by herd/Yr
fogo17 <- DT[lagYear == "Year1"]
fogo18 <- DT[lagYear == "Year2"]
fogo19 <- DT[lagYear == "Year3"]


source("functions/calc_rc.R")
rc_fogo2017 <- calc_rc(get_gbi(fogo17,  id = 'ANIMAL_ID', group = 'group'), 
        n.bootstraps=100, plot.result=T)
rc_fogo2018 <- calc_rc(get_gbi(fogo18,  id = 'ANIMAL_ID', group = 'group'), 
        n.bootstraps=100, plot.result=T)
rc_fogo2019 <- calc_rc(get_gbi(fogo19,  id = 'ANIMAL_ID', group = 'group'), 
        n.bootstraps=100, plot.result=T)

## Rcom = 0 indicates no confidence in the assignment of an individual to its community
## Rcom = 1 indicates certainty in the assignment of an individual to its community

sumStats = data.table(N = c(length(unique(fogo17$ANIMAL_ID)),
                 length(unique(fogo18$ANIMAL_ID)),
                 length(unique(fogo19$ANIMAL_ID))),
          rc = c(rc_fogo2017, rc_fogo2018, rc_fogo2019),
          Q = c(mean(obs[lagYear == "Year1"]$Q),
                mean(obs[lagYear == "Year2"]$Q),
                mean(obs[lagYear == "Year3"]$Q)),
         obs[, uniqueN(membership), by = c("lagYear")])
 
saveRDS(sumStats, "output/8-rcom.RDS")

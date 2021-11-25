

## Cleaned Locs -  SRI ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data
# Outputs: SRI  file

### Packages ----
libs <- c('data.table', 'spatsoc', 'igraph')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
fogo <- readRDS("output/location-data/3-rdm-locs-NN-N20.RDS")
fogo$Year <- as.factor(fogo$Year)
fogo$IDYrIter <- as.factor(paste(fogo$IDYr, fogo$iter, sep = "_"))

#####################################################
############# CALCULATE MOVING WINDOW SRI ###########
####################################################

source("functions/get_sri.R")
source("functions/moving_window.R")

out <- c()
timeOut2017 <- c()
timeOut2018 <- c()
timeOut2019 <- c()

for(i in 1:21) {
  timeOut2017[[i]] <- fogo[iter == i & 
                         Year == "2017"][, moving_window(.SD, 75, 
                                                                   by = c("iter", "Year"))]
}

for(i in 1:21) {
  timeOut2018[[i]] <- fogo[iter == i & 
                         Year == "2018"][, moving_window(.SD, 75, 
                                                         by = c("iter", "Year"))]
}

for(i in 1:21) {
  timeOut2019[[i]] <- fogo[iter == i & 
                             Year == "2019"][, moving_window(.SD, 75, 
                                                             by = c("iter", "Year"))]
}

timeOut <- rbind(rbindlist(timeOut2017), 
                      rbindlist(timeOut2018), 
                      rbindlist(timeOut2019)) 

#timeOut[, comm := (membershipID1==membershipID2)]
#timeOut$comm[timeOut$comm == "TRUE"] <- "Same"
#timeOut$comm[timeOut$comm == "FALSE"] <- "Different"
#timeOut$comm <- as.factor(timeOut$comm)
#timeOut$membershipID1 <- as.factor(timeOut$membershipID1)
#timeOut$membershipID2 <- as.factor(timeOut$membershipID2)

saveRDS(timeOut, "output/location-data/4-sri-N20.RDS")


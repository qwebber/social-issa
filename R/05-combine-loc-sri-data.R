

## NN and SRI derived data ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data
# Outputs: combined SRI and NN file

### Packages ----
libs <- c('data.table')
lapply(libs, require, character.only = TRUE)

## Load rdm locs data 
fogo <- readRDS("output/location-data/3-rdm-locs-NN-N20.RDS")
fogo[, c("ANIMAL_ID", "Y") := tstrsplit(IDYr, "_", fixed=TRUE)][, c("Y") := NULL]

fogo$DyadYrIter1 <- as.factor(paste(fogo$IDYr,
                                   fogo$StartNN_ID, 
                                   fogo$iter,
                                   fogo$JDate, sep = "_"))
fogo$DyadYrIter2 <- as.factor(paste(fogo$IDYr,
                                    fogo$StartNN_ID, 
                                    fogo$iter,
                                    fogo$JDate, sep = "_"))


sri <- readRDS("output/location-data/4-sri-N20.RDS")
sri$DyadYrIter1 <- as.factor(paste(sri$ID1, sri$ID2, sri$iter, sri$JDate, sep = "_"))
sri$DyadYrIter2 <- as.factor(paste(sri$ID2, sri$ID1, sri$iter, sri$JDate, sep = "_"))

sri[,c("Year", "JDate", "iter") := NULL]

## merge modularity data with locs data
aa <- merge(sri, fogo,  by = "DyadYrIter1")
bb <- merge(sri, fogo,  by = "DyadYrIter2")

fogo2 <- rbind(aa[,c("DyadYrIter1", 
            "DyadYrIter2.x",
            "DyadYrIter2.y") := NULL],
      bb[,c("DyadYrIter1.y", 
            "DyadYrIter1.x",
            "DyadYrIter2") := NULL])


## extract EndNN_ID and IDYrIter from main dataset
#EndNN <- data.table(membershipNN = mods$membership, 
#                    EndIDYrIter = mods$IDYrIter)

## add membership for NNend
#DT <- merge(DT, EndNN, by = "EndIDYrIter")
#DT[, comm := (membership==membershipNN)]
#DT$comm[DT$comm == "TRUE"] <- "Same"
#DT$comm[DT$comm == "FALSE"] <- "Different"

#DT[, c("EndIDYrIter") := NULL]

saveRDS(fogo2, "output/location-data/5-rdm-locs-sri-NN-N20.RDS")

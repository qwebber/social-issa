

## NN and SRI derived data ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data
# Outputs: combined SRI and NN file

### Packages ----
libs <- c('data.table')
lapply(libs, require, character.only = TRUE)

## Load modularity data 
fogo <- readRDS("output/3-rdm-locs-NN.RDS")

fogo$DyadYrIter1 <- as.factor(paste(fogo$ANIMAL_ID,
                                   fogo$StartNN_ID, 
                                   fogo$iter,
                                   fogo$JDate, sep = "_"))
fogo$DyadYrIter2 <- as.factor(paste(fogo$ANIMAL_ID,
                                    fogo$StartNN_ID, 
                                    fogo$iter,
                                    fogo$JDate, sep = "_"))


sri <- readRDS("output/4-sri.RDS")
sri$DyadYrIter1 <- as.factor(paste(sri$ID1, sri$ID2, sri$Year, sri$iter, sri$JDate, sep = "_"))
sri$DyadYrIter2 <- as.factor(paste(sri$ID2, sri$ID1, sri$Year, sri$iter, sri$JDate, sep = "_"))

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

saveRDS(fogo2, "output/5-rdm-locs-sri-NN.RDS")

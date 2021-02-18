

## Calculate home range overlap of social communites ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data w/ community membership
# Outputs: list of pairwise UDOI values for communities in a given year

#################################
##### GENERATE HOME RANGES ######
################################ 

### Packages ----
libs <- c('data.table', 'ggplot2', 'rgdal', 
          'spatsoc', 'igraph', 'gridExtra', 'adehabitatHR')
lapply(libs, require, character.only = TRUE)


### Input data ----
DT <- readRDS('output/location-data/7-locs-mod.RDS')

DT[, .N, by = c("membership", "lagYear")]


## year by membership
DT$IDmodLagYr <- as.character(paste(as.character(DT$ANIMAL_ID), 
                              as.character(DT$membership), 
                              as.character(DT$lagYear), sep = "_"))

DT$modLagYr <- as.character(paste(as.character(DT$membership), 
                               as.character(DT$lagYear), sep = "_"))

############################################################
###### Calculate Home range area for each individual #######
###########################################################

utm21N <- '+proj=utm +zone=21 ellps=WGS84'

coords <- c('EASTING', 'NORTHING')

### generate ranges for communities
ptsCommFogo <- SpatialPointsDataFrame(DT[, ..coords],
                                  proj4string = CRS(utm21N),
                                  data = DT[, .(IDmodLagYr)])

#### THIS IS FOR THE BHATTACHARYA HOMERANGE OVERLAP ####
#data.xy <- DT[,c('EASTING', 'NORTHING')]
#xysp <- SpatialPoints(data.xy)
#proj4string(xysp) <- CRS("+proj=utm +zone=21 ellps=WGS84")

#Creates a Spatial Data Frame from all locations
#sppt<-data.frame(xysp)

#Creates a spatial data frame of ID
#idsp<-data.frame(DT$IDmodLagYr)
#coordinates(idsp)<-sppt

udCommFogo <- kernelUD(ptsCommFogo, grid = 700, extent = 7)
verticesFogo <- getverticeshr(udCommFogo, 95)
dfFogo <- fortify(verticesFogo)
setDT(dfFogo)[, c("ID", "membership", "Year") := tstrsplit(id, "_", fixed=TRUE)]
saveRDS(dfFogo, "output/vertices/verticesFogo.RDS")

## generate herd-level MCP

ptsCommMCP <- SpatialPointsDataFrame(DT[, ..coords],
                       proj4string = CRS(utm21N),
                       data = DT)

fogoMCP <- mcp(ptsCommMCP, percent = 95)
saveRDS(fogoMCP, "output/vertices/fogo.mcp.RDS")


## Calculate UDOI
KOverFogo <- adehabitatHR::kerneloverlap(ptsCommFogo,
                                     method = "UDOI",
                                     percent = 95,
                                     grid = 700)

## comparison of social communities 
KOverFogo <- as.matrix(KOverFogo)
diag(KOverFogo) <- NA
KOverFogo[lower.tri(KOverFogo)] <- NA
UDOIFogo <- na.omit(melt(KOverFogo))
setDT(UDOIFogo)[, c("ID1", "membership1", "Yr1") := tstrsplit(Var1, "_", fixed=TRUE)][, c("ID2", "membership2", "Yr2") := tstrsplit(Var2, "_", fixed=TRUE)][,c("Var1", "Var2") := NULL]
UDOIFogo <- UDOIFogo[, combo := (ID1==ID2)][combo != "TRUE"] ## remove comparisons of same individuals
UDOIFogo <- UDOIFogo[, combo := (Yr1==Yr2)][combo != "FALSE"] ## remove comparisons across years
UDOIFogo <- UDOIFogo[, memb := (membership1==membership2)][, c("combo") := NULL]
UDOIFogo$memb[UDOIFogo$memb == "TRUE"] <- "Same"
UDOIFogo$memb[UDOIFogo$memb == "FALSE"] <- "Different"

### Output ----
saveRDS(UDOIFogo, 'output/9-community-UDOI-overlap.Rds')


############################################################
###### Calculate Home range area for each community  #######
###########################################################

### generate ranges for communities
ptsCommFogoMod <- SpatialPointsDataFrame(DT[, ..coords],
                                      proj4string = CRS(utm21N),
                                      data = DT[, .(modLagYr)])

udCommFogoMod <- kernelUD(ptsCommFogoMod, grid = 700, extent = 7)
kernel.area(udCommFogoMod, unout=("km2"), 95)
verticesFogoMod <- getverticeshr(udCommFogoMod, 95)
dfFogoMod <- fortify(verticesFogoMod)
setDT(dfFogoMod)[, c("membership", "Year") := tstrsplit(id, "_", fixed=TRUE)]
saveRDS(dfFogoMod, "output/vertices/verticesFogoCommunity.RDS")

## Calculate UDOI
KOverFogo <- adehabitatHR::kerneloverlap(ptsCommFogoMod,
                                         method = "UDOI",
                                         percent = 95,
                                         grid = 700)

## comparison of social communities 
KOverFogo <- as.matrix(KOverFogo)
diag(KOverFogo) <- NA
KOverFogo[lower.tri(KOverFogo)] <- NA
UDOIFogo <- na.omit(melt(KOverFogo))
setDT(UDOIFogo)[, c("membership1", "Yr1") := tstrsplit(Var1, "_", fixed=TRUE)][, c("membership2", "Yr2") := tstrsplit(Var2, "_", fixed=TRUE)][,c("Var1", "Var2") := NULL]
UDOIFogo <- UDOIFogo[, combo := (Yr1==Yr2)][combo != "FALSE"] ## remove comparisons across years

mean(UDOIFogo$value)
sd(UDOIFogo$value)
range(UDOIFogo$value)

UDOIFogo$value <- round(UDOIFogo$value, digits = 3)

UDOIFogo$comm <- paste(UDOIFogo$membership1, UDOIFogo$membership2, sep = "_")

UDOIFogo[, mean(value, na.rm = T), by = "Yr1"]
UDOIFogo[, sd(value, na.rm = T), by = "Yr1"]


### Output ----
fwrite(UDOIFogo, 'output/10-community-UDOI-overlap.csv')



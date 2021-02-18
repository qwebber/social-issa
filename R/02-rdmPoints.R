
## Cleaned Locs - generate random points ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data + NN
# Outputs: 

### Packages ----
libs <- c('data.table', 'ggplot2', 'rgdal', 'spatsoc', 'amt',
          'tidyverse', 'lubridate', 'raster', 'sp')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
DT <- readRDS("output/1-clean-all.RDS")

DT <- DT[!is.na(season)]

DT[, .N, by = "IDYr"]


## order by datetime
DT <- DT[order(DT$datetime),]
DT <- DT[!is.na(datetime)]

## Variables
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
crs = CRS("+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

N <- 10

## read in functions
source("functions/ExtractPoints.R")
source("functions/rand_by.R")

### NOTE: everything needs to run by ID, except random steps
## Easting = x axis = x coord = east to west = longitude
## Northing = y axis = ycoord = north to south = latitude

## Generate random steps by ID
## Default SL distribution is gamma and default TA distribution is vonmises (no need to specify)
r1 <- DT[, rand_by(
  x = EASTING,
  y = NORTHING,
  t = datetime,
  n = N,
  crs = crs
),
by = ANIMAL_ID]

## extract habitat type at end step
r1[, Value := ExtractPoints(matrix(c(x2_, y2_), ncol = 2),
                                          raster = lcFogo)] 

## rename habitat types
r1 <- merge(r1, Legend, by = 'Value')

## check number of fixes by habitat type: 
r1[, .N, by = "habitat"]

##### Landcover Fogo
lcFogo[is.na(lcFogo)] <- 10
WetlandFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Wetland",1,0)))
BroadleafFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Broadleaf",1,0)))
ConiferFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="ConiferForest",1,0)))
ScrubFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="ConiferScrub",1,0)))
MixedWoodFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="MixedWood",1,0)))
RockFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Rocky",1,0)))
WaterFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Water",1,0)))
LichenFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Lichen",1,0)))
AnthroFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Anthro",1,0)))

## combine habitat types
openMoveFogo <- WetlandFogo + RockFogo + WaterFogo + AnthroFogo
ForestFogo <- ConiferFogo + MixedWoodFogo + ScrubFogo + BroadleafFogo
LichenFogo ## Lichen stays the same

### This step makes new raster layers that are "proportion of habitat within a 100 m 
### buffer that is habitat x". Tends to make analyses more robust and less susceptible
### to problems with autocorrelation.-MPL

## Fogo
openMoveBuffFogo <- focalWeight(openMoveFogo, d = 100, type='circle')
ForestBuffFogo <- focalWeight(ForestFogo, d = 100, type='circle')
LichenBuffFogo <- focalWeight(LichenFogo, d = 100, type='circle')

openMoveBuff100Fogo <- focal(openMoveFogo,openMoveBuffFogo,na.rm=TRUE,pad=TRUE,padValue=0)
ForestBuff100Fogo <- focal(ForestFogo,ForestBuffFogo,na.rm=TRUE,pad=TRUE,padValue=0)
LichenBuff100Fogo <- focal(LichenFogo,LichenBuffFogo,na.rm=TRUE,pad=TRUE,padValue=0)

## Proportion of habitat at end point
ptsFogo <- SpatialPoints(data.frame(r1$x2_,r1$y2_))

## extract proportion of each habitat type
r1$propOpenMove <- raster::extract(openMoveBuff100Fogo, ptsFogo)
r1$propForest <- raster::extract(ForestBuff100Fogo,ptsFogo)
r1$propLichen <- raster::extract(LichenBuff100Fogo,ptsFogo)

## assign value to each iteration
r1[, Year := year(t1_)]
r1$IDYr <- paste(r1$ANIMAL_ID, r1$Year, sep = "_")
r1[, iter := 1:(N+1), by = .(IDYr, t2_)]

saveRDS(r1, "output/2-clean-all-rdm.RDS")



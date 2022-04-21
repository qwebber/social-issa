
## Cleaned Locs - generate random points ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data + NN
# Outputs: 

### Packages ----
libs <- c('data.table', 'ggplot2', 'rgdal', 'spatsoc', 'amt',
          'tidyverse', 'lubridate', 'raster', 'sp')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
DT <- readRDS("output/location-data/1-clean-all.RDS")
DT <- DT[season == "winter"]
lcFogo <- raster("../fogo_coyote_repeat/data/raw-data/Landcover/FogoSDSS_RS.tif") # This is a landcover map with different habitat types
Legend <- fread("../fogo_coyote_repeat/data/raw-data/Landcover/Legend.csv") 

## calculate number of fixes per IDYr
mean(DT[, .N, by = "IDYr"]$N)
sd(DT[, .N, by = "IDYr"]$N)
range(DT[, .N, by = "IDYr"]$N)

## order by datetime
DT <- DT[order(DT$datetime),]
DT <- DT[!is.na(datetime)]

## Variables
crs = CRS("+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

N <- 20

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
by = IDYr]

## extract habitat type at end step
r1[, Value := ExtractPoints(matrix(c(x2_, y2_), ncol = 2),
                                          raster = lcFogo)] 

## convert NAs to unavailable habitat
r1$Value[is.na(r1$Value)] <- 10

## rename habitat types
r1 <- merge(r1, Legend, by = 'Value')

## check number of fixes by habitat type: 
r1[, .N, by = "Cover"]

##### Landcover Fogo
#lcFogo[is.na(lcFogo)] <- 10
WetlandFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Wetland",1,0)))
BroadleafFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Broadleaf",1,0)))
ConiferFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="ConiferForest",1,0)))
ScrubFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="ConiferScrub",1,0)))
MixedWoodFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="MixedWood",1,0)))
RockFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Rocky",1,0)))
WaterFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Water",1,0)))
LichenFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Lichen",1,0)))
AnthroFogo <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="Anthro",1,0)))
NotAvail <- subs(lcFogo, data.frame(Legend$Value, ifelse(Legend$Cover=="NotAvail",1,0)))

## combine habitat types
openMoveFogo <- WetlandFogo + RockFogo + WaterFogo + AnthroFogo
ForestFogo <- ConiferFogo + MixedWoodFogo + ScrubFogo + BroadleafFogo
LichenFogo ## Lichen stays the same

### This step makes new raster layers that are "proportion of habitat within a 100 m 
### buffer that is habitat x". Tends to make analyses more robust and less susceptible
### to problems with autocorrelation.-MPL

## 200m buffer
openMoveBuffFogo200 <- focalWeight(openMoveFogo, d = 200, type='circle')
ForestBuffFogo200 <- focalWeight(ForestFogo, d = 200, type='circle')
LichenBuffFogo200 <- focalWeight(LichenFogo, d = 200, type='circle')

openMoveBuff200Fogo <- focal(openMoveFogo,openMoveBuffFogo200,na.rm=TRUE,pad=TRUE,padValue=0)
ForestBuff200Fogo <- focal(ForestFogo,ForestBuffFogo200,na.rm=TRUE,pad=TRUE,padValue=0)
LichenBuff200Fogo <- focal(LichenFogo,LichenBuffFogo200,na.rm=TRUE,pad=TRUE,padValue=0)

## Proportion of habitat at end point
ptsFogo <- SpatialPoints(data.frame(r1$x2_,r1$y2_))

## extract proportion of each habitat type
r1$propOpenMove200 <- raster::extract(openMoveBuff200Fogo, ptsFogo)
r1$propForest200 <- raster::extract(ForestBuff200Fogo,ptsFogo)
r1$propLichen200 <- raster::extract(LichenBuff200Fogo,ptsFogo)


## 1000m buffer
openMoveBuffFogo1000 <- focalWeight(openMoveFogo, d = 1000, type='circle')
ForestBuffFogo1000 <- focalWeight(ForestFogo, d = 1000, type='circle')
LichenBuffFogo1000 <- focalWeight(LichenFogo, d = 1000, type='circle')

openMoveBuff1000Fogo <- focal(openMoveFogo,openMoveBuffFogo1000,na.rm=TRUE,pad=TRUE,padValue=0)
ForestBuff1000Fogo <- focal(ForestFogo,ForestBuffFogo1000,na.rm=TRUE,pad=TRUE,padValue=0)
LichenBuff1000Fogo <- focal(LichenFogo,LichenBuffFogo1000,na.rm=TRUE,pad=TRUE,padValue=0)

## extract proportion of each habitat type
r1$propOpenMove1000 <- raster::extract(openMoveBuff1000Fogo, ptsFogo)
r1$propForest1000 <- raster::extract(ForestBuff1000Fogo,ptsFogo)
r1$propLichen1000 <- raster::extract(LichenBuff1000Fogo,ptsFogo)

## assign value to each iteration
r1[, Year := year(t1_)]
r1[, iter := rep(1:21), by = .(IDYr, t2_)]

ggplot(r1[iter == 1]) +
  geom_point(aes(propOpenMove1000, propOpenMove200)) +
  ylim(0, 1) +
  xlim(0, 1)

saveRDS(r1, "output/location-data/2-clean-all-rdm-N20.RDS")



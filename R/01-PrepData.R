

### All Locs - Cleaning ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Collar data
# Outputs: Prepped data

### Packages ----
libs <- c('data.table', 'ggplot2', 'rgdal', 'lubridate')
lapply(libs, require, character.only = TRUE)

### Set variables ----

# Filter by bounds
lowEastFogo <- 690000; highEastFogo <- 800000
lowNorthFogo <- 5450000; highNorthFogo <- 6000000

# Time zone 
tz <- 'America/St_Johns'

# Max moverate
maxMoveRate <- 30000

### Projection ----
projCols <- c('EASTING', 'NORTHING')

utm21N <- '+proj=utm +zone=21 ellps=WGS84'

### Input raw data ----
fogo <- fread(paste0('input/FogoCaribou.csv'))

# Prep Fogo data to merge 
fogo[, idate := as.IDate(idate)]
fogo[, itime := as.ITime(itime)]
fogo[, datetime := as.POSIXct(paste(idate,itime), format = "%Y-%m-%d %H:%M:%S" )]

### ID by Yr
fogo$IDYr <- paste(fogo$ANIMAL_ID, fogo$Year, sep = "_")

aa <- fogo[, .N, by = .(ANIMAL_ID, Year)]
aa[order(aa$Year),]


## remove  animals with malfunctioning collars
fogo <- fogo[IDYr != "FO2016006_2017" & ## not enough fixes 
             IDYr != "FO2016006_2018" & ## not enough fixes
             IDYr != "FO2017006_2019" & ## not enough fixes
             ANIMAL_ID != "FO2017013" & ## Dead animal
             ANIMAL_ID != "FO2016001" &  ## Indian Island
             ANIMAL_ID != "FO2016011" &  ## Indian Island  
             IDYr !=  "FO2017004_2019" & ## not enough fixes
             IDYr != "FO2017007_2019"] ## not enough fixes

### round FO2016014 datetime to nearest hour to match fixes for rest of individuals
fogo[, hour := hour(as.ITime(itime))]
is.odd <- function(x) x %% 2 != 0 
fogo$hour <- is.odd(fogo$hour)
fogo <- fogo[hour != "TRUE" ][, c("hour") := NULL]
fogo[, datetime := floor_date(datetime, '1 hour')]
fogo[, itime := as.ITime(datetime)]

### Add days from May 1 as "year"
## Year 1
df1 <- fogo[Year == "2016" & JDate >= 85]
df1$JDateMay1 <- df1$JDate - 84
df1$lagYear <- "Year1"
df2 <- fogo[Year == "2017" & JDate < 85]
df2$JDateMay1 <- df2$JDate + 281
df2$lagYear <- "Year1"
## Year 2
df3 <- fogo[Year == "2017" & JDate >= 85]
df3$JDateMay1 <- df3$JDate - 84
df3$lagYear <- "Year2"
df4 <- fogo[Year == "2018" & JDate < 85]
df4$JDateMay1 <- df4$JDate + 281
df4$lagYear <- "Year2"
## Year 2
df5 <- fogo[Year == "2018" & JDate >= 85]
df5$JDateMay1 <- df5$JDate - 84
df5$lagYear <- "Year3"
df5 <- df5[ANIMAL_ID != "FO2016004" & ANIMAL_ID != "FO2016010"]
df6 <- fogo[Year == "2019" & JDate < 85]
df6$JDateMay1 <- df6$JDate + 281
df6$lagYear <- "Year3"

fogo <- rbind(df1, df2, df3, df4, df5, df6)

### ID by Yr
fogo$IDLagYr <- paste(fogo$ANIMAL_ID, fogo$lagYear, sep = "_")


## assign winter season (Jan 1 to March 16)
fogo[JDate >= 1 & JDate <= 75, season := 'winter']

# UTM zone 21N
fogo[, (projCols) := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]
fogo <- fogo[(lowEastFogo < EASTING & EASTING < highEastFogo) &
               (lowNorthFogo < NORTHING & EASTING < highNorthFogo)]

## Export data
saveRDS(fogo, 'output/1-clean-all.Rds')

message('=== PREP COMPLETE ===')




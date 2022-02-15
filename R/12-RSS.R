


### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed', 'raster', 'ggthemes')
lapply(libs, require, character.only = TRUE)

# Load data
DT <- readRDS("output/location-data/5-rdm-locs-sri-NN-N20.RDS")
issa <- readRDS("output/issa models/SRI_issa_20.RDS")

DT[, .N, by = c("IDYr")]

DT$Use[DT$case_ == "TRUE"] <- 1
DT$Use[DT$case_ == "FALSE"] <- 0

## number of groups
length(unique(DT[iter == 1]$groupEnd))

#### RSS functions ####
### POP ###
p.pop <- function(DT, mod, habvar, habvalue, socvar, socvalue){
  #unique(
  DT[
    ,.(hab = predict(
      mod,
      newdata = .SD[, .(
        sl_ = mean(sl_),
        ta_ = mean(ta_),
        propOpenMove = ifelse(habvar == 'open', habvalue, mean(propOpenMove, na.rm = T)),
        propLichen = ifelse(habvar == 'lichen', habvalue, mean(propLichen, na.rm = T)),
        propForest = ifelse(habvar == 'forest', habvalue, mean(propForest, na.rm = T)),
        StartDist = ifelse(socvar == 'StartDist', socvalue, mean(StartDist, na.rm = T)),  
        sri = ifelse(socvar == 'sri', seq(0, 1, length.out = 100), mean(sri, na.rm = T)),
        EndDist = ifelse(socvar == 'EndDist', 1:500, mean(EndDist, na.rm = T)),
        caribou_step_id_ = NA,
        IDYr = NA
      )],
      type = "link",
      re.form = NA
    ), 
    habvar, habvalue, socvar, socvalue)]
  # )
}

### INDIVS ###
p.indiv <- function(ids, DT, mod, habvar, habvalue, socvar, socvalue){
  lapply(ids, function(i) {
    #unique(
    DT[
      ,.(hab = predict(
        mod,
        newdata = .SD[, .(
          sl_ = mean(sl_),
          ta_ = mean(ta_),
          propOpenMove = ifelse(habvar == 'open', habvalue, mean(propOpenMove, na.rm = T)),
          propLichen = ifelse(habvar == 'lichen', habvalue, mean(propLichen, na.rm = T)),
          propForest = ifelse(habvar == 'forest', habvalue, mean(propForest, na.rm = T)),
          StartDist = ifelse(socvar == 'StartDist', socvalue, mean(StartDist, na.rm = T)),  
          sri = if(socvar == 'sri') seq(0, 1, length.out = 100)
          else mean(sri, na.rm = T),
          EndDist = if(socvar == 'EndDist') 1:500 
          else mean(EndDist, na.rm = T),
          caribou_step_id_ = NA,
          IDYr = i
        )],
        type = "link",
        re.form = NULL
      ), 
      IDYr = i, habvar, habvalue, socvar, socvalue)]
    # )
  })
}


#### baseline habitat selection ####
# list caribou
caribouID <- unique(as.character(DT$IDYr))

#############################################   
############### SRI RSS #####################
#############################################   

# habitat 2
hab_pred_s2 <- rbindlist(lapply(caribouID, function(i) {
  #unique(
  DT[
    ,.(h2 = predict(
      issa,
      newdata = .SD[, .(
        sl_ = mean(sl_),
        ta_ = mean(ta_),
        propOpenMove = mean(propOpenMove, na.rm = T),
        propLichen = mean(propLichen, na.rm = T),
        propForest = mean(propForest, na.rm = T),
        StartDist = mean(StartDist, na.rm = T),  
        sri = mean(sri, na.rm = T),
        EndDist = mean(EndDist, na.rm = T),
        caribou_step_id_ = NA,
        IDYr = i
      )],
      type = "link",
      re.form = NULL
    ), 
    IDYr = i)]
  # )
}))

saveRDS(hab_pred_s2, "output/issa models/RSS_hab_pred_s2.RDS")

## lichen
lichen_pred_s1 <- rbindlist(lapply(caribouID, function(i) {
  #unique(
  DT[
    ,.(hab = predict(
      issa,
      newdata = .SD[, .(
        sl_ = mean(sl_),
        ta_ = mean(ta_),
        propOpenMove = mean(propOpenMove, na.rm = T),
        propLichen = 0.75,
        propForest = mean(propForest, na.rm = T),
        StartDist = mean(StartDist, na.rm = T),  
        sri = mean(sri, na.rm = T),
        EndDist = mean(EndDist, na.rm = T),
        caribou_step_id_ = NA,
        IDYr = i
      )],
      type = "link",
      re.form = NULL
    ), 
    IDYr = i)]
  # )
}))

saveRDS(lichen_pred_s1, "output/issa models/lichen_pred_s1.RDS")


lichen_pred_s1 <- readRDS("output/issa models/lichen_pred_s1.RDS")
hab_pred_s2 <- readRDS("output/issa models/RSS_hab_pred_s2.RDS")

## START HERE

## forest
forest_pred_s1 <- rbindlist(lapply(caribouID, function(i) {
  #unique(
  DT[
    ,.(hab = predict(
      issa,
      newdata = .SD[, .(
        sl_ = mean(sl_),
        ta_ = mean(ta_),
        propOpenMove = mean(propOpenMove, na.rm = T),
        propLichen = mean(propLichen, na.rm = T),
        propForest = 0.75,
        StartDist = mean(StartDist, na.rm = T),  
        sri = mean(sri, na.rm = T),
        EndDist = mean(EndDist, na.rm = T),
        caribou_step_id_ = NA,
        IDYr = i
      )],
      type = "link",
      re.form = NULL
    ), 
    IDYr = i)]
  # )
}))


### open
open_pred_s1 <- rbindlist(lapply(caribouID, function(i) {
  #unique(
  DT[
    ,.(hab = predict(
      issa,
      newdata = .SD[, .(
        sl_ = mean(sl_),
        ta_ = mean(ta_),
        propOpenMove = 0.75,
        propLichen = mean(propLichen, na.rm = T),
        propForest = mean(propForest, na.rm = T),
        StartDist = mean(StartDist, na.rm = T),  
        sri = mean(sri, na.rm = T),
        EndDist = mean(EndDist, na.rm = T),
        caribou_step_id_ = NA,
        IDYr = i
      )],
      type = "link",
      re.form = NULL
    ), 
    IDYr = i)]
  # )
}))

## calculate RSS
lichen_rss <- lichen_pred_s1[,.(IDYr, rss = hab-hab_pred_s2$h2)]
forest_rss <- forest_pred_s1[,.(IDYr, rss = hab-hab_pred_s2$h2)]
open_rss <- open_pred_s1[,.(IDYr, rss = hab-hab_pred_s2$h2)]

####  sri RSS by ID ####
lichen_pred_s1_sri <- p.indiv(ids = caribouID, DT, mod = issa, habvar = 'lichen', habvalue = 0.75, socvar = 'sri', socvalue = seq(0, 1, length.out = 100))
forest_pred_s1_sri <- p.indiv(ids = caribouID, DT, mod = issa, habvar = 'forest', habvalue = 0.75, socvar = 'sri', socvalue = seq(0, 1, length.out = 100))
open_pred_s1_sri <- p.indiv(ids = caribouID, DT, mod = issa, habvar = 'open', habvalue = 0.75, socvar = 'sri', socvalue = seq(0, 1, length.out = 100))

lichen_sri_rss <- merge(rbindlist(lichen_pred_s1_sri), lichen_pred_s1[,.(IDYr, h2=hab)], by = 'IDYr')
lichen_sri_rss[,rss:=hab-h2]
setkey(lichen_sri_rss, IDYr)
setnames(lichen_rss, "rss", "rss_hab")
lichen_sri_rss <- merge(lichen_sri_rss, lichen_rss, by = 'IDYr')
lichen_sri_rss[,rss_total:= rss + rss_hab]

forest_sri_rss <- merge(rbindlist(forest_pred_s1_sri), forest_pred_s1[,.(IDYr, h2=hab)], by = 'IDYr')
forest_sri_rss[,rss:=hab-h2]
setkey(forest_sri_rss, IDYr)
setnames(forest_rss, "rss", "rss_hab")
forest_sri_rss <- merge(forest_sri_rss, forest_rss, by = 'IDYr')
forest_sri_rss[,rss_total:= rss + rss_hab]

open_sri_rss <- merge(rbindlist(open_pred_s1_sri), open_pred_s1[,.(IDYr, h2=hab)], by = 'IDYr')
open_sri_rss[,rss:=hab-h2]
setkey(open_sri_rss, IDYr)
setnames(open_rss, "rss", "rss_hab")
open_sri_rss <- merge(open_sri_rss, open_rss, by = 'IDYr')
open_sri_rss[,rss_total:= rss + rss_hab]


#### collect all RSS ####
df_id <- rbind(lichen_sri_rss ,forest_sri_rss, open_sri_rss)
df_id <- df_id[,.(IDYr, habvar, habvalue, socvar, x = socvalue, rss_intx = rss, rss_hab, rss_total)]
saveRDS(df_id, "output/issa models/11-SRI-RSS-ID.RDS")

#############################################   
############### NN RSS #####################
#############################################   

# habitat 2
hab_pred_s2 <- rbindlist(lapply(caribouID, function(i) {
  #unique(
  DT[
    ,.(h2 = predict(
      issa,
      newdata = .SD[, .(
        sl_ = mean(sl_),
        ta_ = mean(ta_),
        propOpenMove = mean(propOpenMove, na.rm = T),
        propLichen = mean(propLichen, na.rm = T),
        propForest = mean(propForest, na.rm = T),
        StartDist = mean(StartDist, na.rm = T),  
        sri = mean(sri, na.rm = T),
        EndDist = mean(EndDist, na.rm = T),
        caribou_step_id_ = NA,
        IDYr = i
      )],
      type = "link",
      re.form = NULL
    ), 
    IDYr = i)]
  # )
}))


## lichen
lichen_pred_s1 <- rbindlist(lapply(caribouID, function(i) {
  #unique(
  DT[
    ,.(hab = predict(
      issa,
      newdata = .SD[, .(
        sl_ = mean(sl_),
        ta_ = mean(ta_),
        propOpenMove = mean(propOpenMove, na.rm = T),
        propLichen = 0.75,
        propForest = mean(propForest, na.rm = T),
        StartDist = mean(StartDist, na.rm = T),  
        sri = mean(sri, na.rm = T),
        EndDist = mean(EndDist, na.rm = T),
        caribou_step_id_ = NA,
        IDYr = i
      )],
      type = "link",
      re.form = NULL
    ), 
    IDYr = i)]
  # )
}))
lichen_rss <- lichen_pred_s1[,.(IDYr, rss = hab-hab_pred_s2$h2)]

## forest
forest_pred_s1 <- rbindlist(lapply(caribouID, function(i) {
  #unique(
  DT[
    ,.(hab = predict(
      issa,
      newdata = .SD[, .(
        sl_ = mean(sl_),
        ta_ = mean(ta_),
        propOpenMove = mean(propOpenMove, na.rm = T),
        propLichen = mean(propLichen, na.rm = T),
        propForest = 0.75,
        StartDist = mean(StartDist, na.rm = T),  
        sri = mean(sri, na.rm = T),
        EndDist = mean(EndDist, na.rm = T),
        caribou_step_id_ = NA,
        IDYr = i
      )],
      type = "link",
      re.form = NULL
    ), 
    IDYr = i)]
  # )
}))
forest_rss <- forest_pred_s1[,.(IDYr, rss = hab-hab_pred_s2$h2)]


### open
open_pred_s1 <- rbindlist(lapply(caribouID, function(i) {
  #unique(
  DT[
    ,.(hab = predict(
      issa,
      newdata = .SD[, .(
        sl_ = mean(sl_),
        ta_ = mean(ta_),
        propOpenMove = 0.75,
        propLichen = mean(propLichen, na.rm = T),
        propForest = mean(propForest, na.rm = T),
        StartDist = mean(StartDist, na.rm = T),  
        sri = mean(sri, na.rm = T),
        EndDist = mean(EndDist, na.rm = T),
        caribou_step_id_ = NA,
        IDYr = i
      )],
      type = "link",
      re.form = NULL
    ), 
    IDYr = i)]
  # )
}))
open_rss <- open_pred_s1[,.(IDYr, rss = hab-hab_pred_s2$h2)]

####  NN RSS by ID ####
lichen_pred_s1_NN <- p.indiv(ids = caribouID, DT, mod = issa, habvar = 'lichen', habvalue = 0.75, socvar = 'EndDist', socvalue = seq(0, 1, length.out = 100))
forest_pred_s1_NN <- p.indiv(ids = caribouID, DT, mod = issa, habvar = 'forest', habvalue = 0.75, socvar = 'EndDist', socvalue = seq(0, 1, length.out = 100))
open_pred_s1_NN <- p.indiv(ids = caribouID, DT, mod = issa, habvar = 'open', habvalue = 0.75, socvar = 'EndDist', socvalue = seq(0, 1, length.out = 100))

lichen_NN_rss <- merge(rbindlist(lichen_pred_s1_NN), lichen_pred_s1[,.(IDYr, h2=hab)], by = 'IDYr')
lichen_NN_rss[,rss:=hab-h2]
setkey(lichen_NN_rss, IDYr)
setnames(lichen_rss, "rss", "rss_hab")
lichen_NN_rss <- merge(lichen_NN_rss, lichen_rss, by = 'IDYr')
lichen_NN_rss[,rss_total:= rss + rss_hab]

forest_NN_rss <- merge(rbindlist(forest_pred_s1_NN), forest_pred_s1[,.(IDYr, h2=hab)], by = 'IDYr')
forest_NN_rss[,rss:=hab-h2]
setkey(forest_NN_rss, IDYr)
setnames(forest_rss, "rss", "rss_hab")
forest_NN_rss <- merge(forest_NN_rss, forest_rss, by = 'IDYr')
forest_NN_rss[,rss_total:= rss + rss_hab]

open_NN_rss <- merge(rbindlist(open_pred_s1_NN), open_pred_s1[,.(IDYr, h2=hab)], by = 'IDYr')
open_NN_rss[,rss:=hab-h2]
setkey(open_NN_rss, IDYr)
setnames(open_rss, "rss", "rss_hab")
open_NN_rss <- merge(open_NN_rss, open_rss, by = 'IDYr')
open_NN_rss[,rss_total:= rss + rss_hab]

#### collect all RSS ####
df_id <- rbind(lichen_NN_rss ,forest_NN_rss, open_NN_rss)
df_id <- df_id[,.(IDYr, habvar, habvalue, socvar, x = socvalue, rss_intx = rss, rss_hab, rss_total)]
saveRDS(df_id, "output/issa models/11-NN-RSS-ID.RDS")



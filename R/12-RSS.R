


### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed', 'raster', 'ggthemes')
lapply(libs, require, character.only = TRUE)

# Load data
DT <- readRDS("output/location-data/5-rdm-locs-sri-NN.RDS")
sri_ssf <- readRDS("output/issa models/3-sri_issa_rdm.RDS")
#NN_ssf <- readRDS("output/issa models/3-NN_issa_rdm.RDS")
summary(sri_ssf)

DT[, .N, by = c("IDYr")]

DT$Use[DT$case_ == "TRUE"] <- 1
DT$Use[DT$case_ == "FALSE"] <- 0

## number of groups
length(unique(DT[iter == 1]$groupEnd))

## open areas as the reference category
#DT$habitat[DT$habitat == "openMove"] = "aOpenMove"

# ## subset to only observed individuals
# DT <- DT[iter == 1]



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
        sri = ifelse(socvar == 'sri', socvalue, mean(sri, na.rm = T)),
        EndDist = ifelse(socvar == 'EndDist', socvalue, mean(EndDist, na.rm = T)), 
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
          sri = ifelse(socvar == 'sri', socvalue, mean(sri, na.rm = T)),
          EndDist = ifelse(socvar == 'EndDist', socvalue, mean(EndDist, na.rm = T)),
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



# habitat 2
hab_pred_s2 <- rbindlist(lapply(caribouID, function(i) {
  #unique(
  DT[
    ,.(h2 = predict(
      sri_ssf,
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
  # rbindlist(p.indiv(ids=caribouID, DT=DT, mod = sri_ssf,
  #                         habvar = NULL, habvalue = NULL,
  #                         socvar = NULL, socvalue = NULL))
## lichen
lichen_pred_s1 <- rbindlist(lapply(caribouID, function(i) {
  #unique(
  DT[
    ,.(hab = predict(
      sri_ssf,
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
lichen_rss <- lichen_pred_s1[,.(IDYr, habvar, habvalue ,rss = hab-hab_pred_s2$h2)]


## forest
forest_pred_s1 <- rbindlist(p.indiv(ids=caribouID, DT=DT, mod = sri_ssf,
                                    habvar = 'forest', habvalue = 0.75,
                                    socvar = NA, socvalue = NA))
forest_rss <- forest_pred_s1[,.(IDYr, habitat,rss = hab-hab_pred_s2$hab)]

## open
open_pred_s1 <- rbindlist(p.indiv(ids=caribouID, DT=DT, mod = sri_ssf,
                                    habvar = 'open', habvalue = 0.75,
                                    socvar = NA, socvalue = NA))
open_rss <- open_pred_s1[,.(IDYr, habitat,rss = hab-hab_pred_s2$hab)]


## forest
forest_pred_s1 <- 
  rbindlist(lapply(caribouID, function(i) {
    #unique(
    DT[
      ,.(hab = predict(
        sri_ssf,
        newdata = .SD[, .(
          sl_ = mean(sl_),
          ta_ = mean(ta_),
          habitat = factor("Forest", levels = c('Forest', 'not')), 
          StartDist = mean(StartDist),  
          sri =  mean(sri),
          EndDist = mean(EndDist), 
          caribou_step_id_ = NA,
          IDYr = i
        )],
        type = "link",
        re.form = NULL
      ), 
      IDYr = i, habitat = 'Forest')]
    # )
  }))

forest_pred_s2 <- 
  rbindlist( lapply(caribouID, function(i) {
    #unique(
    DT[
      ,.(hab = predict(
        sri_ssf,
        newdata = .SD[, .(
          sl_ = mean(sl_),
          ta_ = mean(ta_),
          habitat = factor("Forest", levels = c('aOpenMove', 'Forest','openForage')), 
          StartDist = mean(StartDist),  
          sri =  mean(sri),
          EndDist = mean(EndDist), 
          caribou_step_id_ = NA,
          IDYr = i
        )],
        type = "link",
        re.form = NULL
      ), 
      IDYr = i)]
    # )
  }))

forest_rss <- forest_pred_s1[,.(IDYr, habitat,rss = hab-forest_pred_s2$hab)]


## move
move_pred_s1 <- 
  rbindlist(lapply(caribouID, function(i) {
    #unique(
    DT[
      ,.(hab = predict(
        sri_ssf,
        newdata = .SD[, .(
          sl_ = mean(sl_),
          ta_ = mean(ta_),
          habitat = factor("aOpenMove", levels = c('aOpenMove', 'not')), 
          StartDist = mean(StartDist),  
          sri =  mean(sri),
          EndDist = mean(EndDist), 
          caribou_step_id_ = NA,
          IDYr = i
        )],
        type = "link",
        re.form = NULL
      ), 
      IDYr = i, habitat = 'Open')]
    # )
  }))

move_pred_s2 <- 
  rbindlist( lapply(caribouID, function(i) {
    #unique(
    DT[
      ,.(hab = predict(
        sri_ssf,
        newdata = .SD[, .(
          sl_ = mean(sl_),
          ta_ = mean(ta_),
          habitat = factor("aOpenMove", levels = c('aOpenMove', 'Forest','openForage')), 
          StartDist = mean(StartDist),  
          sri =  mean(sri),
          EndDist = mean(EndDist), 
          caribou_step_id_ = NA,
          IDYr = i
        )],
        type = "link",
        re.form = NULL
      ), 
      IDYr = i)]
    # )
  }))

move_rss <- forest_pred_s1[,.(IDYr, habitat,rss = hab-move_pred_s2$hab)]


####  NN RSS by ID ####
lichen_pred_s1_NN <- p.indiv(ids = caribouID, DT, mod = sri_ssf, var = 'EndDist', value = 1:500, habitat = 'openForage')
#lichen_pred_s1_NN_pop <- p.pop(DT, mod = sri_ssf, var = 'EndDist', value = 1:500, habitat = 'openForage')
forest_pred_s1_NN <- p.indiv(ids = caribouID, DT, mod = sri_ssf, var = 'EndDist', value = 1:500, habitat = 'Forest')
move_pred_s1_NN <- p.indiv(ids = caribouID, DT, mod = sri_ssf, var = 'EndDist', value = 1:500, habitat = 'AOpenMove')

lichen_NN_rss <- merge(rbindlist(lichen_pred_s1_NN), lichen_pred_s2[,.(IDYear, h2=hab)], by = 'IDYear')
lichen_NN_rss[,rss:=hab-h2]
####  NN RSS by ID ####
lichen_pred_s1_NN <- p.indiv(ids = caribouID, DT, mod = sri_ssf, var = 'EndDist', value = 1:500, habitat = 'openForage')

lichen_s1_NN <- DT[,.(sl_ = mean(sl_),
                      ta_ = mean(ta_),
                      habitat = factor('openForage', levels = c('aOpenMove', 'Forest', 'openForage')),
                      StartDist = mean(StartDist),  
                      sri = mean(sri),
                      EndDist = 1:500,
                      step_id_ = NA),
                   by = "IDYr"]

lichen_s2_NN <- DT[,.(sl_ = mean(sl_),
                      ta_ = mean(ta_),
                      habitat = factor('openForage', levels = c('openForage', 'not')),
                      StartDist = mean(StartDist),  
                      sri = mean(sri),
                      EndDist = mean(EndDist),
                      caribou_step_id_ = NA), 
                   by = "IDYr"]


lichen_pred_s1_NN <- p.indiv(ids = caribouID, DT, mod = sri_ssf, var = 'EndDist', value = 1:500, habitat = 'openForage')

lichen_pred_s1_NN <- data.table(h1 = lichen_pred_s1_NN,
                                IDYr = lichen_s1_NN$IDYr)
lichen_pred_s2_NN <- predict(sri_ssf, newdata = lichen_s2_NN, type='link', re.form = NA)
lichen_pred_s2_NN <- data.table(h2 = lichen_pred_s2_NN,
                                IDYr = lichen_s2_NN$IDYr)

## build DT for figures
lichen_pred_s3_NN <- merge(lichen_pred_s1_NN, lichen_pred_s2_NN, by = "IDYr")
lichen_pred_s3_NN$rss <- lichen_pred_s3_NN$h1 - lichen_pred_s3_NN$h2
lichen_pred_s3_NN$env <- rep(1:500, 38)
lichen_pred_s3_NN$category <- "Lichen"

forest_s1_NN <- DT[,.(sl_ = mean(sl_), 
                      ta_ = mean(ta_),
                      habitat = factor('Forest'),
                      StartDist = mean(StartDist),  
                      sri = mean(sri),
                      EndDist = 1:500, 
                      step_id_ = NA),
                   by = "IDYr"]

forest_s2_NN <- DT[,.(sl_ = mean(sl_),
                      ta_ = mean(ta_),
                      habitat = factor('Forest'),
                      StartDist = mean(StartDist),  
                      sri = mean(sri),
                      EndDist = mean(EndDist),
                      step_id_ = NA), 
                   by = "IDYr"]

forest_pred_s1_NN <- predict(sri_ssf, newdata = forest_s1_NN, type='link', re.form = NA)
forest_pred_s1_NN <- data.table(h1 = forest_pred_s1_NN,
                                IDYr = forest_s1_NN$IDYr)
forest_pred_s2_NN <- predict(sri_ssf, newdata = forest_s2_NN, type='link', re.form = NA)
forest_pred_s2_NN <- data.table(h2 = forest_pred_s2_NN,
                                IDYr = forest_s2_NN$IDYr)

## build DT for figures
forest_pred_s3_NN <- merge(forest_pred_s1_NN, forest_pred_s2_NN, by = "IDYr")
forest_pred_s3_NN$rss <- forest_pred_s3_NN$h1 - forest_pred_s3_NN$h2
forest_pred_s3_NN$env <- rep(1:500, 38)
forest_pred_s3_NN$category <- "Forest"

move_s1_NN <- DT[,.(sl_ = mean(sl_), 
                    ta_ = mean(ta_),
                    habitat = factor('aOpenMove'),
                    StartDist = mean(StartDist),  
                    sri = mean(sri),
                    EndDist = 1:500,
                    step_id_ = NA), 
                 by = "IDYr"]

move_s2_NN <- DT[,.(sl_ = mean(sl_),
                    ta_ = mean(ta_),
                    habitat = factor('aOpenMove'),
                    StartDist = mean(StartDist),  
                    sri = mean(sri),
                    EndDist = mean(EndDist),
                    step_id_ = NA), 
                 by = "IDYr"]

move_pred_s1_NN <- predict(sri_ssf, newdata = move_s1_NN, type='link', re.form = NA)
move_pred_s1_NN <- data.table(h1 = move_pred_s1_NN,
                              IDYr = move_s1_NN$IDYr)
move_pred_s2_NN <- predict(sri_ssf, newdata = move_s2_NN, type='link', re.form = NA)
move_pred_s2_NN <- data.table(h2 = move_pred_s2_NN,
                              IDYr = move_s2_NN$IDYr)

## build DT for figures
move_pred_s3_NN <- merge(move_pred_s1_NN, move_pred_s2_NN, by = "IDYr")
move_pred_s3_NN$rss <- move_pred_s3_NN$h1 - move_pred_s3_NN$h2
move_pred_s3_NN$env <- rep(1:500, 38)
move_pred_s3_NN$category <- "Open"

NN_ssf_id <- rbind(lichen_pred_s3_NN, forest_pred_s3_NN, move_pred_s3_NN)

####  NN RSS by Population ####
lichen_s1_pop_NN <- DT[,.(sl_ = mean(sl_), 
                          ta_ = mean(ta_),
                          habitat = factor('openForage'),
                          StartDist = mean(StartDist),  
                          sri = mean(sri),
                          EndDist = 1:500, 
                          step_id_ = NA,
                          IDYr = NA)]

lichen_s2_pop_NN <- DT[,.(sl_ = mean(sl_),
                          ta_ = mean(ta_),
                          habitat = factor('openForage'),
                          StartDist = mean(StartDist),  
                          sri = mean(sri),
                          EndDist = mean(EndDist),
                          step_id_ = NA, 
                          IDYr = NA)]

lichen_pred_s1_pop_NN <- predict(sri_ssf, newdata = lichen_s1_pop_NN, type='link', re.form = NA)
lichen_pred_s2_pop_NN <- predict(sri_ssf, newdata = lichen_s2_pop_NN, type='link', re.form = NA)

forest_s1_pop_NN <- DT[,.(sl_ = mean(sl_),
                          ta_ = mean(ta_),
                          habitat = factor('Forest'),
                          StartDist = mean(StartDist),  
                          sri = mean(sri),
                          EndDist = 1:500, 
                          step_id_ = NA, 
                          IDYr = NA)]

forest_s2_pop_NN <- DT[,.(sl_ = mean(sl_),
                          ta_ = mean(ta_),
                          habitat = factor('Forest'),
                          StartDist = mean(StartDist),  
                          sri = mean(sri),
                          EndDist = mean(EndDist),
                          step_id_ = NA, 
                          IDYr = NA)]

forest_pred_s1_pop_NN <- predict(sri_ssf, newdata = forest_s1_pop_NN, type='link', re.form = NA)
forest_pred_s2_pop_NN <- predict(sri_ssf, newdata = forest_s2_pop_NN, type='link', re.form = NA)

move_s1_pop_NN <- DT[,.(sl_ = mean(sl_),
                        ta_ = mean(ta_),
                        habitat = factor('aOpenMove'),
                        StartDist = mean(StartDist),  
                        sri = mean(sri),
                        EndDist = 1:500, 
                        step_id_ = NA, 
                        IDYr = NA)]

move_s2_pop_NN <- DT[,.(sl_ = mean(sl_),
                        ta_ = mean(ta_),
                        habitat = factor('aOpenMove'),
                        StartDist = mean(StartDist),  
                        sri = mean(sri),
                        EndDist = mean(EndDist),
                        step_id_ = NA, 
                        IDYr = NA)]

move_pred_s1_pop_NN <- predict(sri_ssf, newdata = move_s1_pop_NN, type='link', re.form = NA)
move_pred_s2_pop_NN <- predict(sri_ssf, newdata = move_s2_pop_NN, type='link', re.form = NA)

step_ssf_pop_NN <- data.table(h1 = c(lichen_pred_s1_pop_NN, 
                                     forest_pred_s1_pop_NN, 
                                     move_pred_s1_pop_NN),
                              h2 = c(lichen_pred_s2_pop_NN, 
                                     forest_pred_s2_pop_NN, 
                                     move_pred_s2_pop_NN),
                              rss = c(lichen_pred_s1_pop_NN - lichen_pred_s2_pop_NN,
                                      forest_pred_s1_pop_NN - forest_pred_s2_pop_NN,
                                      move_pred_s1_pop_NN - move_pred_s2_pop_NN),
                              env = c(1:500, 1:500, 1:500),
                              category = c(rep("Lichen", length(lichen_pred_s1_pop_NN)),
                                           rep("Forest", length(forest_pred_s1_pop_NN)),
                                           rep("Open", length(move_pred_s1_pop_NN))),
                              mod = c(rep("NN", 500)))




####  step length RSS by ID ####
lichen_s1 <- DT[,.(sl_ = 1:500,
                   ta_ = mean(ta_),
                   habitat = factor('openForage'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist), 
                   step_id_ = NA),
                by = "IDYr"]

lichen_s2 <- DT[,.(sl_ = mean(sl_),
                   ta_ = mean(ta_),
                   habitat = factor('openForage'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist),
                   step_id_ = NA), 
                by = "IDYr"]

lichen_pred_s1 <- predict(sri_ssf, newdata = lichen_s1, type='link', re.form = NA)
lichen_pred_s1 <- data.table(h1 = lichen_pred_s1,
                             IDYr = lichen_s1$IDYr)
lichen_pred_s2 <- predict(sri_ssf, newdata = lichen_s2, type='link', re.form = NA)
lichen_pred_s2 <- data.table(h2 = lichen_pred_s2,
                             IDYr = lichen_s2$IDYr)

## build DT for figures
lichen_pred_s3 <- merge(lichen_pred_s1, lichen_pred_s2, by = "IDYr")
lichen_pred_s3$rss <- lichen_pred_s3$h1 - lichen_pred_s3$h2
lichen_pred_s3$env <- rep(1:500, 38)
lichen_pred_s3$category <- "Lichen"

forest_s1 <- DT[,.(sl_ = 1:500,
                   ta_ = mean(ta_),
                   habitat = factor('Forest'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist), 
                   step_id_ = NA), 
                   #IDYr = NA),
                by = "IDYr"]
forest_s2 <- DT[,.(sl_ = mean(sl_),
                   ta_ = mean(ta_),
                   habitat = factor('Forest'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist),
                   step_id_ = NA), 
                   #IDYr = NA),
                by = "IDYr"]

forest_pred_s1 <- predict(sri_ssf, newdata = forest_s1, type='link', re.form = NA)
forest_pred_s1 <- data.table(h1 = forest_pred_s1,
                           IDYr = forest_s1$IDYr)
forest_pred_s2 <- predict(sri_ssf, newdata = forest_s2, type='link', re.form = NA)
forest_pred_s2 <- data.table(h2 = forest_pred_s2,
                           IDYr = forest_s2$IDYr)

## build DT for figures
forest_pred_s3 <- merge(forest_pred_s1, forest_pred_s2, by = "IDYr")
forest_pred_s3$rss <- forest_pred_s3$h1 - forest_pred_s3$h2
forest_pred_s3$env <- rep(1:500, 38)
forest_pred_s3$category <- "Forest"


move_s1 <- DT[,.(sl_ = 1:500,
                 ta_ = mean(ta_),
                 habitat = factor('aOpenMove'),
                 StartDist = mean(StartDist),  
                 sri = mean(sri),
                 EndDist = mean(EndDist),
                 step_id_ = NA), 
              by = "IDYr"]

move_s2 <- DT[,.(sl_ = mean(sl_),
                 ta_ = mean(ta_),
                 habitat = factor('aOpenMove'),
                 StartDist = mean(StartDist),  
                 sri = mean(sri),
                 EndDist = mean(EndDist),
                 step_id_ = NA), 
              by = "IDYr"]

move_pred_s1 <- predict(sri_ssf, newdata = move_s1, type='link', re.form = NA)
move_pred_s1 <- data.table(h1 = move_pred_s1,
                           IDYr = move_s1$IDYr)
move_pred_s2 <- predict(sri_ssf, newdata = move_s2, type='link', re.form = NA)
move_pred_s2 <- data.table(h2 = move_pred_s2,
                           IDYr = move_s2$IDYr)

## build DT for figures
move_pred_s3 <- merge(move_pred_s1, move_pred_s2, by = "IDYr")
move_pred_s3$rss <- move_pred_s3$h1 - move_pred_s3$h2
move_pred_s3$env <- rep(1:500, 38)
move_pred_s3$category <- "Open"

step_ssf_id <- rbind(lichen_pred_s3, forest_pred_s3, move_pred_s3)

####  step length RSS by Population ####
lichen_s1_pop <- DT[,.(sl_ = 1:500,
                   ta_ = mean(ta_),
                   habitat = factor('openForage'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist), 
                   step_id_ = NA,
                   IDYr = NA)]

lichen_s2_pop <- DT[,.(sl_ = mean(sl_),
                   ta_ = mean(ta_),
                   habitat = factor('openForage'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist),
                   step_id_ = NA, 
                   IDYr = NA)]

lichen_pred_s1_pop <- predict(sri_ssf, newdata = lichen_s1_pop, type='link', re.form = NA)
lichen_pred_s2_pop <- predict(sri_ssf, newdata = lichen_s2_pop, type='link', re.form = NA)

forest_s1_pop <- DT[,.(sl_ = 1:500,
                   ta_ = mean(ta_),
                   habitat = factor('Forest'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist), 
                   step_id_ = NA, 
                   IDYr = NA)]
forest_s2_pop <- DT[,.(sl_ = mean(sl_),
                   ta_ = mean(ta_),
                   habitat = factor('Forest'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist),
                   step_id_ = NA, 
                   IDYr = NA)]

forest_pred_s1_pop <- predict(sri_ssf, newdata = forest_s1_pop, type='link', re.form = NA)
forest_pred_s2_pop <- predict(sri_ssf, newdata = forest_s2_pop, type='link', re.form = NA)

move_s1_pop <- DT[,.(sl_ = 1:500,
                 ta_ = mean(ta_),
                 habitat = factor('aOpenMove'),
                 StartDist = mean(StartDist),  
                 sri = mean(sri),
                 EndDist = mean(EndDist),
                 step_id_ = NA, 
                 IDYr = NA)]

move_s2_pop <- DT[,.(sl_ = mean(sl_),
                 ta_ = mean(ta_),
                 habitat = factor('aOpenMove'),
                 StartDist = mean(StartDist),  
                 sri = mean(sri),
                 EndDist = mean(EndDist),
                 step_id_ = NA, 
                 IDYr = NA)]

move_pred_s1_pop <- predict(sri_ssf, newdata = move_s1_pop, type='link', re.form = NA)
move_pred_s2_pop <- predict(sri_ssf, newdata = move_s2_pop, type='link', re.form = NA)

step_ssf_pop <- data.table(h1 = c(lichen_pred_s1_pop, 
                            forest_pred_s1_pop, 
                            move_pred_s1_pop),
                     h2 = c(lichen_pred_s2_pop, 
                            forest_pred_s2_pop, 
                            move_pred_s2_pop),
                     rss = c(lichen_pred_s1_pop - lichen_pred_s2_pop,
                             forest_pred_s1_pop - forest_pred_s2_pop,
                             move_pred_s1_pop - move_pred_s2_pop),
                     env = c(1:500, 1:500, 1:500),
                     category = c(rep("Lichen", length(lichen_pred_s1_pop)),
                                  rep("Forest", length(forest_pred_s1_pop)),
                                  rep("Open", length(move_pred_s1_pop))),
                     mod = c(rep("Step", 500)))



####  NN RSS by ID ####
lichen_s1_NN <- DT[,.(sl_ = mean(sl_),
                   ta_ = mean(ta_),
                   habitat = factor('openForage', levels = c('aOpenMove', 'Forest', 'openForage')),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = 1:500,
                   step_id_ = NA),
                by = "IDYr"]

lichen_s2_NN <- DT[,.(sl_ = mean(sl_),
                   ta_ = mean(ta_),
                   habitat = factor('openForage', levels = c('openForage', 'not')),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist),
                   step_id_ = NA), 
                by = "IDYr"]

lichen_pred_s1_NN <- predict(sri_ssf, newdata = lichen_s1_NN, type='link', re.form = NA)
lichen_pred_s1_NN <- data.table(h1 = lichen_pred_s1_NN,
                             IDYr = lichen_s1_NN$IDYr)
lichen_pred_s2_NN <- predict(sri_ssf, newdata = lichen_s2_NN, type='link', re.form = NA)
lichen_pred_s2_NN <- data.table(h2 = lichen_pred_s2_NN,
                             IDYr = lichen_s2_NN$IDYr)

## build DT for figures
lichen_pred_s3_NN <- merge(lichen_pred_s1_NN, lichen_pred_s2_NN, by = "IDYr")
lichen_pred_s3_NN$rss <- lichen_pred_s3_NN$h1 - lichen_pred_s3_NN$h2
lichen_pred_s3_NN$env <- rep(1:500, 38)
lichen_pred_s3_NN$category <- "Lichen"

forest_s1_NN <- DT[,.(sl_ = mean(sl_), 
                   ta_ = mean(ta_),
                   habitat = factor('Forest'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = 1:500, 
                   step_id_ = NA),
                by = "IDYr"]

forest_s2_NN <- DT[,.(sl_ = mean(sl_),
                   ta_ = mean(ta_),
                   habitat = factor('Forest'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist),
                   step_id_ = NA), 
                by = "IDYr"]

forest_pred_s1_NN <- predict(sri_ssf, newdata = forest_s1_NN, type='link', re.form = NA)
forest_pred_s1_NN <- data.table(h1 = forest_pred_s1_NN,
                             IDYr = forest_s1_NN$IDYr)
forest_pred_s2_NN <- predict(sri_ssf, newdata = forest_s2_NN, type='link', re.form = NA)
forest_pred_s2_NN <- data.table(h2 = forest_pred_s2_NN,
                             IDYr = forest_s2_NN$IDYr)

## build DT for figures
forest_pred_s3_NN <- merge(forest_pred_s1_NN, forest_pred_s2_NN, by = "IDYr")
forest_pred_s3_NN$rss <- forest_pred_s3_NN$h1 - forest_pred_s3_NN$h2
forest_pred_s3_NN$env <- rep(1:500, 38)
forest_pred_s3_NN$category <- "Forest"

move_s1_NN <- DT[,.(sl_ = mean(sl_), 
                 ta_ = mean(ta_),
                 habitat = factor('aOpenMove'),
                 StartDist = mean(StartDist),  
                 sri = mean(sri),
                 EndDist = 1:500,
                 step_id_ = NA), 
              by = "IDYr"]

move_s2_NN <- DT[,.(sl_ = mean(sl_),
                 ta_ = mean(ta_),
                 habitat = factor('aOpenMove'),
                 StartDist = mean(StartDist),  
                 sri = mean(sri),
                 EndDist = mean(EndDist),
                 step_id_ = NA), 
              by = "IDYr"]

move_pred_s1_NN <- predict(sri_ssf, newdata = move_s1_NN, type='link', re.form = NA)
move_pred_s1_NN <- data.table(h1 = move_pred_s1_NN,
                           IDYr = move_s1_NN$IDYr)
move_pred_s2_NN <- predict(sri_ssf, newdata = move_s2_NN, type='link', re.form = NA)
move_pred_s2_NN <- data.table(h2 = move_pred_s2_NN,
                           IDYr = move_s2_NN$IDYr)

## build DT for figures
move_pred_s3_NN <- merge(move_pred_s1_NN, move_pred_s2_NN, by = "IDYr")
move_pred_s3_NN$rss <- move_pred_s3_NN$h1 - move_pred_s3_NN$h2
move_pred_s3_NN$env <- rep(1:500, 38)
move_pred_s3_NN$category <- "Open"

NN_ssf_id <- rbind(lichen_pred_s3_NN, forest_pred_s3_NN, move_pred_s3_NN)

####  NN RSS by Population ####
lichen_s1_pop_NN <- DT[,.(sl_ = mean(sl_), 
                       ta_ = mean(ta_),
                       habitat = factor('openForage'),
                       StartDist = mean(StartDist),  
                       sri = mean(sri),
                       EndDist = 1:500, 
                       step_id_ = NA,
                       IDYr = NA)]

lichen_s2_pop_NN <- DT[,.(sl_ = mean(sl_),
                       ta_ = mean(ta_),
                       habitat = factor('openForage'),
                       StartDist = mean(StartDist),  
                       sri = mean(sri),
                       EndDist = mean(EndDist),
                       step_id_ = NA, 
                       IDYr = NA)]

lichen_pred_s1_pop_NN <- predict(sri_ssf, newdata = lichen_s1_pop_NN, type='link', re.form = NA)
lichen_pred_s2_pop_NN <- predict(sri_ssf, newdata = lichen_s2_pop_NN, type='link', re.form = NA)

forest_s1_pop_NN <- DT[,.(sl_ = mean(sl_),
                       ta_ = mean(ta_),
                       habitat = factor('Forest'),
                       StartDist = mean(StartDist),  
                       sri = mean(sri),
                       EndDist = 1:500, 
                       step_id_ = NA, 
                       IDYr = NA)]

forest_s2_pop_NN <- DT[,.(sl_ = mean(sl_),
                       ta_ = mean(ta_),
                       habitat = factor('Forest'),
                       StartDist = mean(StartDist),  
                       sri = mean(sri),
                       EndDist = mean(EndDist),
                       step_id_ = NA, 
                       IDYr = NA)]

forest_pred_s1_pop_NN <- predict(sri_ssf, newdata = forest_s1_pop_NN, type='link', re.form = NA)
forest_pred_s2_pop_NN <- predict(sri_ssf, newdata = forest_s2_pop_NN, type='link', re.form = NA)

move_s1_pop_NN <- DT[,.(sl_ = mean(sl_),
                     ta_ = mean(ta_),
                     habitat = factor('aOpenMove'),
                     StartDist = mean(StartDist),  
                     sri = mean(sri),
                     EndDist = 1:500, 
                     step_id_ = NA, 
                     IDYr = NA)]

move_s2_pop_NN <- DT[,.(sl_ = mean(sl_),
                     ta_ = mean(ta_),
                     habitat = factor('aOpenMove'),
                     StartDist = mean(StartDist),  
                     sri = mean(sri),
                     EndDist = mean(EndDist),
                     step_id_ = NA, 
                     IDYr = NA)]

move_pred_s1_pop_NN <- predict(sri_ssf, newdata = move_s1_pop_NN, type='link', re.form = NA)
move_pred_s2_pop_NN <- predict(sri_ssf, newdata = move_s2_pop_NN, type='link', re.form = NA)

step_ssf_pop_NN <- data.table(h1 = c(lichen_pred_s1_pop_NN, 
                                  forest_pred_s1_pop_NN, 
                                  move_pred_s1_pop_NN),
                           h2 = c(lichen_pred_s2_pop_NN, 
                                  forest_pred_s2_pop_NN, 
                                  move_pred_s2_pop_NN),
                           rss = c(lichen_pred_s1_pop_NN - lichen_pred_s2_pop_NN,
                                   forest_pred_s1_pop_NN - forest_pred_s2_pop_NN,
                                   move_pred_s1_pop_NN - move_pred_s2_pop_NN),
                           env = c(1:500, 1:500, 1:500),
                           category = c(rep("Lichen", length(lichen_pred_s1_pop_NN)),
                                        rep("Forest", length(forest_pred_s1_pop_NN)),
                                        rep("Open", length(move_pred_s1_pop_NN))),
                           mod = c(rep("NN", 500)))


#### SRI RSS #####

####  sri RSS by ID ####
lichen_s1_sri <- DT[,.(sl_ = mean(sl_),
                      ta_ = mean(ta_),
                      habitat = factor('openForage'),
                      StartDist = mean(StartDist),  
                      sri = seq(0, 1, length.out = 100), 
                      EndDist = mean(EndDist),
                      step_id_ = NA),
                   by = "IDYr"]

lichen_s2_sri <- DT[,.(sl_ = mean(sl_),
                      ta_ = mean(ta_),
                      habitat = factor('openForage'),
                      StartDist = mean(StartDist),  
                      sri = mean(sri),
                      EndDist = mean(EndDist),
                      step_id_ = NA), 
                   by = "IDYr"]

lichen_pred_s1_sri <- predict(sri_ssf, newdata = lichen_s1_sri, type='link', re.form = NA)
lichen_pred_s1_sri <- data.table(h1 = lichen_pred_s1_sri,
                                IDYr = lichen_s1_sri$IDYr)
lichen_pred_s2_sri <- predict(sri_ssf, newdata = lichen_s2_sri, type='link', re.form = NA)
lichen_pred_s2_sri <- data.table(h2 = lichen_pred_s2_sri,
                                IDYr = lichen_s2_sri$IDYr)

## build DT for figures
lichen_pred_s3_sri <- merge(lichen_pred_s1_sri, lichen_pred_s2_sri, by = "IDYr")
lichen_pred_s3_sri$rss <- lichen_pred_s3_sri$h1 - lichen_pred_s3_sri$h2
lichen_pred_s3_sri$env <- rep(seq(0, 1, length.out = 100), 38)
lichen_pred_s3_sri$category <- "Lichen"

forest_s1_sri <- DT[,.(sl_ = mean(sl_),
                      ta_ = mean(ta_),
                      habitat = factor('Forest'),
                      StartDist = mean(StartDist),  
                      sri = seq(0, 1, length.out = 100), 
                      EndDist = mean(EndDist),
                      step_id_ = NA),
                   by = "IDYr"]

forest_s2_sri <- DT[,.(sl_ = mean(sl_),
                      ta_ = mean(ta_),
                      habitat = factor('Forest'),
                      StartDist = mean(StartDist),  
                      sri = mean(sri),
                      EndDist = mean(EndDist),
                      step_id_ = NA), 
                   by = "IDYr"]

forest_pred_s1_sri <- predict(sri_ssf, newdata = forest_s1_sri, type='link', re.form = NA)
forest_pred_s1_sri <- data.table(h1 = forest_pred_s1_sri,
                                IDYr = forest_s1_sri$IDYr)
forest_pred_s2_sri <- predict(sri_ssf, newdata = forest_s2_sri, type='link', re.form = NA)
forest_pred_s2_sri <- data.table(h2 = forest_pred_s2_sri,
                                IDYr = forest_s2_sri$IDYr)

## build DT for figures
forest_pred_s3_sri <- merge(forest_pred_s1_sri, forest_pred_s2_sri, by = "IDYr")
forest_pred_s3_sri$rss <- forest_pred_s3_sri$h1 - forest_pred_s3_sri$h2
forest_pred_s3_sri$env <- rep(seq(0, 1, length.out = 100), 38)
forest_pred_s3_sri$category <- "Forest"


move_s1_sri <- DT[,.(sl_ = mean(sl_), 
                    ta_ = mean(ta_),
                    habitat = factor('aOpenMove'),
                    StartDist = mean(StartDist),  
                    sri = seq(0, 1, length.out = 100), 
                    EndDist = mean(EndDist),
                    step_id_ = NA), 
                 by = "IDYr"]

move_s2_sri <- DT[,.(sl_ = mean(sl_),
                    ta_ = mean(ta_),
                    habitat = factor('aOpenMove'),
                    StartDist = mean(StartDist),  
                    sri = mean(sri),
                    EndDist = mean(EndDist),
                    step_id_ = NA), 
                 by = "IDYr"]

move_pred_s1_sri <- predict(sri_ssf, newdata = move_s1_sri, type='link', re.form = NA)
move_pred_s1_sri <- data.table(h1 = move_pred_s1_sri,
                              IDYr = move_s1_sri$IDYr)
move_pred_s2_sri <- predict(sri_ssf, newdata = move_s2_sri, type='link', re.form = NA)
move_pred_s2_sri <- data.table(h2 = move_pred_s2_sri,
                              IDYr = move_s2_sri$IDYr)

## build DT for figures
move_pred_s3_sri <- merge(move_pred_s1_sri, move_pred_s2_sri, by = "IDYr")
move_pred_s3_sri$rss <- move_pred_s3_sri$h1 - move_pred_s3_sri$h2
move_pred_s3_sri$env <- rep(seq(0, 1, length.out = 100), 38)
move_pred_s3_sri$category <- "Open"

sri_ssf_id <- rbind(lichen_pred_s3_sri, forest_pred_s3_sri, move_pred_s3_sri)

####  step length RSS by Population ####
lichen_s1_pop_sri <- DT[,.(sl_ = mean(sl_), 
                          ta_ = mean(ta_),
                          habitat = factor('openForage'),
                          StartDist = mean(StartDist),  
                          sri = seq(0, 1, length.out = 100), 
                          EndDist = mean(EndDist),
                          step_id_ = NA,
                          IDYr = NA)]

lichen_s2_pop_sri <- DT[,.(sl_ = mean(sl_),
                          ta_ = mean(ta_),
                          habitat = factor('openForage'),
                          StartDist = mean(StartDist),  
                          sri = mean(sri), 
                          EndDist = mean(EndDist),
                          step_id_ = NA, 
                          IDYr = NA)]

lichen_pred_s1_pop_sri <- predict(sri_ssf, newdata = lichen_s1_pop_sri, type='link', re.form = NA)
lichen_pred_s2_pop_sri <- predict(sri_ssf, newdata = lichen_s2_pop_sri, type='link', re.form = NA)

forest_s1_pop_sri <- DT[,.(sl_ = mean(sl_),
                          ta_ = mean(ta_),
                          habitat = factor('Forest'),
                          StartDist = mean(StartDist),  
                          sri = seq(0, 1, length.out = 100), 
                          EndDist = mean(EndDist),
                          step_id_ = NA, 
                          IDYr = NA)]

forest_s2_pop_sri <- DT[,.(sl_ = mean(sl_),
                          ta_ = mean(ta_),
                          habitat = factor('Forest'),
                          StartDist = mean(StartDist),  
                          sri = mean(sri),
                          EndDist = mean(EndDist),
                          step_id_ = NA, 
                          IDYr = NA)]

forest_pred_s1_pop_sri <- predict(sri_ssf, newdata = forest_s1_pop_sri, type='link', re.form = NA)
forest_pred_s2_pop_sri <- predict(sri_ssf, newdata = forest_s2_pop_sri, type='link', re.form = NA)

move_s1_pop_sri <- DT[,.(sl_ = mean(sl_),
                        ta_ = mean(ta_),
                        habitat = factor('aOpenMove'),
                        StartDist = mean(StartDist),  
                        sri = seq(0, 1, length.out = 100),
                        EndDist = mean(EndDist),
                        step_id_ = NA, 
                        IDYr = NA)]

move_s2_pop_sri <- DT[,.(sl_ = mean(sl_),
                        ta_ = mean(ta_),
                        habitat = factor('aOpenMove'),
                        StartDist = mean(StartDist),  
                        sri = mean(sri),
                        EndDist = mean(EndDist),
                        step_id_ = NA, 
                        IDYr = NA)]

move_pred_s1_pop_sri <- predict(sri_ssf, newdata = move_s1_pop_sri, type='link', re.form = NA)
move_pred_s2_pop_sri <- predict(sri_ssf, newdata = move_s2_pop_sri, type='link', re.form = NA)

step_ssf_pop_sri <- data.table(h1 = c(lichen_pred_s1_pop_sri, 
                                     forest_pred_s1_pop_sri, 
                                     move_pred_s1_pop_sri),
                              h2 = c(lichen_pred_s2_pop_sri, 
                                     forest_pred_s2_pop_sri, 
                                     move_pred_s2_pop_sri),
                              rss = c(lichen_pred_s1_pop_sri - lichen_pred_s2_pop_sri,
                                      forest_pred_s1_pop_sri - forest_pred_s2_pop_sri,
                                      move_pred_s1_pop_sri - move_pred_s2_pop_sri),
                              env = c(seq(0, 1, length.out = 100), 
                                      seq(0, 1, length.out = 100), 
                                      seq(0, 1, length.out = 100)),
                              category = c(rep("Lichen", length(lichen_pred_s1_pop_sri)),
                                           rep("Forest", length(forest_pred_s1_pop_sri)),
                                           rep("Open", length(move_pred_s1_pop_sri))),
                              mod = c(rep("SRI", 300)))

df_id <- rbind(step_ssf_id ,NN_ssf_id, sri_ssf_id)
saveRDS(df_id, "output/11-RSS-ID.RDS")
df_pop <- rbind(step_ssf_pop ,step_ssf_pop_NN, step_ssf_pop_sri)
saveRDS(df_pop, "output/11-RSS-POP.RDS")

png("graphics/FigS4.1.png", width = 6000, height = 6000, units = "px", res = 600)
aa <- ggplot() +
  geom_line(data = step_ssf_id, 
            aes(env, rss, 
                group = IDYr, 
                color = category), 
            lty = 1, 
            lwd = 0.1) +
  geom_line(data = step_ssf_pop, 
            aes(env,rss, 
                color = category), 
            lty = 1, 
            lwd = 1) +
  scale_color_manual(values = c("#f1a340", "#91bfdb", "#5ab4ac")) +
  ylab("log(relative selection strength)") + 
  xlab("Speed (m/hr)") +
  #ggtitle("A)") +
  geom_hline(yintercept = 0, lty = 2) +
  theme(legend.position = 'none',
        strip.background = element_rect(color = "black", 
                                        fill = "white", 
                                        size = 1),
        strip.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  facet_wrap(~category)
  
bb <- ggplot() +
  geom_line(data = NN_ssf_id, 
            aes(env, rss, 
                group = IDYr, 
                color = category), 
            lty = 1, 
            lwd = 0.1) +
  geom_line(data = step_ssf_pop_NN, 
            aes(env,rss, 
                color = category), 
            lty = 1, 
            lwd = 1) +
  scale_color_manual(values = c("#f1a340", "#91bfdb", "#5ab4ac")) +
  ylab("log(relative selection strength)") +
  xlab("Nearest Neighbour distance (m)") +
  #ggtitle("B)") +
  geom_hline(yintercept = 0, lty = 2) +
  theme(legend.position = 'none',
        strip.background = element_rect(color = "black", 
                                        fill = "white", 
                                        size = 1),
        strip.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))+
  facet_wrap(~category)

cc <- ggplot() +
  geom_line(data = sri_ssf_id, 
            aes(env, rss, 
                group = IDYr, 
                color = category), 
            lty = 1, 
            lwd = 0.1) +
  geom_line(data = step_ssf_pop_sri, 
            aes(env, rss, 
                color = category), 
            lty = 1, 
            lwd = 1) +
  scale_color_manual(values = c("#f1a340", "#91bfdb", "#5ab4ac")) +
  ylab("log(relative selection strength)") +
  xlab("Simple ratio index") +
  #ggtitle('C)') +
  geom_hline(yintercept = 0, lty = 2) +
  theme(legend.position = 'none',
        strip.background = element_rect(color = "black", 
                                        fill = "white", 
                                        size = 1),
        strip.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  facet_wrap(~category)
  
gridExtra::grid.arrange(aa, bb, cc, nrow = 3)
dev.off()

ggplot() +
  geom_line(data = df_pop, 
            aes(env, rss, 
                color = category), 
            lty = 1, 
            lwd = 1) +
  scale_color_manual(values = c("#f1a340", "#91bfdb", "#5ab4ac")) +
  ylab("log(relative selection strength)") +
  #xlab("Simple ratio index") +
  #ggtitle('C)') +
  geom_hline(yintercept = 0, lty = 2) +
  theme(#legend.position = 'none',
        strip.background = element_rect(color = "black", 
                                        fill = "white", 
                                        size = 1),
        strip.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  facet_wrap(~mod, scale = "free")

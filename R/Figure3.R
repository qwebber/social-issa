


### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed', 'raster', 'ggthemes')
lapply(libs, require, character.only = TRUE)

# Load data
DT <- readRDS("output/location-data/5-rdm-locs-sri-NN-N20.RDS")
sri_ssf <- readRDS("output/issa models/SRI_issa_20.RDS")

DT[, .N, by = c("IDYr")]

DT$Use[DT$case_ == "TRUE"] <- 1
DT$Use[DT$case_ == "FALSE"] <- 0

#### movement parameters ####
SLdistr <- function(x.col, y.col, date.col, crs, ID, NumbRandSteps, sl_distr, ta_distr) {
  print(ID)
  #create track from dataset
  trk <- track(x.col, y.col, date.col, ID, crs) %>%
    #function turns locs into steps
    steps()
  #remove any steps that span more than 2hr
  trk$dt_ <- difftime(trk$t2_, trk$t1_, unit='hours')
  trk <- subset(trk, trk$dt_ > 1.9 & trk$dt_ < 2.1, drop = T)
  #generate random steps
  trk %>%
    random_steps() %>%
    sl_distr_params()
}

#TAdistr <- function(x.col, y.col, date.col, crs, ID, NumbRandSteps, sl_distr, ta_distr) {
#  print(ID)
  #create track from dataset
 # trk <- track(x.col, y.col, date.col, ID, crs) %>%
    #function turns locs into steps
  #  steps()
  #remove any steps that span more than 2hr
  #trk$dt_ <- difftime(trk$t2_, trk$t1_, unit='hours')
  #trk <- subset(trk, trk$dt_ > 1.9 & trk$dt_ < 2.1, drop = T)
  #generate random steps
  #trk %>%
  #  random_steps() %>%
  #  ta_distr_params()
#}

utm21N <- '+proj=utm +zone=21 ellps=WGS84'

slParams <- DT[, SLdistr(x.col = x1_, 
                         y.col = y1_, 
                         date.col = t1_, 
                         crs = utm21N, 
                         ID = IDYr,
                         sl_distr = "gamma", 
                         ta_distr = "vonmises"),
                    by = IDYr]

Params <- slParams#merge(slParams, taParams[,.(IDYr,kappa)], by = 'IDYr')

###for each wolf get the estimates you want and then go through models
# Make a vector of all unique wolf IDs 

caribou_IDs <- unique(DT$IDYr) 

fix_vals <- setDT(tidy(sri_ssf, effect = "fixed"))
ran_vals <- tidy(sri_ssf, effect= 'ran_vals')
indiv.se <-setDT(ran_vals)[group=='IDYr']
setnames(indiv.se, "level", "IDYr")

n <- length(caribou_IDs)

## Calculate speed for Lichen
move_lichen <- NULL
for(i in 1:n){
  
  k = as.character(caribou_IDs[i])
  
  move_covs_dat01 = indiv.se[IDYr == k]
  move_pars_dat01 = Params[IDYr == k]
  
  prop = seq(0,1, by = 0.01)
  
  shape = move_pars_dat01$shape
  scale = move_pars_dat01$scale
  
  covi = move_covs_dat01[term == "I(log(sl_ + 1))"]
  Bi = covi$estimate
  covij = fix_vals[term == "I(log(sl_ + 1)):propLichen"]
  Bij = covij$estimate
  
  SLForest <- fix_vals[term == "I(log(sl_ + 1)):propForest"]$estimate 
  medForest <- median(DT$propForest, na.rm = T)
  
  SLMove <- fix_vals[term == "I(log(sl_ + 1)):propOpenMove"]$estimate 
  medOpen <- median(DT$propOpenMove, na.rm = T)
  
  #SLNN <- move_covs_dat01[term == "I(log(StartDist + 1)):I(log(sl_ + 1))"]$estimate 
  #medNN <- log(median(DT$StartDist + 1, na.rm = T))
  
  SLSRI <- move_covs_dat01[term == "I(log(sl_ + 1)):I(log(sri + 0.125))"]$estimate 
  medSRI <- log(median(DT$sri + 0.125, na.rm = T))
  
  speed = (shape + Bi + 
             (Bij*prop) + 
             (SLForest * medForest) +
             (SLMove * medOpen) +
             #(SLNN * medNN) +
             (SLSRI * medSRI)
             
           )*(scale)
  
  move_lichen[[i]] = data.table(IDYr = k, 
                          Habitat = "Lichen",
                          Bi = Bi, 
                          Bij = Bij, 
                          Proportion = prop, 
                          Speed = speed) 
  
  
}
move_lichen <- rbindlist(move_lichen)

## Calculate speed for Forest
move_forest <- NULL
for(i in 1:n){
  
  k = as.character(caribou_IDs[i])
  
  move_covs_dat01 = indiv.se[IDYr == k]
  move_pars_dat01 = Params[IDYr == k]
  
  prop = seq(0,1, by = 0.01)
  
  shape = move_pars_dat01$shape
  scale = move_pars_dat01$scale
  kappa = move_pars_dat01$kappa
  
  covi = move_covs_dat01[term == "I(log(sl_ + 1))"]
  Bi = covi$estimate
  covij = fix_vals[term == "I(log(sl_ + 1)):propForest"]
  Bij = covij$estimate
  
  speed = (shape+Bi+(Bij*prop) + 
             (fix_vals[term == "I(log(sl_ + 1)):propLichen"]$estimate * median(DT$propLichen, na.rm = T)) +
             (fix_vals[term == "I(log(sl_ + 1)):propOpenMove"]$estimate * median(DT$propOpenMove, na.rm = T)) +
             #(move_covs_dat01[term == "I(log(StartDist + 1)):I(log(sl_ + 1))"]$estimate * log(median(DT$StartDist + 1, na.rm = T))) +
             (move_covs_dat01[term == "I(log(sl_ + 1)):I(log(sri + 0.125))"]$estimate * log(median(DT$sri + 0.125, na.rm = T))) 
           )*(scale)
  
  move_forest[[i]] = data.table(IDYr = k, 
                                Habitat = "Forest",
                                Bi = Bi, 
                                Bij = Bij, 
                                Proportion = prop, 
                                Speed = speed) 
  
  
}
move_forest <- rbindlist(move_forest)


## Calculate speed for Forest
move_open <- NULL
for(i in 1:n){
  
  k = as.character(caribou_IDs[i])
  
  move_covs_dat01 = indiv.se[IDYr == k]
  move_pars_dat01 = Params[IDYr == k]
  
  prop = seq(0,1, by = 0.01)
  
  shape = move_pars_dat01$shape
  scale = move_pars_dat01$scale
  
  covi = move_covs_dat01[term == "I(log(sl_ + 1))"]
  Bi = covi$estimate
  covij = fix_vals[term == "I(log(sl_ + 1)):propOpenMove"]
  Bij = covij$estimate
  
  speed = (shape+Bi+(Bij*prop) + 
             (fix_vals[term == "I(log(sl_ + 1)):propForest"]$estimate * median(DT$propForest, na.rm = T)) +
             (fix_vals[term == "I(log(sl_ + 1)):propLichen"]$estimate * median(DT$propLichen, na.rm = T)) +
             #(move_covs_dat01[term == "I(log(StartDist + 1)):I(log(sl_ + 1))"]$estimate * log(median(DT$StartDist + 1, na.rm = T))) +
             (move_covs_dat01[term == "I(log(sl_ + 1)):I(log(sri + 0.125))"]$estimate * log(median(DT$sri + 0.125, na.rm = T)))
           )*(scale)
  
  move_open[[i]] = data.table(IDYr = k, 
                                Habitat = "Open",
                                Bi = Bi, 
                                Bij = Bij, 
                                Proportion = prop, 
                                Speed = speed) 
  
  
}
move_open <- rbindlist(move_open)

move_all <- rbind(move_lichen, move_forest, move_open)


png("graphics/Fig3.png", width = 5000, height = 2500, units = "px", res = 600)
fig3a <- ggplot(data = move_all[Habitat == "Lichen"]) +
  geom_smooth(aes(Proportion, Speed, group = IDYr), 
              fill = "#91bfdb", 
              color = "#91bfdb", 
              method = 'glm') +
  xlab("Proportion of lichen") +
  ylab("Speed (m/hr)") +
  ylim(0, 600) +
  theme(legend.position = 'none', #legend.key =  element_blank(),
        strip.background = element_rect(color = "black",
                                        fill = "white",
                                        size = 1),
        strip.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) 

fig3b <- ggplot(data = move_all[Habitat == "Forest"]) +
  geom_smooth(aes(Proportion, Speed, group = IDYr), 
              fill = "#f1a340", 
              color = "#f1a340", 
              method = 'glm') +
  xlab("Proportion of forest") +
  ylab("Speed (m/hr)") +
  ylim(0, 600) +
  theme(legend.position = 'none', 
        strip.background = element_rect(color = "black",
                                        fill = "white",
                                        size = 1),
        strip.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) 


fig3c <- ggplot(data = move_all[Habitat == "Open"]) +
  geom_smooth(aes(Proportion, Speed, group = IDYr), 
              fill = "#5ab4ac", 
              color = "#5ab4ac", 
              method = 'glm') +
  xlab("Proportion of open") +
  ylab("Speed (m/hr)") +
  ylim(0, 600) +
  theme(legend.position = 'none', 
        strip.background = element_rect(color = "black",
                                        fill = "white",
                                        size = 1),
        strip.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) 

gridExtra::grid.arrange(fig3a, fig3b, fig3c, nrow = 1)
dev.off()



## load libraries
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed', 'raster', 'ggthemes', 'performance')
lapply(libs, require, character.only = TRUE)

# Load data
DT <- readRDS("output/location-data/5-rdm-locs-sri-NN.RDS")

DT[, .N, by = c("IDYr")]

DT$Use[DT$case_ == "TRUE"] <- 1
DT$Use[DT$case_ == "FALSE"] <- 0

## number of groups
length(unique(DT[iter == 1]$groupEnd))

## open areas as the reference category
DT[,habitat := factor(habitat, levels = c('openMove', 'Forest', 'openForage'))]


## create unique step id by animal
DT[,'caribou_step_id_'] <- paste(DT$IDYr, DT$step_id_, sep = '_')
#DT[,'caribou_iter_step_id_'] <- paste(DT$IDYr, DT$iter, DT$step_id_, sep = '_')


## core model (no interactions or random effects)
core_ssf_vif <- glmmTMB(Use ~ 
                      ## step length and turn angle fixed and random effects 
                      I(log(sl_+1)) + 
                      cos(ta_)  + 
                      habitat +
                      (1|caribou_step_id_) + 
                      
                      #I(log(sl_+1))*habitat + 
                      #cos(ta_)*habitat + 
                      
                      #(0 + I(log(sl_+1)) | IDYr) + 
                      #(0 + cos(ta_) | IDYr) + 
                      (1 | IDYr),
                      #(0 + I(log(sl_+1)):habitat | IDYr)  +
                      #(0 + cos(ta_):habitat | IDYr),
                      
                    family=poisson(), 
                    data = DT,
                    map = list(theta=factor(c(NA,1:1))), 
                    start = list(theta=c(log(100), seq(0,0, length.out = 1))))

summary(core_ssf_vif)
check_collinearity(core_ssf_vif)
saveRDS(core_ssf_vif, "output/issa models/1-core_ssf_vif.RDS")

## core model (no random effects)
core_ssf_no_rdm <- glmmTMB(Use ~ 
                                          ## step length and turn angle fixed and random effects 
                                          I(log(sl_+1)) + 
                                          cos(ta_)  + 
                                          habitat +
                                          (1|step_id_) + 
                                          
                                          I(log(sl_+1))*habitat + 
                                          I(log(sl_+1))*cos(ta_) +
                                          
                                          (1 | IDYr),
                                        
                                        family=poisson(), 
                                        data = DT,
                                        map = list(theta=factor(c(NA,1:1))), 
                                        start = list(theta=c(log(100), seq(0,0, length.out = 1))))

summary(core_ssf_no_rdm)
saveRDS(core_ssf_no_rdm, "output/issa models/2-core_ssf_no_rdm.RDS")


## core model (interactions and random effects included)
core_ssf_rdm <- glmmTMB(Use ~ 
                                      ## step length and turn angle fixed and random effects 
                                      I(log(sl_+1)) + 
                                      cos(ta_)  + 
                                      habitat +
                                      (1|caribou_step_id_) + 
                                      
                                      I(log(sl_+1))*habitat + 
                                      I(log(sl_+1))*cos(ta_) +
                                       
                                      (0 + I(log(sl_+1)) | IDYr) + 
                                      (0 + cos(ta_) | IDYr) + 
                                      (0 + habitat | IDYr),
                                    
                                    family=poisson(), 
                                    data = DT,  
                                    map = list(theta=factor(c(NA,1:8))), 
                                    start = list(theta=c(log(1000), seq(0,0, length.out = 8))))

summary(core_ssf_rdm)
saveRDS(core_ssf_rdm, "output/issa models/3-core_issa_rdm.RDS")

## core model (interactions and random effects included)
core_ssf_rdm_int <- glmmTMB(Use ~ 
                          ## step length and turn angle fixed and random effects 
                          I(log(sl_+1)) + 
                          cos(ta_)  + 
                          habitat +
                          (1|caribou_step_id_) + 
                          
                          I(log(sl_+1))*habitat + 
                          I(log(sl_+1))*cos(ta_) +
                          
                          (0 + I(log(sl_+1)):habitat | IDYr) +
                          (0 + I(log(sl_+1)):cos(ta_) | IDYr),
                        
                        family=poisson(), 
                        data = DT,  
                        map = list(theta=factor(c(NA,1:7))), 
                        start = list(theta=c(log(1000), seq(0,0, length.out = 7))))

summary(core_ssf_rdm_int)
saveRDS(core_ssf_rdm_int, "output/issa models/4-core_ssf_rdm_int.RDS")


## NN model to run VIF
NN_ssf_vif <- glmmTMB(Use ~ 
                        ## habitat variables: fixed and random effects
                        ## step length and turn angle fixed and random effects 
                        I(log(sl_+1)) + 
                        cos(ta_) +
                        (1|caribou_step_id_) + 
                        
                        ## habitat variables: fixed and random effects
                        habitat +
                        ## social variables in interactions with movement and habitat 
                        I(log(EndDist + 1)) + 
                        
                        (1 | IDYr),
                      
                      family=poisson(), 
                      data = DT,  
                      map = list(theta=factor(c(NA,1:1))), 
                      start = list(theta=c(log(1000), seq(0,0, length.out = 1))))

summary(NN_ssf_vif)
check_collinearity(NN_ssf_vif)
saveRDS(NN_ssf_vif, "output/issa models/1-NN_ssf_vif.RDS")


## NN model with interactions and no random effects
NN_ssf_no_rdm <- glmmTMB(Use ~ 
                           ## habitat variables: fixed and random effects
                           ## step length and turn angle fixed and random effects 
                           I(log(sl_+1)) + 
                           cos(ta_) +
                           I(log(sl_+1))*habitat + 
                           #cos(ta_)*habitat + 
                           cos(ta_)*I(log(sl_+1)) +
                           (1|caribou_step_id_) + 
                           
                           ## habitat variables: fixed and random effects
                           habitat +
                           ## social variables in interactions with movement and habitat 
                           I(log(EndDist + 1)) + 
                           I(log(StartDist + 1))*I(log(sl_+1)) +
                           I(log(EndDist + 1))*habitat + 
                           
                           (1 | IDYr),
                         
                         family=poisson(), 
                         data = DT,  
                         map = list(theta=factor(c(NA,1:1))), 
                         start = list(theta=c(log(1000), seq(0,0, length.out = 1))))

summary(NN_ssf_no_rdm)
saveRDS(NN_ssf_no_rdm, "output/issa models/2-NN_ssf_no_rdm.RDS")

## included random effects, but no interactions
NN_issa_rdm <- glmmTMB(Use ~ 
                      ## step length and turn angle fixed and random effects 
                      I(log(sl_+1)) + 
                      cos(ta_) +
                      I(log(sl_+1))*habitat + 
                      #cos(ta_)*habitat + 
                      I(log(sl_+1))*cos(ta_) +
                      (1|caribou_step_id_) + 
                    
                      ## habitat variables: fixed and random effects
                      habitat +
                      
                      ## social variables in interactions with movement and habitat 
                      I(log(EndDist + 1)) + 
                      
                      I(log(StartDist + 1))*I(log(sl_+1)) +
                      I(log(EndDist + 1))*habitat + 
                      
                      ## random effects    
                      (0 + habitat | IDYr) +
                      (0 + I(log(sl_+1)) | IDYr) + 
                      (0 + cos(ta_) | IDYr) +
                      (0 + I(log(EndDist+1)) | IDYr),
                        
                    family=poisson(), 
                    data = DT,  
                    map = list(theta=factor(c(NA,1:9))), 
                    start = list(theta=c(log(1000), seq(0,0, length.out = 9))))

summary(NN_issa_rdm)
saveRDS(NN_issa_rdm, "output/issa models/3-NN_issa_rdm.RDS")


## SRI model to run VIF
SRI_ssf_vif <- glmmTMB(Use ~ 
                        ## habitat variables: fixed and random effects
                        ## step length and turn angle fixed and random effects 
                        I(log(sl_+1)) + 
                        cos(ta_) +
                        (1|caribou_step_id_) + 
                        
                        ## habitat variables: fixed and random effects
                        habitat +
                        ## social variables in interactions with movement and habitat 
                        I(log(EndDist + 1)) + 
                        I(log(sri+0.125)) +
                        (1 | IDYr),
                      
                      family=poisson(), 
                      data = DT,  
                      map = list(theta=factor(c(NA,1:1))), 
                      start = list(theta=c(log(1000), seq(0,0, length.out = 1))))

summary(SRI_ssf_vif)
check_collinearity(SRI_ssf_vif)
saveRDS(SRI_ssf_vif, "output/issa models/1-SRI_ssf_vif.RDS")

## included random effects, but no interactions

SRI_issa_rdm <- glmmTMB(case_ ~ 
                         ## step length and turn angle fixed and random effects 
                         I(log(sl_+1)) + 
                         #I(cos(ta_)) +
                         #I(log(sl_+1))*habitat + 
                          I(log(sl_+1)):(propOpenMove + propForest + propLichen) +
                         #I(log(sl_+1)):I(cos(ta_)) +
                         
                         
                         ## habitat variables
                         #habitat +
                          propOpenMove + propForest + propLichen +
                         ## social variables in interactions with movement and habitat 
                         I(log(EndDist + 1)) + 
                         I(log(StartDist + 1)):I(log(sl_+1)) +
                         I(log(EndDist + 1)):(propOpenMove + propForest + propLichen) + 
                         
                         I(log(sri+0.125)) +
                         I(log(sri+0.125)):I(log(sl_+1)) +
                         I(log(sri+0.125)):(propOpenMove + propForest + propLichen) + 
                          
                         ## random effects  
                          (1|caribou_step_id_) + 
                          (0 + I(log(sl_+1)) | IDYr) +
                         # (0 + cos(ta_) | IDYr) +
                          (0 + propOpenMove:I(log(EndDist+1)) | IDYr) +
                          (0 + propOpenMove:I(log(sri+0.125))| IDYr) +
                          (0 + propForest:I(log(EndDist+1)) | IDYr) +
                          (0 + propForest:I(log(sri+0.125))| IDYr) +
                          (0 + propLichen:I(log(EndDist+1)) | IDYr) +
                          (0 + propLichen:I(log(sri+0.125))| IDYr) +
                          (0 + I(log(EndDist+1)) | IDYr) +
                          (0 + I(log(sri+0.125)) | IDYr) +
                          (0 + I(log(StartDist + 1)):I(log(sl_+1)) | IDYr) +
                          (0 + I(log(sl_+1)):I(log(sri+0.125))| IDYr),
                        
                       family=poisson(), 
                       data = DT,  
                       map = list(theta=factor(c(NA,1:11))), 
                       start = list(theta=c(log(1000), seq(0,0, length.out = 11))))

summary(SRI_issa_rdm)
ran_vals <-tidy(SRI_issa_rdm, effect= 'ran_vals')
indiv.se <-setDT(ran_vals)[group=='IDYr']

saveRDS(SRI_issa_rdm, "output/issa models/3-SRI_issa_rdm.RDS")


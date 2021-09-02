

## load libraries
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed', 'raster', 'ggthemes', 'performance', 'AICcmodavg')
lapply(libs, require, character.only = TRUE)

# Load data
DT <- readRDS("output/location-data/5-rdm-locs-sri-NN.RDS")

DT[, .N, by = c("IDYr")]

DT$Use[DT$case_ == "TRUE"] <- 1
DT$Use[DT$case_ == "FALSE"] <- 0

## number of groups
length(unique(DT[iter == 1]$groupEnd))

## create unique step id by animal
DT[,'caribou_step_id_'] <- paste(DT$IDYr, DT$step_id_, sep = '_')

### ISSF MODEL WITH NN
NN_issa_rdm <- glmmTMB(case_ ~ 
                          ## step length
                          I(log(sl_+1)) + 
                          I(log(sl_+1)):(propOpenMove + propForest + propLichen) +
                          
                          ## habitat variables
                          propOpenMove + propForest + propLichen +
                          
                          ## social variables in interactions with movement and habitat 
                          I(log(EndDist + 1)) + 
                          I(log(StartDist + 1)):I(log(sl_+1)) +
                          I(log(EndDist + 1)):(propOpenMove + propForest + propLichen) + 
                          
                          ## random effects  
                          (1|caribou_step_id_) + 
                          (0 + I(log(sl_+1)) | IDYr) +
                          (0 + propOpenMove:I(log(EndDist+1)) | IDYr) +
                          (0 + propForest:I(log(EndDist+1)) | IDYr) +
                          (0 + propLichen:I(log(EndDist+1)) | IDYr) +
                          (0 + I(log(EndDist+1)) | IDYr) +
                          (0 + I(log(StartDist + 1)):I(log(sl_+1)) | IDYr),
                        
                        family=poisson(), 
                        data = DT,  
                        map = list(theta=factor(c(NA,1:6))), 
                        start = list(theta=c(log(1000), seq(0,0, length.out = 6))))
summary(NN_issa_rdm)
check_collinearity(NN_issa_rdm)

saveRDS(NN_issa_rdm, "output/issa models/NN_issa_rdm.RDS")


### ISSF MODEL WITH NN and SRI 
SRI_issa_rdm <- glmmTMB(case_ ~ 
                         ## step length
                         I(log(sl_+1)) + 
                         I(log(sl_+1)):(propOpenMove + propForest + propLichen) +
                         
                         ## habitat variables
                         propOpenMove + propForest + propLichen +
                          
                         ## social variables in interactions with movement and habitat 
                         I(log(sri+0.125)) +
                         I(log(sri+0.125)):I(log(sl_+1)) +
                         I(log(sri+0.125)):(propOpenMove + propForest + propLichen) + 
                          
                         ## random effects  
                          (1|caribou_step_id_) + 
                          (0 + I(log(sl_+1)) | IDYr) +
                          (0 + propOpenMove:I(log(sri+0.125))| IDYr) +
                          (0 + propForest:I(log(sri+0.125))| IDYr) +
                          (0 + propLichen:I(log(sri+0.125))| IDYr) +
                          (0 + I(log(sri+0.125)) | IDYr) +
                          (0 + I(log(sl_+1)):I(log(sri+0.125))| IDYr),
                        
                       family=poisson(), 
                       data = DT,  
                       map = list(theta=factor(c(NA,1:6))), 
                       start = list(theta=c(log(1000), seq(0,0, length.out = 6))))

summary(SRI_issa_rdm)
check_collinearity(SRI_issa_rdm)

saveRDS(SRI_issa_rdm, "output/issa models/SRI_issa_rdm.RDS")


##assign model names
Modnames <- c("NN", "SRI")

##compute model selection table
aicctable.out <- aictab(cand.set = list(NN_ssf, sri_ssf), 
                        modnames = Modnames)

##compute evidence ratio
evidence(aic.table = aicctable.out)

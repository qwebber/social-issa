

#' Get started and load libraries
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed', 'raster', 'ggthemes')
lapply(libs, require, character.only = TRUE)

# Load data
DT <- readRDS("output/location-data/5-rdm-locs-sri-NN.RDS")

DT[, .N, by = c("IDYr")]

DT$Use[DT$case_ == "TRUE"] <- 1
DT$Use[DT$case_ == "FALSE"] <- 0

## number of groups
length(unique(DT[iter == 1]$groupEnd))

## open areas as the reference category
DT$habitat[DT$habitat == "openMove"] = "aOpenMove"

## NN + SRI model
core_ssf <- glmmTMB(Use ~ 
                      ## step length and turn angle fixed and random effects 
                      I(log(sl_+1)) + 
                      cos(ta_)  + 
                      (1|step_id_) + 
                      I(log(sl_+1))*habitat + 
                      cos(ta_)*habitat + 
                      (0 + I(log(sl_+1)) | IDYr) + 
                      
                      ## habitat variables: fixed and random effects
                      habitat +
                      (0 + habitat | IDYr),
                    family=poisson(), 
                    data = DT,  
                    map = list(theta=factor(c(NA,1:7))), 
                    start = list(theta=c(log(1000), seq(0,0, length.out = 7))))

summary(core_ssf)

saveRDS(core_ssf, "output/issa models/core_issa.RDS")

#' Look at individual coefficients

indivs <- coef(core_ssf)$cond$IDYr[ , -1] %>% tibble::rownames_to_column("IDYr") %>% 
  pivot_longer(-IDYr, names_to = "term", values_to = "estimate") %>% 
  mutate(method = "ME")

ggplot(indivs) +
  geom_density(aes(estimate)) +
  facet_wrap(~term, scale = 'free') 

## NN + SRI model
NN_ssf <- glmmTMB(Use ~ 
                      ## step length and turn angle fixed and random effects 
                      I(log(sl_+1)) + 
                      cos(ta_) +
                      I(log(sl_+1))*habitat + 
                      cos(ta_)*habitat + 
                      (1|step_id_) + 
                      (0 + I(log(sl_+1)) | IDYr) + 
                      
                      ## habitat variables: fixed and random effects
                      habitat +
                      (0 + habitat | IDYr) +
                      
                      ## social variables in interactions with movement and habitat 
                      I(log(EndDist + 1)) + 
                      (0 + I(log(EndDist+1)) | IDYr) + 
                      
                      I(log(StartDist + 1))*I(log(sl_+1)) +
                      #I(log(StartDist + 1))*ta_ +
                      (0 + I(log(StartDist + 1)):I(log(sl_+1)) | IDYr)  +
                      
                      I(log(EndDist + 1))*habitat + 
                      (0 + I(log(EndDist + 1)):habitat | IDYr), 
                    family=poisson(), 
                    data = DT,  
                    map = list(theta=factor(c(NA,1:15))), 
                    start = list(theta=c(log(1000), seq(0,0, length.out = 15))))

# we use the map function to set the variance really high for the step ids
summary(NN_ssf)

saveRDS(NN_ssf, "output/issa models/NN_issa.RDS")

indivsNN <- coef(NN_ssf)$cond$IDYr[ , -1] %>% tibble::rownames_to_column("IDYr") %>% 
  pivot_longer(-IDYr, names_to = "term", values_to = "estimate") %>% 
  mutate(method = "ME")

ggplot(indivsNN) +
  geom_density(aes(estimate)) +
  geom_vline(xintercept = 0, lty = 2) +
  facet_wrap(~term, scale = 'free') 


## NN + SRI model
sri_ssf <- glmmTMB(Use ~ 
                    ## step length and turn angle fixed and random effects 
                    I(log(sl_+1)) + 
                    cos(ta_)  +
                    #tod_end_ +
                    I(log(sl_+1))*habitat + 
                    cos(ta_)*habitat + 
                    (1|step_id_) + 
                    #(0 + ta_ | IDYr) + 
                    #(0 + I(log(sl_+1)) | IDYr) + 
                    
                    ## habitat variables: fixed and random effects
                    habitat +
                    #propLichen + 
                    #propForest + 
                    #propOpenMove + 
                    #(0 + habitat | IDYr) +
                    #(0 + propForest | IDYr) + 
                    #(0 + propOpenMove | IDYr) +
                    
                    ## social variables in interactions with movement and habitat 
                    I(log(EndDist + 1)) + 
                    #(0 + I(log(EndDist+1)) | IDYr) + 
                    
                    I(log(StartDist + 1))*I(log(sl_+1)) +
                    #I(log(StartDist + 1))*ta_ +
                    #(0 + I(log(StartDist + 1)):I(log(sl_+1)) | IDYr)  +
                    
                    I(log(EndDist + 1))*habitat + 
                    #(0 + I(log(EndDist + 1)):habitat | IDYr) + 
                     
                    #(0 + I(log(sri+0.125)) | IDYr) + 
                    
                     I(log(sri+0.125))*I(log(sl_+1)) +
                     (0 + I(log(sri+0.125)):I(log(sl_+1)) | IDYr)  +
                   
                     I(log(sri+0.125))*habitat + 
                     (0 + I(log(sri+0.125)):habitat | IDYr), 
                  family=poisson(), 
                  data = DT,  
                  map = list(theta=factor(c(NA,1:7))), 
                  start = list(theta=c(log(1000), seq(0,0, length.out = 7))))

summary(sri_ssf)
saveRDS(sri_ssf, "output/issa models/sri_issa.RDS")


indivsSRI <- coef(sri_ssf)$cond$IDYr[ , -1] %>% tibble::rownames_to_column("IDYr") %>% 
  pivot_longer(-IDYr, names_to = "term", values_to = "estimate") %>% 
  mutate(method = "ME")

ggplot(indivsSRI) +
  geom_density(aes(estimate)) +
  geom_vline(xintercept = 0, lty = 2) +
  facet_wrap(~term, scale = 'free') 

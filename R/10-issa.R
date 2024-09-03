

## load libraries
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed', 'raster', 'ggthemes', 'performance', 'AICcmodavg')
lapply(libs, require, character.only = TRUE)

# Load data
DT <- readRDS("output/location-data/5-rdm-locs-sri-NN-N20.RDS")

DT$dyad <- as.factor(paste(DT$ID1, DT$ID2, sep = "_"))
ggplot(DT[iter==1]) +
  geom_line(aes(JDate, sri, color = dyad), stat='identity') +
  ylab("Simple Ratio Index (SRI)") + xlab("Day of the year") + 
  scale_color_viridis_d() +
  theme(legend.position = 'none', 
        strip.background = element_rect(color = "black", 
                                        fill = "white"),
        strip.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text.x = element_text(size = 12, color = 'black'), 
        axis.text.y = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  facet_wrap(~Year)

DT[, .N, by = c("IDYr")]

DT$Use[DT$case_ == "TRUE"] <- 1
DT$Use[DT$case_ == "FALSE"] <- 0

## check overlap between used/available points
ggplot(DT[step_id_ == 228 & ANIMAL_ID == "FO2018002"]) +
  geom_point(aes(x2_, y2_, color = as.factor(Use)), pch=21,
                            size=4) 

## number of groups
length(unique(DT[iter == 1]$groupEnd))

## create unique step id by animal
DT[,'caribou_step_id_'] <- paste(DT$IDYr, DT$step_id_, sep = '_')

null20 <- glmmTMB(case_ ~ 
                       ## step length
                       I(log(sl_+1)) + 
                       I(log(sl_+1)):(
                         propForest + propLichen) +
                       
                       
                       ## habitat variables
                       propForest + 
                       propLichen +
                       
                       ## random effects  
                       (1|caribou_step_id_) + 
                       (0 + I(log(sl_+1)) | IDYr) +
                       (0 + propForest:I(log(sl_+1)) | IDYr) +
                       (0 + propLichen:I(log(sl_+1)) | IDYr),
                     
                     family=poisson(), 
                     data = DT,  
                     map = list(theta=factor(c(NA,1:3))), 
                     start = list(theta=c(log(1000), seq(0,0, length.out = 3))))


null20_ta <- glmmTMB(case_ ~ 
                       ## step length
                       I(log(sl_+1)) + 
                       I(log(sl_+1)):(
                         propForest + propLichen) +
                       
                    
                       ## habitat variables
                       propForest + 
                       propLichen +
                    
                       ## TA
                    I(ta_) + 
                    I(ta_):(
                      propForest + propLichen) +
                       ## random effects  
                       (1|caribou_step_id_) + 
                       (0 + I(log(sl_+1)) | IDYr) +
                       (0 + propForest:I(log(sl_+1)) | IDYr) +
                       (0 + propLichen:I(log(sl_+1)) | IDYr) + 
                       (0 + propForest:I(ta_) | IDYr) +
                       (0 + propLichen:I(ta_) | IDYr),
                     
                     family=poisson(), 
                     data = DT,  
                     map = list(theta=factor(c(NA,1:5))), 
                     start = list(theta=c(log(1000), seq(0,0, length.out = 5))))

summary(null20_ta)
check_collinearity(null20_ta)

### ISSF MODEL WITH NN and SRI 
SRI_20 <- glmmTMB(case_ ~ 
                         ## step length
                         I(log(sl_+1)) + 
                         I(log(sl_+1)):(#propOpenMove + 
                           propForest + propLichen) +
                         
                         ## habitat variables
                         #propOpenMove + 
                         propForest + 
                         propLichen +
                          
                         ## social variables in interactions with movement and habitat 
                         I(log(EndDist + 1)) + 
                         I(log(StartDist + 1)):I(log(sl_+1)) +
                         I(log(EndDist + 1)):(#propOpenMove + 
                           propForest + propLichen) + 
                    
                         I(log(sri+0.125)) +
                         I(log(sri+0.125)):I(log(sl_+1)) +
                         I(log(sri+0.125)):(#propOpenMove + 
                           propForest + propLichen) + 
                          
                         ## random effects  
                          (1|caribou_step_id_) + 
                          (0 + I(log(sl_+1)) | IDYr) +
                          #(0 + propOpenMove:I(log(sri+0.125))| IDYr) +
                          (0 + propForest:I(log(sri+0.125))| IDYr) +
                          (0 + propLichen:I(log(sri+0.125))| IDYr) +
                          (0 + I(log(sri+0.125)) | IDYr) +
                          (0 + I(log(sl_+1)):I(log(sri+0.125))| IDYr) +
                          #(0 + propOpenMove:I(log(EndDist+1)) | IDYr) +
                          (0 + propForest:I(log(EndDist+1)) | IDYr) +
                          (0 + propLichen:I(log(EndDist+1)) | IDYr) +
                          (0 + I(log(EndDist+1)) | IDYr) +
                          (0 + I(log(StartDist + 1)):I(log(sl_+1)) | IDYr),
                        
                       family=poisson(), 
                       data = DT,  
                       map = list(theta=factor(c(NA,1:9))), 
                       start = list(theta=c(log(1000), seq(0,0, length.out = 9))))

summary(SRI_20)
check_collinearity(SRI_20)

saveRDS(SRI_20, "output/issa models/SRI_issa_20_2.RDS")
saveRDS(null20, "output/issa models/issa_null.RDS")
saveRDS(null20_ta, "output/issa models/issa_null_ta.RDS")

SRI_20 <- readRDS("output/issa models/SRI_issa_20_2.RDS")
null20 <- readRDS("output/issa models/issa_null.RDS")

## model selection
aa <- AIC(null20, SRI_20, null20_ta)
aa$delta <- aa$AIC - min(aa$AIC)

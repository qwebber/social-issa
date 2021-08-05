


### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed', 'raster', 'ggthemes')
lapply(libs, require, character.only = TRUE)

# Load data
DT <- readRDS("output/location-data/5-rdm-locs-sri-NN.RDS")
sri_ssf <- readRDS("output/issa models/3-sri_issa_rdm.RDS")
NN_ssf <- readRDS("output/issa models/3-NN_issa_rdm.RDS")
summary(sri_ssf)

DT[, .N, by = c("IDYr")]

DT$Use[DT$case_ == "TRUE"] <- 1
DT$Use[DT$case_ == "FALSE"] <- 0

## number of groups
length(unique(DT[iter == 1]$groupEnd))

## open areas as the reference category
DT$habitat[DT$habitat == "openMove"] = "aOpenMove"

## subset to only observed individuals
DT <- DT[iter == 1]

####  step length RSS ####
lichen_s1 <- DT[,.(sl_ = 1:500,
                   ta_ = mean(ta_),
                   habitat = factor('openForage'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist), 
                   step_id_ = NA, 
                   IDYr = NA)]

lichen_s2 <- DT[,.(sl_ = mean(sl_),
                   ta_ = mean(ta_),
                   habitat = factor('openForage'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist),
                   step_id_ = NA, 
                   IDYr = NA)]

lich_pred_s1 <- predict(sri_ssf, newdata = lichen_s1, type='link', re.form = NA)
lich_pred_s2 <- predict(sri_ssf, newdata = lichen_s2, type='link', re.form = NA)


forest_s1 <- DT[,.(sl_ = 1:500,
                   ta_ = mean(ta_),
                   habitat = factor('openForage'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist), 
                   step_id_ = NA, 
                   IDYr = NA)]
forest_s2 <- DT[,.(sl_ = mean(sl_),
                   ta_ = mean(ta_),
                   habitat = factor('Forest'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist),
                   step_id_ = NA, 
                   IDYr = NA)]

forest_pred_s1 <- predict(sri_ssf, newdata = forest_s1, type='link', re.form = NA)
forest_pred_s2 <- predict(sri_ssf, newdata = forest_s2, type='link', re.form = NA)


move_s1 <- DT[,.(sl_ = 1:500,
                 ta_ = mean(ta_),
                 habitat = factor('aOpenMove'),
                 StartDist = mean(StartDist),  
                 sri = mean(sri),
                 EndDist = mean(EndDist),
                 step_id_ = NA, 
                 IDYr = NA)]
move_s2 <- DT[,.(sl_ = mean(sl_),
                 ta_ = mean(ta_),
                 habitat = factor('aOpenMove'),
                 StartDist = mean(StartDist),  
                 sri = mean(sri),
                 EndDist = mean(EndDist),
                 step_id_ = NA, 
                 IDYr = NA)]

move_pred_s1 <- predict(sri_ssf, newdata = move_s1, type='link', re.form = NA)
move_pred_s2 <- predict(sri_ssf, newdata = move_s2, type='link', re.form = NA)


step_ssf <- data.table(h1 = c(lich_pred_s1, 
                            forest_pred_s1, 
                            move_pred_s1),
                     h2 = c(lich_pred_s2, 
                            forest_pred_s2, 
                            move_pred_s2),
                     rss = c(lich_pred_s1 - lich_pred_s2,
                             forest_pred_s1 - forest_pred_s2,
                             move_pred_s1 - move_pred_s2),
                     env = c(1:500, 1:500, 1:500),
                     category = c(rep("Lichen", 500),
                                  rep("Forest", 500),
                                  rep("Open", 500)),
                     mod = c(rep("Step", 500)))



#### NN RSS #####
lichen_h1 <- DT[,.(sl_ = mean(sl_),
                    ta_ = mean(ta_),
                    habitat = factor('openForage'),
                    StartDist = mean(StartDist),  
                    sri = mean(sri),
                    EndDist = 1:500,
                    step_id_ = NA, 
                    IDYr = NA)]

lichen_h2 <- DT[,.(sl_ = mean(sl_),
                   ta_ = mean(ta_),
                   habitat = factor('openForage'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist),
                   step_id_ = NA, 
                   IDYr = NA)]

lich_pred_h1 <- predict(sri_ssf, newdata = lichen_h1, type='link', re.form = NA)
lich_pred_h2 <- predict(sri_ssf, newdata = lichen_h2, type='link', re.form = NA)


forest_h1 <- DT[,.(sl_ = mean(sl_),
                   ta_ = mean(ta_),
                    habitat = factor('Forest'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                    EndDist = 1:500,
                    step_id_ = NA, 
                    IDYr = NA)]
forest_h2 <- DT[,.(sl_ = mean(sl_),
                   ta_ = mean(ta_),
                habitat = factor('Forest'),
                StartDist = mean(StartDist),  
                sri = mean(sri),
                EndDist = mean(EndDist),
                step_id_ = NA, 
                IDYr = NA)]

forest_pred_h1 <- predict(sri_ssf, newdata = forest_h1, type='link', re.form = NA)
forest_pred_h2 <- predict(sri_ssf, newdata = forest_h2, type='link', re.form = NA)


move_h1 <- DT[,.(sl_ = mean(sl_),
                 ta_ = mean(ta_),
                  habitat = factor('aOpenMove'),
                 StartDist = mean(StartDist),  
                 sri = mean(sri),
                  EndDist = 1:500,
                  step_id_ = NA, 
                  IDYr = NA)]
move_h2 <- DT[,.(sl_ = mean(sl_),
                 ta_ = mean(ta_),
              habitat = factor('aOpenMove'),
              StartDist = mean(StartDist),  
              sri = mean(sri),
              EndDist = mean(EndDist),
              step_id_ = NA, 
              IDYr = NA)]

move_pred_h1 <- predict(sri_ssf, newdata = move_h1, type='link', re.form = NA)
move_pred_h2 <- predict(sri_ssf, newdata = move_h2, type='link', re.form = NA)


NN_ssf <- data.table(h1 = c(lich_pred_h1, 
                               forest_pred_h1, 
                               move_pred_h1),
                    h2 = c(lich_pred_h2, 
                           forest_pred_h2, 
                           move_pred_h2),
                    rss = c(lich_pred_h1 - lich_pred_h2,
                            forest_pred_h1 - forest_pred_h2,
                            move_pred_h1 - move_pred_h2),
                      env = c(1:500, 1:500, 1:500),
                      category = c(rep("Lichen", 500),
                                   rep("Forest", 500),
                                   rep("Open", 500)),
                    mod = c(rep("NN", 500)))



#### SRI RSS #####
lichen_h1_sri <- DT[,.(sl_ = mean(sl_),
                       ta_ = mean(ta_),
                   habitat = factor('openForage'),
                   StartDist = mean(StartDist),    
                   sri = seq(from = 0, to = 1, length.out = 100),
                   EndDist = mean(EndDist),
                   step_id_ = NA, 
                   IDYr = NA)]

lichen_h2_sri <- DT[,.(sl_ = mean(sl_),
                       ta_ = mean(ta_),
                   habitat = factor('openForage'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist),
                   step_id_ = NA, 
                   IDYr = NA)]

lich_pred_h1_sri <- predict(sri_ssf, newdata = lichen_h1_sri, type='link', re.form = NA)
lich_pred_h2_sri <- predict(sri_ssf, newdata = lichen_h2_sri, type='link', re.form = NA)


forest_h1_sri <- DT[,.(sl_ = mean(sl_),
                       ta_ = mean(ta_),
                   habitat = factor('Forest'),
                   StartDist = mean(StartDist),  
                   sri = seq(from = 0, to = 1, length.out = 100),
                   EndDist = mean(EndDist),
                   step_id_ = NA, 
                   IDYr = NA)]
forest_h2_sri <- DT[,.(sl_ = mean(sl_),
                       ta_ = mean(ta_),
                   habitat = factor('Forest'),
                   StartDist = mean(StartDist),  
                   sri = mean(sri),
                   EndDist = mean(EndDist),
                   step_id_ = NA, 
                   IDYr = NA)]

forest_pred_h1_sri <- predict(sri_ssf, newdata = forest_h1_sri, type='link', re.form = NA)
forest_pred_h2_sri <- predict(sri_ssf, newdata = forest_h2_sri, type='link', re.form = NA)


move_h1_sri <- DT[,.(sl_ = mean(sl_),
                     ta_ = mean(ta_),
                 habitat = factor('aOpenMove'),
                 StartDist = mean(StartDist),  
                 sri = seq(from = 0, to = 1, length.out = 100),
                 EndDist = mean(EndDist),
                 step_id_ = NA, 
                 IDYr = NA)]
move_h2_sri <- DT[,.(sl_ = mean(sl_),
                     ta_ = mean(ta_),
                 habitat = factor('aOpenMove'),
                 StartDist = mean(StartDist),  
                 sri = mean(sri),
                 EndDist = mean(EndDist),
                 step_id_ = NA, 
                 IDYr = NA)]

move_pred_h1_sri <- predict(sri_ssf, newdata = move_h1_sri, type='link', re.form = NA)
move_pred_h2_sri <- predict(sri_ssf, newdata = move_h2_sri, type='link', re.form = NA)


sri_rss <- data.table(h1 = c(lich_pred_h1_sri, 
                            forest_pred_h1_sri, 
                            move_pred_h1_sri),
                     h2 = c(lich_pred_h2_sri, 
                            forest_pred_h2_sri, 
                            move_pred_h2_sri),
                     rss = c(lich_pred_h1_sri - lich_pred_h2_sri,
                             forest_pred_h1_sri - forest_pred_h2_sri,
                             move_pred_h1_sri - move_pred_h2_sri),
                     env = c(seq(from = 0, to = 1, length.out = 100), 
                             seq(from = 0, to = 1, length.out = 100),
                             seq(from = 0, to = 1, length.out = 100)),
                     category = c(rep("Lichen", 100),
                                  rep("Forest", 100),
                                  rep("Open", 100)),
                     mod = c(rep("sri", 100)))


df <- rbind(step_ssf ,NN_ssf, sri_rss)
saveRDS(df, "output/11-RSS.RDS")

png("graphics/FigS4.png", width = 5000, height = 2500, units = "px", res = 600)
aa <- ggplot(step_ssf) +
  geom_line(aes(env, rss, color = category)) +
  scale_color_manual(values = c("#f1a340", "#91bfdb", "#5ab4ac")) +
  ylab("log(relative selection strength)") +
  xlab("Step length") +
  ggtitle("A)") +
  geom_hline(yintercept = 0, lty = 2) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
  
bb <- ggplot(NN_ssf) +
  geom_line(aes(env, rss, color = category)) +
  scale_color_manual(values = c("#f1a340", "#91bfdb", "#5ab4ac")) +
  ylab("log(relative selection strength)") +
  xlab("NN distance (m)") +
  ggtitle("B)") +
  geom_hline(yintercept = 0, lty = 2) +
  theme(legend.position = c(0.7,0.85),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))

cc <- ggplot(sri_rss) +
  geom_line(aes(env, rss, color = category)) +
  scale_color_manual(values = c("#f1a340", "#91bfdb", "#5ab4ac")) +
  ylab("log(relative selection strength)") +
  xlab("Simple ratio index") +
  ggtitle('C)') +
  geom_hline(yintercept = 0, lty = 2) +
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
  
gridExtra::grid.arrange(aa, bb, cc, nrow = 1)
dev.off()

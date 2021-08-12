


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
          sri = ifelse(socvar == 'sri', seq(0, 1, length.out = 100), mean(sri, na.rm = T)),
          EndDist = ifelse(socvar == 'EndDist', 1:500, mean(EndDist, na.rm = T)),
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
lichen_rss <- lichen_pred_s1[,.(IDYr, rss = hab-hab_pred_s2$h2)]



## forest
forest_pred_s1 <- rbindlist(lapply(caribouID, function(i) {
  #unique(
  DT[
    ,.(hab = predict(
      sri_ssf,
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
      sri_ssf,
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
lichen_pred_s1_NN <- p.indiv(ids = caribouID, DT, mod = sri_ssf, habvar = 'lichen', habvalue = 0.75, socvar = 'EndDist', socvalue = 1:500)
#lichen_pred_s1_NN_pop <- p.pop(DT, mod = sri_ssf, var = 'EndDist', value = 1:500, habitat = 'openForage')
forest_pred_s1_NN <- p.indiv(ids = caribouID, DT, mod = sri_ssf, habvar = 'forest', habvalue = 0.75, socvar = 'EndDist', socvalue = 1:500)
open_pred_s1_NN <- p.indiv(ids = caribouID, DT, mod = sri_ssf, habvar = 'open', habvalue = 0.75, socvar = 'EndDist', socvalue = 1:500)

####  sri RSS by ID ####
lichen_pred_s1_sri <- p.indiv(ids = caribouID, DT, mod = sri_ssf, habvar = 'lichen', habvalue = 0.75, socvar = 'sri', socvalue = seq(0, 1, length.out = 100))
#lichen_pred_s1_NN_pop <- p.pop(DT, mod = sri_ssf, var = 'EndDist', value = 1:500, habitat = 'openForage')
forest_pred_s1_sri <- p.indiv(ids = caribouID, DT, mod = sri_ssf, habvar = 'forest', habvalue = 0.75, socvar = 'sri', socvalue = seq(0, 1, length.out = 100))
open_pred_s1_sri <- p.indiv(ids = caribouID, DT, mod = sri_ssf, habvar = 'open', habvalue = 0.75, socvar = 'sri', socvalue = seq(0, 1, length.out = 100))



lichen_NN_rss <- merge(rbindlist(lichen_pred_s1_NN), lichen_pred_s2[,.(IDYr, h2=hab)], by = 'IDYr')
lichen_NN_rss[,rss:=hab-h2]
lichen_NN_rss <- merge(lichen_NN_rss, lichen_rss[.(IDYr, rss_hab = rss)], by = 'IDYr')
lichen_NN_rss[,rss_total:= rss + rss_hab]

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

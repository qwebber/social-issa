
## load libraries
libs <- c('data.table', 'dplyr','ggplot2', 'glmmTMB', 
          'broom')
lapply(libs, require, character.only = TRUE)


core_ssf_no_rdm<- readRDS("output/issa models/2-core_ssf_no_rdm.RDS")
core_ssf_rdm<- readRDS("output/issa models/3-core_issa_rdm.RDS")
core_ssf_rdm_int <- readRDS("output/issa models/4-core_ssf_rdm_int.RDS")

#NN_ssf_no_rdm <- readRDS("output/issa models/2-NN_ssf_no_rdm.RDS")
NN_ssf_rdm <- readRDS("output/issa models/3-NN_issa_rdm.RDS")
#NN_ssf_rdm_int <- readRDS("output/issa models/4-NN_ssf_rdm_int.RDS")
SRI_ssf_rdm <- readRDS("output/issa models/3-SRI_issa_rdm.RDS")


aic <- data.table(model = c( "Core 1 (No random terms)", 
                            "Core 2 (Random terms included, no interactions)",
                            "Core 3 (Random terms included, only interactions)",
                            "NN (Random terms included, no interactions)",
                            "SRI (Random terms included, no interactions)"),
                  AIC = c(AIC(core_ssf_no_rdm),
                          260386.6, #AIC(core_ssf_rdm),
                          AIC(core_ssf_rdm_int),
                          AIC(NN_ssf_rdm),
                          AIC(SRI_ssf_rdm)))

aic$deltaAIC = min(aic$AIC, na.rm = T) - aic$AIC
aic

summary(SRI_ssf_rdm)

eff1 <- data.table(term = rbind(data.table(rownames(data.frame(summary(core_ssf_no_rdm)[6]$coefficients$cond[,1])))),
                   coef = data.table(summary(core_ssf_no_rdm)[6]$coefficients$cond[,1]),
                   se = data.table(summary(core_ssf_no_rdm)[6]$coefficients$cond[,2]),
                   model = "Core 1 (No random terms)",
                   set = "Core")

eff2 <- data.table(term = rbind(data.table(rownames(data.frame(summary(core_ssf_rdm)[6]$coefficients$cond[,1])))),
           coef = data.table(summary(core_ssf_rdm)[6]$coefficients$cond[,1]),
           se = data.table(summary(core_ssf_rdm)[6]$coefficients$cond[,2]),
           model = "Core 2 (Random terms included, no interactions)",
           set = "Core")

eff3 <- data.table(term = rbind(data.table(rownames(data.frame(summary(core_ssf_rdm_int)[6]$coefficients$cond[,1])))),
                   coef = data.table(summary(core_ssf_rdm_int)[6]$coefficients$cond[,1]),
                   se = data.table(summary(core_ssf_rdm_int)[6]$coefficients$cond[,2]),
                   model = "Core 3 (Random terms included, only interactions)",
                   set = "Core")

eff4 <- data.table(term = rbind(data.table(rownames(data.frame(summary(NN_ssf_rdm)[6]$coefficients$cond[,1])))),
                   coef = data.table(summary(NN_ssf_rdm)[6]$coefficients$cond[,1]),
                   se = data.table(summary(NN_ssf_rdm)[6]$coefficients$cond[,2]),
                   model = "NN (Random terms included, no interactions)",
                   set = "NN")

eff5 <- data.table(term = rbind(data.table(rownames(data.frame(summary(SRI_ssf_rdm)[6]$coefficients$cond[,1])))),
                   coef = data.table(summary(SRI_ssf_rdm)[6]$coefficients$cond[,1]),
                   se = data.table(summary(SRI_ssf_rdm)[6]$coefficients$cond[,2]),
                   model = "SRI (Random terms included, no interactions)",
                   set = "SRI")

eff_all <- rbind(eff1, eff2, eff3, eff4, eff5)

eff_all$CIlow <- (eff_all$se * -0.96)

setnames(eff_all, c("term.V1", "coef.V1", "se.V1"), c("term", "coef", "se"))

png("graphics/FigS7.png", width = 5000, height = 3000, units = "px", res = 600)
ggplot(eff_all[term != "(Intercept)" & set == "Core"]) +
  geom_point(aes(coef, term, color = model), 
             position = position_dodge(width = 0.4)) +
  geom_errorbarh(aes(y = term, 
                     xmin = coef - se,
                     xmax = coef + se, 
                     color = model), 
                 position = position_dodge(width = 0.4), height = 0) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 1.5, lty = 2) +
  geom_hline(yintercept = 2.5, lty = 2) +
  geom_hline(yintercept = 3.5, lty = 2) +
  geom_hline(yintercept = 4.5, lty = 2) +
  geom_hline(yintercept = 5.5, lty = 2) +
  geom_hline(yintercept = 6.5, lty = 2) +
  geom_hline(yintercept = 7.5, lty = 2) +
  geom_hline(yintercept = 8.5, lty = 2) +
  geom_hline(yintercept = 9.5, lty = 2) +
  geom_hline(yintercept = 10.5, lty = 2) +
  geom_hline(yintercept = 11.5, lty = 2) +
  geom_hline(yintercept = 12.5, lty = 2) +
  geom_hline(yintercept = 13.5, lty = 2) +
  geom_hline(yintercept = 14.5, lty = 2) +
  geom_hline(yintercept = 15.5, lty = 2) +
    scale_y_discrete(labels = c(`I(log(sl_ + 1))` = "Step length", 
                              `cos(ta_)` = "Turn angle",
                              `habitatForest` = "Forest", 
                              `habitatopenForage` = "Lichen",
                              `I(log(EndDist + 1))` = "Nearest neighbour (end)", 
                              `I(log(StartDist + 1))` = "Nearest neighbour (start)",
                              `I(log(sri + 0.125))` = "Simple ratio index", 
                              `I(log(sl_ + 1)):habitatForest` = "Step length : Forest", 
                              `I(log(sl_ + 1)):habitatopenForage` = "Step length : Lichen", 
                              `I(log(sl_ + 1)):cos(ta_)` = "Step length : Turn angle",
                              `cos(ta_):habitatForest` = "Turn angle : Forest", 
                              `cos(ta_):habitatopenForage` = "Turn angle : Lichen",
                              `I(log(sl_ + 1)):I(log(StartDist + 1))` = "Step length : Nearest neigbhour (start)",
                              `habitatForest:I(log(EndDist + 1))` = "Forest : Nearest neighbour (end)",
                              `habitatopenForage:I(log(EndDist + 1))` = "Lichen : Nearest neighbour (end)",
                              `I(log(sl_ + 1)):I(log(sri + 0.125))` = "Step length : Simple ratio index",
                              `habitatForest:I(log(sri + 0.125))` = "Forest : Simple ratio index",
                              `habitatopenForage:I(log(sri + 0.125))` = "Lichen : Simple ratio index",
                              `I(log(sl_ + 1)):habitatopenMove` = "Step length : Open habitat",
                              `cos(ta_):habitatopenMove` = "Turn angle : Open habitat")) +
 scale_color_viridis_d() + #(values = c("#7570b3", "#d95f02")) +
  xlab("Fixed effect coefficient estimate") + 
  ylab("")  +
  #xlim(-1,1) +
  theme(#legend.position = c(0.75,0.8),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
dev.off()

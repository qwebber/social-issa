
library(data.table)
library(lme4)
library(ggplot2)
library(broom)
library(dplyr)
library(dotwhisker)
library(ggforce)

## load data
core <- readRDS("output/issa models/core_issa.RDS")
NN_ssf <- readRDS("output/issa models/NN_issa.RDS")
sri_ssf <- readRDS("output/issa models/sri_issa.RDS")

aic <-AIC(core, NN_ssf, sri_ssf)

aic$delta <-  round(aic$AIC - 185513.1, 1)


## pull out fixed effects
sri_ssf2 <- tidy(sri_ssf)
sri_ssf2$model <- "SRI"

setDT(sri_ssf2)

sri_ssf2$term2 <- factor(sri_ssf2$term, levels=c(rev(sri_ssf2$term)))
sri_ssf2$model_var <- data.table(c(rep("Core", 5),
                                   rep("Nearest neighbour", 2),
                                   rep("Simple ratio index", 1),
                                   rep("Core", 4),
                                   rep("Nearest neighbour", 3),
                                   rep("Simple ratio index", 3),
                                   rep("NA", 8)))

png("graphics/Fig3.png", width = 5000, height = 5000, units = "px", res = 600)
ggplot(data = sri_ssf2[effect == "fixed" & term != "(Intercept)"]) +
  geom_point(aes(estimate, term2, color = model_var), size = 2) +
  geom_errorbar(aes(estimate, term2, 
                                xmin = estimate - std.error*1.96, 
                                xmax = estimate + std.error*1.96,
                    color = model_var), width = 0, size = 0.75) +  
  geom_vline(xintercept = 0, lty = 2) +
  scale_y_discrete(labels = c(`I(log(sl_ + 1))` = "Step length", 
                              `cos(ta_)` = "Turn angle",
                              `habitatForest` = "Forest", 
                              `habitatopenForage` = "Lichen",
                              `I(log(EndDist + 1))` = "Nearest neighbour (end)", 
                              `I(log(StartDist + 1))` = "Nearest neighbour (start)",
                              `I(log(sri + 0.125))` = "Simple ratio index", 
                              `I(log(sl_ + 1)):habitatForest` = "Step length : Forest", 
                              `I(log(sl_ + 1)):habitatopenForage` = "Step length : Lichen", 
                              `cos(ta_):habitatForest` = "Turn angle : Forest", 
                              `cos(ta_):habitatopenForage` = "Turn angle : Lichen",
                              `I(log(sl_ + 1)):I(log(StartDist + 1))` = "Step length : Nearest neigbhour (start)",
                              `habitatForest:I(log(EndDist + 1))` = "Forest : Nearest neighbour (end)",
                              `habitatopenForage:I(log(EndDist + 1))` = "Lichen : Nearest neighbour (end)",
                              `I(log(sl_ + 1)):I(log(sri + 0.125))` = "Step length : Simple ratio index",
                              `habitatForest:I(log(sri + 0.125))` = "Forest : Simple ratio index",
                              `habitatopenForage:I(log(sri + 0.125))` = "Lichen : Simple ratio index")) +
  scale_color_manual(values = c("#7570b3", "#d95f02", "#1b9e77"),
                     labels = c("Core", "Nearest neighbour", "Simple ratio index")) +
  xlab("Fixed effect coefficient estimate") + 
  ylab("") +
  theme(legend.position = c(0.75,0.8),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
facet_zoom(xlim = c(-0.4, 0.4)) # zoom.data = ifelse(a <= 0.5, NA, FALSE))
dev.off()


sri_ssf2$id <- c(1:26)

png("graphics/presentations/Fig3zoom.png", width = 5000, height = 3000, units = "px", res = 600)
ggplot(data = sri_ssf2[
                       id == 13 | id == 14 | id == 15 |
                       id == 16 | id == 17 | id == 18]) +  #effect == "fixed" & term != "(Intercept)"]) +
  geom_point(aes(estimate, term2, color = model_var), size = 2) +
  geom_errorbar(aes(estimate, term2, 
                    xmin = estimate - std.error*1.96, 
                    xmax = estimate + std.error*1.96,
                    color = model_var), width = 0, size = 0.75) +  
  geom_vline(xintercept = 0, lty = 2) +
  scale_y_discrete(labels = c(`I(log(sl_ + 1))` = "Step length", 
                              `cos(ta_)` = "Turn angle",
                              `habitatForest` = "Forest", 
                              `habitatopenForage` = "Lichen",
                              `I(log(EndDist + 1))` = "Nearest neighbour (end)", 
                              `I(log(StartDist + 1))` = "Nearest neighbour (start)",
                              `I(log(sri + 0.125))` = "Simple ratio index", 
                              `I(log(sl_ + 1)):habitatForest` = "Step length : Forest", 
                              `I(log(sl_ + 1)):habitatopenForage` = "Step length : Lichen", 
                              `cos(ta_):habitatForest` = "Turn angle : Forest", 
                              `cos(ta_):habitatopenForage` = "Turn angle : Lichen",
                              `I(log(sl_ + 1)):I(log(StartDist + 1))` = "Step length : Nearest neigbhour (start)",
                              `habitatForest:I(log(EndDist + 1))` = "Forest : Nearest neighbour (end)",
                              `habitatopenForage:I(log(EndDist + 1))` = "Lichen : Nearest neighbour (end)",
                              `I(log(sl_ + 1)):I(log(sri + 0.125))` = "Step length : Simple ratio index",
                              `habitatForest:I(log(sri + 0.125))` = "Forest : Simple ratio index",
                              `habitatopenForage:I(log(sri + 0.125))` = "Lichen : Simple ratio index")) +
  scale_color_manual(values = c("#d95f02", "#1b9e77"),
                     labels = c("Nearest neighbour", "Simple ratio index")) +
  xlab("Fixed effect coefficient estimate") + 
  ylab("") +
  theme(legend.position = c(0.2,0.8),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
dev.off()


library(data.table)
library(lme4)
library(ggplot2)
library(dplyr)
library(dotwhisker)
library(ggforce)
library(broom.mixed)
library(glmmTMB)

## load data
sri_ssf <- readRDS("output/issa models/SRI_issa_20.RDS")

## pull out fixed effects
sri_ssf2 <- broom.mixed::tidy(sri_ssf)

setDT(sri_ssf2)

sri_ssf2$term2 <- factor(sri_ssf2$term, levels=c(rev(sri_ssf2$term)))
sri_ssf2 <- sri_ssf2[effect == "fixed"]

sri_ssf2$var_col <- c("Core", "Core", "Open", "Forest", "Lichen",
                      "Core", "Core" ,"Open", "Forest", "Lichen",
                      "Core", "Open", "Forest", "Lichen",
                      "Core", "Open", "Forest", "Lichen")


sri_ssf2$lwrCI <- sri_ssf2$estimate - sri_ssf2$std.error*1.96
sri_ssf2$uprCI <- sri_ssf2$estimate + sri_ssf2$std.error*1.96


png("graphics/FigS5.png", width = 5000, height = 5000, units = "px", res = 600)
ggplot(data = sri_ssf2[effect == "fixed" & term != "(Intercept)"]) +
  geom_point(aes(estimate, term2, color = var_col), 
             size = 2) +
  geom_errorbar(aes(estimate, term2, 
                                xmin = estimate - std.error*1.96, 
                                xmax = estimate + std.error*1.96,
                    color = var_col), 
                width = 0, 
                size = 0.75) +  
  geom_vline(xintercept = 0, lty = 2) +
  scale_y_discrete(labels = c(`I(log(sl_ + 1))` = "Step length", 
                              `propOpenMove` = "Open",
                              `propForest` = "Forest", 
                              `propLichen` = "Lichen",
                              `I(log(EndDist + 1))` = "Nearest neighbour (end)", 
                              `I(log(StartDist + 1))` = "Nearest neighbour (start)",
                              `I(log(sri + 0.125))` = "Simple ratio index", 
                              `I(log(sl_ + 1)):propForest` = "Step length : Forest", 
                              `I(log(sl_ + 1)):propLichen` = "Step length : Lichen", 
                              `I(log(sl_ + 1)):propOpenMove` = "Step length : Open", 
                              `I(log(sl_ + 1)):I(log(StartDist + 1))` = "Step length : Nearest neigbhour (start)",
                              `propForest:I(log(EndDist + 1))` = "Forest : Nearest neighbour (end)",
                              `propLichen:I(log(EndDist + 1))` = "Lichen : Nearest neighbour (end)",
                              `propOpenMove:I(log(EndDist + 1))` = "Open : Nearest neighbour (end)",
                              `I(log(sl_ + 1)):I(log(sri + 0.125))` = "Step length : Simple ratio index",
                              `propForest:I(log(sri + 0.125))` = "Forest : Simple ratio index",
                              `propLichen:I(log(sri + 0.125))` = "Lichen : Simple ratio index",
                              `propOpenMove:I(log(sri + 0.125))` = "Open : Simple ratio index")) +
  scale_color_manual(values = c("darkgray","#f1a340", "#91bfdb", "#5ab4ac"),
                     labels = c("Core", "Forest", "Lichen", "Open")) +
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
facet_zoom(xlim = c(-2,2))
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

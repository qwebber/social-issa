

library(data.table)
library(ggplot2)

all_fold <- fread("output/issa models/all_k_fold.csv")
all_fold$fold <- as.character(all_fold$fold)

png("graphics/FigS7.png", width = 5000, height = 5000, units = "px", res = 600)
ggplot(data = all_fold[effect == "fixed" & term != "(Intercept)"]) +
  geom_point(aes(estimate, term2, color = fold), 
             size = 1,
             position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(estimate, term2, 
                    xmin = estimate - std.error*1.96, 
                    xmax = estimate + std.error*1.96,
                    color = fold), 
                size = 0.5,
                position = position_dodge(width = 0.8)) +  
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
  geom_hline(yintercept = 16.5, lty = 2) +
  scale_y_discrete(labels = c(`I(log(sl_ + 1))` = "Step length", 
                              #`propOpenMove` = "Open",
                              `propForest` = "Forest", 
                              `propLichen` = "Lichen",
                              `I(log(EndDist + 1))` = "Nearest neighbour (end)", 
                              `I(log(StartDist + 1))` = "Nearest neighbour (start)",
                              `I(log(sri + 0.125))` = "Simple ratio index", 
                              `I(log(sl_ + 1)):propForest` = "Step length : Forest", 
                              `I(log(sl_ + 1)):propLichen` = "Step length : Lichen", 
                              #`I(log(sl_ + 1)):propOpenMove` = "Step length : Open", 
                              `I(log(sl_ + 1)):I(log(StartDist + 1))` = "Step length : Nearest neigbhour (start)",
                              `propForest:I(log(EndDist + 1))` = "Forest : Nearest neighbour (end)",
                              `propLichen:I(log(EndDist + 1))` = "Lichen : Nearest neighbour (end)",
                              #`propOpenMove:I(log(EndDist + 1))` = "Open : Nearest neighbour (end)",
                              `I(log(sl_ + 1)):I(log(sri + 0.125))` = "Step length : Simple ratio index",
                              `propForest:I(log(sri + 0.125))` = "Forest : Simple ratio index",
                              `propLichen:I(log(sri + 0.125))` = "Lichen : Simple ratio index")) +
                              #`propOpenMove:I(log(sri + 0.125))` = "Open : Simple ratio index")) +
  scale_color_viridis_d() +
  xlab("Fixed effect coefficient estimate") + 
  ylab("") +
  theme(#legend.position = c(0.75,0.8),
    #$legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 14, color = 'black'),
    axis.text = element_text(size = 12, color = 'black'),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA, size = 1))
dev.off()

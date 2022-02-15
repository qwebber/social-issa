
## load packages
library(ggplot2)
library(gridExtra)
library(data.table)

## load RSS data
df_id_sri <- readRDS("output/issa models/11-SRI-RSS-ID.RDS")
df_id_NN <- readRDS("output/issa models/11-NN-RSS-ID.RDS")
df_id_NN$NN <- rep(1:500, 114)

df_id_sri$habvar[df_id_sri$habvar == "forest"] <- "Forest"
df_id_sri$habvar[df_id_sri$habvar == "lichen"] <- "Lichen"
df_id_sri$habvar[df_id_sri$habvar == "open"] <- "Open"
df_id_NN$habvar[df_id_NN$habvar == "forest"] <- "Forest"
df_id_NN$habvar[df_id_NN$habvar == "lichen"] <- "Lichen"
df_id_NN$habvar[df_id_NN$habvar == "open"] <- "Open"

avg <- setDT(df_id_NN)[, mean(rss_total), by = c("NN", "habvar")]

png("graphics/Fig4.png", width = 6000, height = 4000, units = "px", res = 600)
aa <- ggplot() +
  geom_line(data = df_id_NN, 
            aes(NN, rss_total, 
                group = IDYr, 
                color = habvar),
            lty = 1, 
            lwd = 0.1) +
  geom_line(data = avg, 
              aes(NN, V1, 
                  group = habvar, 
                  color = habvar), 
            lwd = 1) +
  scale_color_manual(values = c("#f1a340", "#91bfdb", "#5ab4ac")) +
  ylab("log(relative selection strength)") +
  xlab("Nearest Neighbour distance (m)") +
  geom_hline(yintercept = 0, lty = 2) +
  theme(legend.position = 'none',
        strip.background = element_rect(color = "black", 
                                        fill = "white", 
                                        size = 1),
        strip.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text.x = element_text(size = 12, color = 'black'), #hjust = 1, angle = 45),
        axis.text.y = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  facet_wrap(~habvar)

bb <- ggplot() +
  geom_line(data = df_id_sri, 
            aes(x, rss_total, 
                group = IDYr, 
                color = habvar), 
            lty = 1, 
            lwd = 0.1) +
  geom_smooth(data = df_id_sri, 
              aes(x, rss_total, 
                  group = habvar, 
                  color = habvar), 
              se = F, 
              lwd = 1) +
  scale_color_manual(values = c("#f1a340", "#91bfdb", "#5ab4ac")) +
  ylab("log(relative selection strength)") +
  xlab("Simple ratio index") +
  geom_hline(yintercept = 0, lty = 2) +
  #xlim(0, 0.25) + 
  theme(legend.position = 'none',
        strip.background = element_rect(color = "black", 
                                        fill = "white", 
                                        size = 1),
        strip.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text.x = element_text(size = 12, color = 'black'), #hjust = 1, angle = 45),
        axis.text.y = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  facet_wrap(~habvar)

gridExtra::grid.arrange(aa,bb,nrow = 2)

dev.off()

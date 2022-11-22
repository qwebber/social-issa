

# Load data
DT <- readRDS("output/location-data/5-rdm-locs-sri-NN.RDS")

DT[, .N, by = c("IDYr")]

DT$Use[DT$case_ == "TRUE"] <- 1
DT$Use[DT$case_ == "FALSE"] <- 0

I(log(sl_+1)) + 
  cos(ta_)

## correlations between step length and turn angle
cor.test(I(log(DT[iter == 1 & Year == "2017"]$step_id_+1)), cos(DT[iter == 1 & Year == "2017"]$ta_), 
         method = "spearman") 
cor.test(I(log(DT[iter == 1 & Year == "2018"]$step_id_+1)), cos(DT[iter == 1 & Year == "2018"]$ta_), 
         method = "spearman") 
cor.test(I(log(DT[iter == 1 & Year == "2019"]$step_id_+1)), cos(DT[iter == 1 & Year == "2019"]$ta_), 
         method = "spearman") 


png("graphics/FigS5.png", width = 5000, height = 3000, units = "px", res = 600)
ggplot(DT[iter == 1]) +
  geom_point(aes(I(log(sl_+1)), cos(ta_), 
             color = Year),
             alpha = 0.25) +
  ylab("cosine-transformed turn angle") +
  xlab("log-transformed step length") +
  scale_color_manual(values = c("#f1a340", "#91bfdb", "#5ab4ac")) +
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, color = 'black'),
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.text.x = element_text(size = 12, color = 'black'),
        plot.title=element_text(size = 12, hjust=0),
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        strip.text = element_text(size = 12)) +
  facet_wrap(~Year)
dev.off()
         

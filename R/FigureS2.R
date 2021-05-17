

## Generate Figure S2 ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: iSSA and daily trajectory randomization data
# Outputs: Figure S2

### Packages ----
libs <- c('data.table', 'ggplot2', 'gridExtra', 'lme4')

lapply(libs, require, character.only = TRUE)

### LOAD DAILY TRAJECTORY DATA ###
dynNets <- readRDS('output/rdmNets-100.RDS')
dynNets$year[dynNets$lagYear == "Year1"] <- "2017"
dynNets$year[dynNets$lagYear == "Year2"] <- "2018"
dynNets$year[dynNets$lagYear == "Year3"] <- "2019"


dynNets2 <- dynNets[, mean(Q), by = c("iteration", "year")]

rdmLow <- dynNets[, quantile(Q, 0.025), by = c("year")]
rdmUp<- dynNets[, quantile(Q, 0.975), by = c("year")]

obs <- readRDS('output/6-network-stats.RDS')
obs$year[obs$lagYear == "Year1"] <- "2017"
obs$year[obs$lagYear == "Year2"] <- "2018"
obs$year[obs$lagYear == "Year3"] <- "2019"

obs2 <- obs[, mean(Q), by = c("year")]


dynNets2$year[dynNets2$lagYear == "Year1"] <- "2017"
dynNets2$year[dynNets2$lagYear == "Year2"] <- "2018"
dynNets2$year[dynNets2$lagYear == "Year3"] <- "2019"


# Figure S2
png('graphics/FigS2.png', units = 'px', 
    width = 4000, height = 2000, res = 600)
ggplot() +
  geom_histogram(data = dynNets2, aes(V1), fill = "darkgrey", bins = 15) +
  geom_vline(data = obs2, aes(xintercept = V1), color = "red") +
  geom_vline(data = rdmLow,  aes(xintercept = V1), color = "black", lty = 2) +
  geom_vline(data = rdmUp, aes(xintercept = V1), color = "black", lty = 2) +
  #xlim(0, 0.70) +
  xlab("Modularity") +
  ylab("Frequency") +
  theme(legend.position = 'none', 
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        plot.title=element_text(size = 14, hjust=0),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        strip.text = element_text(size = 14)) +
  facet_wrap(~year)
dev.off()


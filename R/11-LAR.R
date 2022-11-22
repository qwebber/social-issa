

## Cleaned Locs - generate lagged association rates ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data + NN
# Outputs: LAR

### Packages ----
libs <- c('data.table', 'ggplot2', 'asnipe', 'spatsoc', 'plotrix', 'gridExtra')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
fogo <- readRDS("output/location-data/7-locs-mod.RDS")
fogo$Year <- as.factor(fogo$Year)

## remove 
fogo <- fogo[!is.na(season)]

fogo$IDmemb <- as.factor(paste(fogo$ANIMAL_ID, fogo$membership, sep = "_"))
a1 <- fogo[, .N, by = c("ANIMAL_ID", "membership", "Year")]
a1[, c("N") := NULL]
write.csv(a1, "output/community_membership.csv")

######################## 
######## 2017 ######### 
########################

fo17 <- get_gbi(fogo[Year == "2017"], group = 'group', id = 'ANIMAL_ID')
times17 <- data.table(time = fogo[Year == "2017"]$JDate,
                      group = fogo[Year == "2017"]$group)
days <- data.table(group = as.numeric(rownames(fo17)))
qq <- merge(times17, days, by = "group")
times17 <- unique(qq)
times17 <- times17$time

lra17 <- data.table(LRA(fo17, times = times17, timejump = 1, output_style = 2))
LRA17avg <- lra17[, meanLRA := mean(RATE), by = .(TIME)]
LRA17avg <- lra17[, seLRA := std.error(RATE), by = .(TIME)]
avg17 <- LRA17avg[, unique(meanLRA), by = c("TIME")]
se17 <- LRA17avg[, unique(seLRA), by = c("TIME")][,c("TIME") := NULL]
LRA17 <- cbind(avg17, se17)
colnames(LRA17) <- c("TIME", "meanLRA", "seLRA")
LRA17$Year <- "2017"

######################## 
######## 2018 ######### 
########################

fo18 <- get_gbi(fogo[Year == "2018"], group = 'group', id = 'ANIMAL_ID')
times18 <- data.table(time = fogo[Year == "2018"]$JDate,
                      group = fogo[Year == "2018"]$group)
days <- data.table(group = as.numeric(rownames(fo18)))
qq <- merge(times18, days, by = "group")
times18 <- unique(qq)
times18 <- times18$time

lra18 <- data.table(LRA(fo18, times = times18, timejump = 1, output_style = 2))
LRA18avg <- lra18[, meanLRA := mean(RATE), by = .(TIME)]
LRA18avg <- lra18[, seLRA := std.error(RATE), by = .(TIME)]
avg18 <- LRA18avg[, unique(meanLRA), by = c("TIME")]
se18 <- LRA18avg[, unique(seLRA), by = c("TIME")][,c("TIME") := NULL]
LRA18 <- cbind(avg18, se18)
colnames(LRA18) <- c("TIME", "meanLRA", "seLRA")
LRA18$Year <- "2018"

######################## 
######## 2019 ######### 
########################

fo19 <- get_gbi(fogo[Year == "2019"], group = 'group', id = 'ANIMAL_ID')
times19 <- data.table(time = fogo[Year == "2019"]$JDate,
                      group = fogo[Year == "2019"]$group)
days <- data.table(group = as.numeric(rownames(fo19)))
qq <- merge(times19, days, by = "group")
times19 <- unique(qq)
times19 <- times19$time

lra19 <- data.table(LRA(fo19, times = times19, timejump = 1, output_style = 2))
LRA19avg <- lra19[, meanLRA := mean(RATE), by = .(TIME)]
LRA19avg <- lra19[, seLRA := std.error(RATE), by = .(TIME)]
avg19 <- LRA19avg[, unique(meanLRA), by = c("TIME")]
se19 <- LRA19avg[, unique(seLRA), by = c("TIME")][,c("TIME") := NULL]
LRA19 <- cbind(avg19, se19)
colnames(LRA19) <- c("TIME", "meanLRA", "seLRA")
LRA19$Year <- "2019"

LAR <- rbind(LRA17, LRA18, LRA19)

png("graphics/Fig2.png", width = 3000, height = 3000,
    units = "px", res = 600)
ggplot(LAR, aes(TIME, meanLRA)) +
  geom_line(aes(color = as.factor(Year)), alpha = 0.5, size = 1) + 
  ylab("Lagged association rate") +
  xlab("Days") +
  ylim(0, 0.7) +
  geom_errorbar(aes(ymin = meanLRA - seLRA, 
                    ymax = meanLRA + seLRA, 
                color = as.factor(Year)),
                width=0.04, alpha = 0.5) + 
  scale_color_manual(values = c("#7570b3", "#d95f02", "#1b9e77")) +
  labs(color = "Year") +
  theme(legend.position = c(0.15,0.85),
        legend.title = element_text(size = 12),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        plot.title=element_text(size = 14, hjust=0),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) 
dev.off()




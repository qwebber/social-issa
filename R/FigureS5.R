

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

######################## 
######## 2017 ######### 
########################

fo17 <- get_gbi(fogo[Year == "2017"], group = 'group', id = 'IDmemb')
times17 <- data.table(time = fogo[Year == "2017"]$JDate,
                      group = fogo[Year == "2017"]$group)
days <- data.table(group = as.numeric(rownames(fo17)))
qq <- merge(times17, days, by = "group")
times17 <- unique(qq)
times17 <- times17$time

lra17 <- data.table(LRA(fo17, times = times17, timejump = 1, output_style = 2))
lra17[, c("ID", "membership1") := tstrsplit(ID, "_", fixed=TRUE)][, c("ASSOCIATE", "membership2") := tstrsplit(ASSOCIATE, "_", fixed=TRUE)]
lra17$memb <- paste(lra17$membership1, lra17$membership2, sep = "_")
LRA17avg <- lra17[, meanLRA := mean(RATE), by = .(TIME, memb)]
LRA17avg <- lra17[, seLRA := std.error(RATE), by = .(TIME, memb)]
avg17 <- LRA17avg[, unique(meanLRA), by = c("TIME", "memb")]
se17 <- LRA17avg[, unique(seLRA), by = c("TIME", "memb")][,c("TIME", "memb") := NULL]
LRA17tot <- cbind(avg17, se17)
colnames(LRA17tot) <- c("TIME", "memb", "meanLRA", "seLRA")

LRA17avg$memb[LRA17avg$memb == "2_1"] <- "Different community"
LRA17avg$memb[LRA17avg$memb == "3_1"] <- "Different community"
LRA17avg$memb[LRA17avg$memb == "3_2"] <- "Different community"
LRA17avg$memb[LRA17avg$memb == "1_2"] <- "Different community"
LRA17avg$memb[LRA17avg$memb == "1_3"] <- "Different community"
LRA17avg$memb[LRA17avg$memb == "2_3"] <- "Different community"
LRA17avg$memb[LRA17avg$memb == "1_1"] <- "Same community"
LRA17avg$memb[LRA17avg$memb == "2_2"] <- "Same community"


LRA17avg1 <- LRA17avg[, mean(meanLRA), by = c("TIME", "memb")]
LRA17se <- LRA17avg[, mean(seLRA), by = c("TIME", "memb")]
LRA17 <- cbind(LRA17avg1, LRA17se$V1)
colnames(LRA17) <- c("TIME", "memb", "avg", "se")

######################## 
######## 2018 ######### 
######################## 

fo18 <- get_gbi(fogo[Year == "2018"], group = 'group', id = 'IDmemb')
times18 <- data.table(time = fogo[Year == "2018"]$JDate,
                      group = fogo[Year == "2018"]$group)
days <- data.table(group = as.numeric(rownames(fo18)))
qq <- merge(times18, days, by = "group")
times18 <- unique(qq)
times18 <- times18$time

lra18 <- data.table(LRA(fo18, times = times18, timejump = 1, output_style = 2))
lra18[, c("ID", "membership1") := tstrsplit(ID, "_", fixed=TRUE)][, c("ASSOCIATE", "membership2") := tstrsplit(ASSOCIATE, "_", fixed=TRUE)]
lra18$memb <- paste(lra18$membership1, lra18$membership2, sep = "_")
LRA18avg <- lra18[, meanLRA := mean(RATE), by = .(TIME, memb)]
LRA18se <- lra18[, seLRA := std.error(RATE), by = .(TIME, memb)]
avg18 <- LRA18avg[, mean(meanLRA), by = c("TIME", "memb")]
se18 <- LRA18se[, mean(seLRA), by = c("TIME", "memb")][,c("TIME", "memb") := NULL]
LRA18tot <- cbind(avg18, se18)
colnames(LRA18tot) <- c("TIME", "memb", "meanLRA", "seLRA")

LRA18avg$memb[LRA18avg$memb == "2_1"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "3_1"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "4_1"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "5_1"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "6_1"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "7_1"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "8_1"] <- "Different community"

LRA18avg$memb[LRA18avg$memb == "3_2"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "4_2"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "5_2"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "6_2"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "7_2"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "8_2"] <- "Different community"

LRA18avg$memb[LRA18avg$memb == "4_3"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "5_3"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "6_3"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "7_3"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "8_3"] <- "Different community"

LRA18avg$memb[LRA18avg$memb == "5_4"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "6_4"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "7_4"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "8_4"] <- "Different community"

LRA18avg$memb[LRA18avg$memb == "6_5"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "7_5"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "8_5"] <- "Different community"

LRA18avg$memb[LRA18avg$memb == "7_6"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "8_6"] <- "Different community"

LRA18avg$memb[LRA18avg$memb == "8_7"] <- "Different community"

LRA18avg$memb[LRA18avg$memb == "1_2"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "1_3"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "1_4"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "1_5"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "1_6"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "1_7"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "1_8"] <- "Different community"

LRA18avg$memb[LRA18avg$memb == "2_3"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "2_4"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "2_5"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "2_6"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "2_7"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "2_8"] <- "Different community"

LRA18avg$memb[LRA18avg$memb == "3_4"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "3_5"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "3_6"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "3_7"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "3_8"] <- "Different community"

LRA18avg$memb[LRA18avg$memb == "4_5"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "4_6"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "4_7"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "4_8"] <- "Different community"

LRA18avg$memb[LRA18avg$memb == "5_6"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "5_7"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "5_8"] <- "Different community"

LRA18avg$memb[LRA18avg$memb == "6_7"] <- "Different community"
LRA18avg$memb[LRA18avg$memb == "6_8"] <- "Different community"

LRA18avg$memb[LRA18avg$memb == "7_8"] <- "Different community"

LRA18avg$memb[LRA18avg$memb == "1_1"] <- "Same community"
LRA18avg$memb[LRA18avg$memb == "2_2"] <- "Same community"
LRA18avg$memb[LRA18avg$memb == "3_3"] <- "Same community"
LRA18avg$memb[LRA18avg$memb == "4_4"] <- "Same community"


LRA18avg1 <- LRA18avg[, mean(meanLRA, na.rm = T), by = c("TIME", "memb")]
LRA18se <- LRA18avg[, mean(seLRA, na.rm = T), by = c("TIME", "memb")]
LRA18 <- cbind(LRA18avg1, LRA18se$V1)
colnames(LRA18) <- c("TIME", "memb", "avg", "se")

######################## 
######## 2019 ######### 
######################## 

fo19 <- get_gbi(fogo[Year == "2019"], group = 'group', id = 'IDmemb')
times19 <- data.table(time = fogo[Year == "2019"]$JDate,
                      group = fogo[Year == "2019"]$group)
days <- data.table(group = as.numeric(rownames(fo19)))
qq <- merge(times19, days, by = "group")
times19 <- unique(qq)
times19 <- times19$time

lra19 <- data.table(LRA(fo19, times = times19, timejump = 1, output_style = 2))
lra19[, c("ID", "membership1") := tstrsplit(ID, "_", fixed=TRUE)][, c("ASSOCIATE", "membership2") := tstrsplit(ASSOCIATE, "_", fixed=TRUE)]
lra19$memb <- paste(lra19$membership1, lra19$membership2, sep = "_")
LRA19avg <- lra19[, meanLRA := mean(RATE), by = .(TIME, memb)]
LRA19se <- lra19[, seLRA := std.error(RATE), by = .(TIME, memb)]
avg19 <- LRA19avg[, mean(meanLRA), by = c("TIME", "memb")]
se19 <- LRA19se[, mean(seLRA), by = c("TIME", "memb")][,c("TIME", "memb") := NULL]
LRA19tot <- cbind(avg19, se19)
colnames(LRA19tot) <- c("TIME", "memb", "meanLRA", "seLRA")

LRA19avg$memb[LRA19avg$memb == "2_1"] <- "Different community"
LRA19avg$memb[LRA19avg$memb == "1_2"] <- "Different community"
LRA19avg$memb[LRA19avg$memb == "1_1"] <- "Same community"
LRA19avg$memb[LRA19avg$memb == "2_2"] <- "Same community"

LRA19avg1 <- LRA19avg[, mean(meanLRA, na.rm = T), by = c("TIME", "memb")]
LRA19se <- LRA19avg[, mean(seLRA, na.rm = T), by = c("TIME", "memb")]
LRA19 <- cbind(LRA19avg1, LRA19se$V1)
colnames(LRA19) <- c("TIME", "memb", "avg", "se")



color = c("orange", "dodgerblue")

png("graphics/FigS5.png", width = 6000, height = 3000,
    units = "px", res = 600)
aa = ggplot(LRA17, aes(TIME, avg, fill = factor(memb))) +
  geom_line(alpha = 0.5, aes(color = factor(memb)), size = 1) + 
  ylab("Lagged association rate") +
  xlab("Days") +
  ggtitle('A) 2017') +
  ylim(0, 0.7) +
  geom_errorbar(aes(ymin = avg - se, 
                    ymax = avg + se, 
                    color = factor(memb)), 
                width=0.04, alpha = 0.5) + 
  scale_fill_manual(values = color) +
  scale_color_manual(values = color) +
  labs(color = "Annual winter social communities") +
  theme(legend.position = c(0.5,0.9),
        legend.title = element_text(size = 12),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        plot.title=element_text(size = 14, hjust=0),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour="black", size = 1, fill = NA),
        strip.text = element_text(size = 14)) 

bb = ggplot(LRA18, aes(TIME, avg, fill = factor(memb))) +
  geom_line(alpha = 0.5, aes(color = factor(memb)), size = 1) + 
  ylab("Lagged association rate") +
  xlab("Days") +
  ggtitle('B) 2018') +
  ylim(0, 0.7) +
  geom_errorbar(aes(ymin = avg - se, 
                    ymax = avg + se, 
                    color = factor(memb)), 
                width = 0.04, alpha = 0.5) + 
  scale_fill_manual(values = color) +
  scale_color_manual(values = color) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        plot.title=element_text(size = 14, hjust=0),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour="black", size = 1, fill = NA),
        strip.text = element_text(size = 14)) 

cc = ggplot(LRA19, aes(TIME, avg, fill = factor(memb))) +
  geom_line(alpha = 0.5, aes(color = factor(memb)), size = 1) + 
  ylab("Lagged association rate") +
  xlab("Days") +
  ggtitle('C) 2019') +
  ylim(0, 0.7) +
  geom_errorbar(aes(ymin = avg - se, 
                    ymax = avg + se, 
                    color = factor(memb)), 
                width = 0.04, alpha = 0.5) + 
  scale_fill_manual(values = color) +
  scale_color_manual(values = color) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        plot.title=element_text(size = 14, hjust=0),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour="black", size = 1, fill = NA),
        strip.text = element_text(size = 14)) 
grid.arrange(aa,bb,cc, ncol = 3, nrow = 1)
dev.off()
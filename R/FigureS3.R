

## Generate Figure 2 ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: UDOI vertices
# Outputs: Figure 1

library(ggplot2)
library(dplyr)

verticesFogo <- readRDS("output/vertices/verticesFogoCommunity.RDS")

verticesFogo$Year[verticesFogo$Year == "Year1"] <- "2017"
verticesFogo$Year[verticesFogo$Year == "Year2"] <- "2018"
verticesFogo$Year[verticesFogo$Year == "Year3"] <- "2019"

islands <- readRDS('output/vertices/islandsPoly.Rds')
islands <- st_as_sf(islands)

# Colors
watercol <- '#c3e2ec'
islandcol <- 'lightgrey'
coastcol <- '#82796a'
gridcol <- '#323232'


# Themes 
themeMap <- theme(legend.position = 'right',
                  panel.border = element_rect(size = 1, fill = NA),
                  panel.background = element_rect(fill = watercol), 
                  panel.grid = element_line(color = gridcol, size = 0.2),
                  strip.background = element_rect(colour="black", size = 1, fill = "white"),
                  strip.text = element_text(size = 12),
                  axis.text = element_blank(),
                  axis.title = element_blank())


verticesFogo2 <- st_as_sf(verticesFogo, coords=c("long","lat")) %>%
  st_set_crs(32621) %>%
  group_by(membership, group, Year) %>%
  summarise() %>%
  #ungroup() %>%  # Just in case
  st_convex_hull()

output <- verticesFogo2 %>% 
  st_intersection(islands)

png("graphics/FigS3.png", width = 5000, height = 3000, units = "px", res = 500)
ggplot() + 
    geom_sf(data = islands, fill = islandcol, size = 0.13, color = coastcol) + 
    geom_sf(data = output, aes(fill = membership,  group = group), alpha = 0.25) + 
    annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_minimal) +
    themeMap +
    theme(axis.text = element_text(size = 11, color = 'black')) +
    scale_y_continuous(label = function(x) sprintf('%.2f°N', x)) +
    scale_x_continuous(label = function(x) sprintf('%.1f°W', -1 * x)) +
  facet_wrap(~Year)
dev.off()

## plot polygons
png("graphics/FigS1.png", width = 3000, height = 3000, units = "px", res = 500)
ggplot(verticesFogo, aes(x = long, y = lat, 
                       fill = membership,  group = group)) +
  geom_polygon(alpha = 0.25, size = 0.5, color = "black") +
  coord_equal() +
  ylab("Northing") +
  xlab("Easting") +
  xlim(685000, 720000) +
  theme(axis.title = element_text(size = 14, color = 'black'),
        axis.text.y = element_text(size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black', angle = 45, hjust = 1),
        plot.title=element_text(size = 12, hjust=0),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        strip.text = element_text(size = 12)) +
  facet_wrap(~Year, ncol = 2, nrow = 2)
dev.off()

## plot polygons
png("output/Figure1/overlap.png", width = 3000, height = 3000, units = "px", res = 600)
ggplot(verticesFogo[Year == "2018"], aes(x = long, y = lat, 
                         fill = membership,  group = group)) +
  geom_polygon(alpha = 0.25, size = 0.5, color = "black") +
  coord_equal() +
  ylab("Northing") +
  xlab("Easting") +
  xlim(685000, 720000) +
  scale_color_viridis_d() +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=3))
dev.off()

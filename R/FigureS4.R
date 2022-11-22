
# Plot networks in geographic space ---------------------------------------
# Alec L. Robitaille
# 2022-10-20

# Packages ----------------------------------------------------------------
library(data.table)
library(igraph)
library(asnipe)
library(spatsoc)
library(ggnetwork)
library(sf)
library(ggplot2)
library(ggnetwork)
library(gridExtra)


# Data --------------------------------------------------------------------
# Read example data from spatsoc

### Input raw data ----
DT <- readRDS("output/location-data/1-clean-all.RDS")
DT <- DT[season == "winter"]
#setnames(DT, c("EASTING", "NORTHING"), c('x_proj', 'y_proj'))

# Fogo island polygon (from the study area figures repository)
# Source: Open Street Map
# Transform the CRS to match the coordinates, in this case 4326
fogo_polygon <- st_read('../study-area-figures/output/fogo-island-polygons.gpkg')
fogo_polygon_4326 <- st_transform(fogo_polygon, 4326)

islands <- readRDS('output/vertices/islandsPoly.Rds')
islands <- st_as_sf(islands)



# Make networks -----------------------------------------------------------
# If you already have network measures calculated, you can skip - 
#   we just need an edgelist with the dyadic network measure

# Here, we'll follow the example from the spatsoc vignette
# Details: https://docs.ropensci.org/spatsoc/articles/using-in-sna.html

# Cast datetime, spatiotemporal grouping, 
#  then get the group by individual matrix for 2016
DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
sub_DT1 <- DT[lagYear == "Year1"]
group_times(sub_DT1, datetime = 'datetime', threshold = '5 minutes')
group_pts(sub_DT1, threshold = 50, id = 'ANIMAL_ID',
          coords = c("EASTING", "NORTHING"), timegroup = 'timegroup')
gbi1 <- get_gbi(DT = sub_DT1, group = 'group', id = 'ANIMAL_ID')

sub_DT2 <- DT[lagYear == "Year2"]
group_times(sub_DT2, datetime = 'datetime', threshold = '5 minutes')
group_pts(sub_DT2, threshold = 50, id = 'ANIMAL_ID',
          coords = c("EASTING", "NORTHING"), timegroup = 'timegroup')
gbi2 <- get_gbi(DT = sub_DT2, group = 'group', id = 'ANIMAL_ID')

sub_DT3 <- DT[lagYear == "Year3"]
group_times(sub_DT3, datetime = 'datetime', threshold = '5 minutes')
group_pts(sub_DT3, threshold = 50, id = 'ANIMAL_ID',
          coords = c("EASTING", "NORTHING"), timegroup = 'timegroup')
gbi3 <- get_gbi(DT = sub_DT3, group = 'group', id = 'ANIMAL_ID')

# Convert the network into a graph, then extract the edgelist and eg weight
net1 <- get_network(gbi1, data_format = "GBI", association_index = "SRI")
g1 <- graph.adjacency(net1, 'undirected', diag = FALSE, weighted = TRUE)
edge_DT1 <- as.data.table(as_edgelist(g1))
setnames(edge_DT1, c('id_1', 'id_2'))
edge_DT1[, weight := E(g1)$weight]

net2 <- get_network(gbi2, data_format = "GBI", association_index = "SRI")
g2 <- graph.adjacency(net2, 'undirected', diag = FALSE, weighted = TRUE)
edge_DT2 <- as.data.table(as_edgelist(g2))
setnames(edge_DT2, c('id_1', 'id_2'))
edge_DT2[, weight := E(g2)$weight]

net3 <- get_network(gbi3, data_format = "GBI", association_index = "SRI")
g3 <- graph.adjacency(net3, 'undirected', diag = FALSE, weighted = TRUE)
edge_DT3 <- as.data.table(as_edgelist(g3))
setnames(edge_DT3, c('id_1', 'id_2'))
edge_DT3[, weight := E(g3)$weight]

# Add spatial -------------------------------------------------------------
# Decide where you want the nodes to be placed
# In this case, we'll take the mean XY for each id as the node location.
# First, get the centroids
centroid_DT1 <- sub_DT1[, .(mean_x = median(EASTING), mean_y = median(NORTHING)), by = ANIMAL_ID]
centroid_DT2 <- sub_DT2[, .(mean_x = median(EASTING), mean_y = median(NORTHING)), by = ANIMAL_ID]
centroid_DT3 <- sub_DT3[, .(mean_x = median(EASTING), mean_y = median(NORTHING)), by = ANIMAL_ID]

# Then merge them onto the edges
# Note we need to merge twice, to get the start and end for the edges
edge_DT1[centroid_DT1, c('mean_x_1', 'mean_y_1') := .(mean_x, mean_y), 
        on = .(id_1 == ANIMAL_ID)]
edge_DT1[centroid_DT1, c('mean_x_2', 'mean_y_2') := .(mean_x, mean_y), 
        on = .(id_2 == ANIMAL_ID)]

edge_DT2[centroid_DT2, c('mean_x_1', 'mean_y_1') := .(mean_x, mean_y), 
         on = .(id_1 == ANIMAL_ID)]
edge_DT2[centroid_DT2, c('mean_x_2', 'mean_y_2') := .(mean_x, mean_y), 
         on = .(id_2 == ANIMAL_ID)]

edge_DT3[centroid_DT3, c('mean_x_1', 'mean_y_1') := .(mean_x, mean_y), 
         on = .(id_1 == ANIMAL_ID)]
edge_DT3[centroid_DT3, c('mean_x_2', 'mean_y_2') := .(mean_x, mean_y), 
         on = .(id_2 == ANIMAL_ID)]


## merge with community membership
nets <- readRDS("output/6-network-stats.RDS")
setnames(nets, "ID", "ANIMAL_ID")

centroid_DT1 <- merge(centroid_DT1, nets[lagYear == "Year1"][,c("ANIMAL_ID", "membership")], by = "ANIMAL_ID")
centroid_DT2 <- merge(centroid_DT2, nets[lagYear == "Year2"][,c("ANIMAL_ID", "membership")], by = "ANIMAL_ID")
centroid_DT3 <- merge(centroid_DT3, nets[lagYear == "Year3"][,c("ANIMAL_ID", "membership")], by = "ANIMAL_ID")


# Colors
watercol <- '#c3e2ec'
islandcol <- 'lightgrey'
coastcol <- '#82796a'
gridcol <- '#323232'


themeMap <- theme(legend.position = 'none',
                  panel.border = element_rect(size = 1, fill = NA),
                  panel.background = element_rect(fill = watercol), 
                  panel.grid = element_line(color = gridcol, size = 0.2),
                  strip.background = element_rect(colour="black", size = 1, fill = "white"),
                  strip.text = element_text(size = 12))


# Plot --------------------------------------------------------------------
a1 <- ggplot() +
  geom_sf(data = islands, fill = '#e7dddd') +
  geom_edges(data = edge_DT1, aes(#size = weight,
    x = mean_x_1,
    y = mean_y_1,
    xend = mean_x_2,
    yend = mean_y_2), alpha = 0.5) +
  geom_nodes(data = centroid_DT1, 
             aes(x = mean_x,
                 y = mean_y, 
                 color = as.factor(membership)),
             size = 1) +
  ggtitle("A) 2017") +
  labs(x = 'Longitude', y = 'Latitude') +   
  themeMap

a2 <- ggplot() +
  geom_sf(data = islands, fill = '#e7dddd') +
  geom_edges(data = edge_DT2, 
             aes(
    x = mean_x_1,
    y = mean_y_1,
    xend = mean_x_2,
    yend = mean_y_2)) +
  geom_nodes(data = centroid_DT2, 
             aes(x = mean_x,
                 y = mean_y, 
                 color = as.factor(membership)),
             size = 1)  +
  ggtitle("B) 2018") +
  labs(x = 'Longitude', y = 'Latitude') +   
  themeMap

a3 <- ggplot() +
  geom_sf(data = islands, fill = '#e7dddd') +
  geom_edges(data = edge_DT3, 
             aes(
    x = mean_x_1,
    y = mean_y_1,
    xend = mean_x_2,
    yend = mean_y_2)) +
  geom_nodes(data = centroid_DT3, 
             aes(x = mean_x,
                 y = mean_y, 
                 color = as.factor(membership)),
             size = 1)  +
  ggtitle("C) 2019") +
  labs(x = 'Longitude', y = 'Latitude') +   
  themeMap

png("graphics/FigS4.png", width = 5000, height = 3000, units = "px", res = 500)
grid.arrange(a1, a2, a3, nrow = 1)
dev.off()

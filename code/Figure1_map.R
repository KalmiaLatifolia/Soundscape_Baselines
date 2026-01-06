
# Code for figure 1
# Baselines overview manuscript
# updated May 5th 2025
# Laura Berman

library(sf)
library(mapview)
library(tidyverse)
library(ggplot2)
library(rnaturalearth)

# https://ecoregions.appspot.com/
# https://developers.google.com/earth-engine/datasets/catalog/RESOLVE_ECOREGIONS_2017


################################################################################
# Get ecoregions map
# in google earth engine
################################################################################
# 
# var ecoRegions = ee.FeatureCollection('RESOLVE/ECOREGIONS/2017');
# 
# // patch updated colors
# var colorUpdates = [
#   {ECO_ID: 204, COLOR: '#B3493B'},
#   {ECO_ID: 245, COLOR: '#267400'},
#   {ECO_ID: 259, COLOR: '#004600'},
#   {ECO_ID: 286, COLOR: '#82F178'},
#   {ECO_ID: 316, COLOR: '#E600AA'},
#   {ECO_ID: 453, COLOR: '#5AA500'},
#   {ECO_ID: 317, COLOR: '#FDA87F'},
#   {ECO_ID: 763, COLOR: '#A93800'},
# ];
# 
# // loop over all other features and create a new style property for styling
# // later on
# var ecoRegions = ecoRegions.map(function(f) {
#   var color = f.get('COLOR');
#   return f.set({style: {color: color, width: 0}});
# });
# 
# // make styled features for the regions we need to update colors for,
# // then strip them from the main asset and merge in the new feature
# for (var i=0; i < colorUpdates.length; i++) {
#   colorUpdates[i].layer = ecoRegions
#   .filterMetadata('ECO_ID','equals',colorUpdates[i].ECO_ID)
#   .map(function(f) {
#     return f.set({style: {color: colorUpdates[i].COLOR, width: 0}});
#   });
#   
#   ecoRegions = ecoRegions
#   .filterMetadata('ECO_ID','not_equals',colorUpdates[i].ECO_ID)
#   .merge(colorUpdates[i].layer);
# }
# 
# // use style property to color shapes
# var imageRGB = ecoRegions.style({styleProperty: 'style'});
# 
# Map.setCenter(16, 49, 4);
# Map.addLayer(imageRGB, {}, 'RESOLVE/ECOREGIONS/2017');
# 
# 
# // Export the FeatureCollection to a KML file.
# Export.table.toDrive({
#   collection: ecoRegions,
#   description:'Ecoregions_2',
#   fileFormat: 'KML'
# });


################################################################################
# Prepare Data
################################################################################

# GitHub can't handle the Ecoregions shapefile (too large), so anyone on GitHub will have to download directly from the source with the code above ^

# set wd (use your own)---------------------------------------------------------
setwd("/Users/lauraberman/OneDrive - National University of Singapore/Documents/Wisconsin/Sound Forest Lab/Ecoregions")

# load ecoregions --------------------------------------------------------------
Ecoregions <- read_sf("ResolveEcoregions2017.shp")
ForestRegions <-subset(Ecoregions, grepl('Forest', BIOME_NAME))

# load world maps --------------------------------------------------------------
world_small <- ne_countries(scale='small', returnclass = 'sf')


################################################################################
# plot figure
################################################################################

ggplot() +
  geom_sf(data = world_small, color=NA) +
  geom_sf(data = ForestRegions, aes(fill=REALM), show.legend=TRUE, color=NA ) +
  scale_fill_manual(values = c("#D98324", "#FF9A9A", "#732255", "#27548A", "#9a7d0a", "#5b2c6f", "#9ACBD0")) +
  theme_minimal() +
  theme(legend.position = c(0.15, 0.4)) +
  guides(fill=guide_legend(title="Realm"), colour=guide_legend(title=""))

# save it ----------------------------------------------------------------------
ggsave("Figure1_map_202050516.PDF", height=7, width=15)



################################################################################
# Inset maps
################################################################################

# Packages
library(ggplot2)
library(sf)
library(raster)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(basemaps)


# WISCONSIN, USA ---------------------------------------------------------------


# mark sites
points_df <- data.frame(
  name = paste0("W", 1:6),
  lon = c(-89.90704676, -89.93848797, -89.83137264, -89.80117434, -89.53238905, -89.74750963),
  lat = c(43.37174736, 43.35882402, 43.39660723, 43.39508251, 43.47777985, 43.39200048)
)
points_sf <- st_as_sf(points_df, coords = c("lon","lat"), crs=4326,remove=FALSE)

# Create bounding box
ext <- st_as_sfc(st_bbox( c(
  xmin = -90.1, xmax = -89.4, 
  ymin = 43.2,    ymax = 43.6), 
  crs = 4326))


# Germany ----------------------------------------------------------------------


# mark sites
points_df <- data.frame(
  name = paste0("D", 1:5),
  lon = c(13.25033, 13.23153, 13.27662, 13.29535, 13.4074),
  lat = c(49.1009, 49.10139, 49.10505, 49.08456, 48.97583)
)
points_sf <- st_as_sf(points_df, coords = c("lon","lat"), crs=4326,remove=FALSE)

# Create bounding box
ext <- st_as_sfc(st_bbox( c(
  xmin = 13, xmax = 13.5, 
  ymin = 48.95,    ymax = 49.2), 
  crs = 4326))


# Ecuador ----------------------------------------------------------------------


# mark sites
points_df <- data.frame(
  name = paste0("E", 1:6),
  lon = c(-76.15847094, -76.1680306, -76.16144759, -76.15253577, -76.15120798, -76.14189748),
  lat = c(-0.626352081, -0.622623236, -0.637504435, -0.645339819, -0.654439859, -0.647766427))
points_sf <- st_as_sf(points_df, coords = c("lon","lat"), crs=4326,remove=FALSE)

# Create bounding box
ext <- st_as_sfc(st_bbox( c(
  xmin = -76.2, xmax = -76.1, 
  ymin = -0.68,    ymax = -0.6), 
  crs = 4326))


# Peru ----------------------------------------------------------------------


# mark sites
points_df <- data.frame(
  name = paste0("P", 1:6),
  lon = c(-71.40609202, -71.40484994, -71.43848974, -71.43096178, -71.42751104, -71.42605954),
  lat = c(-12.86868656, -12.85624692, -12.84383793, -12.84908506, -12.85892047, -12.86982163))
points_sf <- st_as_sf(points_df, coords = c("lon","lat"), crs=4326,remove=FALSE)

# Create bounding box
ext <- st_as_sfc(st_bbox( c(
  xmin = -71.48, xmax = -71.35, 
  ymin = -12.9,    ymax = -12.8), 
  crs = 4326))


# Gabon ----------------------------------------------------------------------


# mark sites
points_df <- data.frame(
  name = paste0("G", 1:6),
  lon = c(13.23575422, 13.24577163, 13.25243964, 13.24175956, 13.23158164, 13.22454552),
  lat = c(0.676958399, 0.67348049, 0.666384095, 0.663965784, 0.664278632, 0.671013075))
points_sf <- st_as_sf(points_df, coords = c("lon","lat"), crs=4326,remove=FALSE)

# Create bounding box
ext <- st_as_sfc(st_bbox( c(
  xmin = 13.2, xmax = 13.3, 
  ymin = 0.63,    ymax = 0.71), 
  crs = 4326))



# Sierra Leone -----------------------------------------------------------------


# mark sites
points_df <- data.frame(
  name = paste0("SL", 1:6),
  lon = c(-10.913293, -10.905513, -10.882183, -10.905516, -10.889954, -10.921076),
  lat = c(7.65075603, 7.63740098, 7.65075733, 7.6463062, 7.63739701, 7.64630049))
points_sf <- st_as_sf(points_df, coords = c("lon","lat"), crs=4326,remove=FALSE)

# Create bounding box
ext <- st_as_sfc(st_bbox( c(
  xmin = -10.95, xmax = -10.82, 
  ymin = 7.6,    ymax = 7.7), 
  crs = 4326))


# Singapore --------------------------------------------------------------------


# mark sites
points_df <- data.frame(
  name = paste0("SG", 1:4),
  lon = c(103.804549, 103.777492, 103.779266, 103.735308),
  lat = c(1.355488, 1.358419, 1.295069, 1.441586))
points_sf <- st_as_sf(points_df, coords = c("lon","lat"), crs=4326,remove=FALSE)

# Create bounding box
ext <- st_as_sfc(st_bbox( c(
  xmin = 104, xmax = 103.6, 
  ymin = 1.5,    ymax = 1.2), 
  crs = 4326))



# Brunei -----------------------------------------------------------------------


# mark sites
points_df <- data.frame(
  name = paste0("B", 1:2),
  lon = c(114.76777, 114.77122),
  lat = c(4.71972, 4.69333))
points_sf <- st_as_sf(points_df, coords = c("lon","lat"), crs=4326,remove=FALSE)

# Create bounding box
ext <- st_as_sfc(st_bbox( c(
  xmin = 114.7, xmax = 114.9, 
  ymin = 4.65,    ymax = 4.8), 
  crs = 4326))


# MAP IT -----------------------------------------------------------------------


# Transform to EPSG:3857 (Web Mercator)
ext_3857 <- st_transform(ext, 3857)
sites_3857 <- st_transform(points_sf, 3857)

# set default basemap settings
basemaps::set_defaults(ext = ext_3857)

#plot it
ggplot() +
  basemap_gglayer(ext_3857, map_service = "esri", map_type = "world_imagery", force=TRUE) +
  scale_fill_identity() + 
  geom_sf(data = sites_3857, color = "white", size = 3) +
  #geom_sf_text(data = sites_3857, aes(label = name), color = "white", size = 2.5, nudge_y = 1000) +
  annotation_scale(location = "bl", width_hint = 0.2, line_col = "white", text_col = "white") +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = st_bbox(ext_3857)[c("xmin", "xmax")],
           ylim = st_bbox(ext_3857)[c("ymin", "ymax")],
           expand = FALSE) +
  theme(legend.position = "none") +
  xlab("") +
  ylab("")

attr(basemap_gglayer(ext_3857, map_service = "esri", map_type = "world_imagery", force=TRUE), "attribution")

ggsave("SingMap.pdf", height=3, width=5)


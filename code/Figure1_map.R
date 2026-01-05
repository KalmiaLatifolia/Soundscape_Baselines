
# Code for figure 1
# Baselines overview manuscript
# updated 20250505 (May 5th 2025)
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





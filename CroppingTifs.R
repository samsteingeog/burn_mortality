
# load packages
library(raster)
library(sf)
library(ggplot2)

# ---------------------------------------------
# download dead biomass tif file
dead_biomass <- raster("data/DeadBiomassSmall3.tif") # you have to download this file onto your personal computer (not on git)

# download live biomass tif file
live_biomass <- raster("data/LivingBiomassSmall3.tif") # you have to download this file onto your personal computer (not on git)

# download tree density tif file
tree_density <- raster("data/TreeDensSmall3.tif") # you have to download this file onto your personal computer (not on git)

# download veg type tif file
veg_type <- raster("data/VegTypeResamp.tif") # you have to download this file onto your personal computer (not on git)

# read 110 fire shapefile
fire_polygons <- st_read('data/fire_data/110fires_sample.shp') # fire polygons

# store fire polygon geometry separately
fire_polygons_geom <- st_geometry(fire_polygons)
names(fire_polygons_geom) <- fire_polygons$OBJECTID

# ---------------------------------------------
# Dead Biomass
# Need to crop DEAD biomass file by each fire polygon

# match fire projection to dead biomass projection
fires_projected <- st_transform(fire_polygons_geom, crs(dead_biomass))

# crop dead biomass for all fires
dead_biomassList <- list() # initialize empty list
for (i in 1:length(fires_projected)) {
  extent <- as_Spatial(fires_projected[i])
  dead_biomassList[[i]] <- crop(x = dead_biomass, y = extent)
}

# name the dead_biomass list by Object IDs
names(dead_biomassList) <- fire_polygons$OBJECTID

# ---------------------------------------------
# Live Biomass
# Need to crop LIVE biomass file by each fire polygon

# match fire projection to live biomass projection
fires_projected <- st_transform(fire_polygons_geom, crs(live_biomass))

# check CRS's match
st_crs(fires_projected)
crs(live_biomass)

# crop live biomass for all fires
live_biomassList <- list() # initialize empty list
for (i in 1:length(fires_projected)) {
  extent <- as_Spatial(fires_projected[i])
  live_biomassList[[i]] <- crop(x = live_biomass, y = extent)
}

# name the live_biomass list by Object IDs
names(live_biomassList) <- fire_polygons$OBJECTID

# ---------------------------------------------
# Tree Density
# Need to crop TREE DENSITY file by each fire polygon

# match fire projection to tree density projection
fires_projected <- st_transform(fire_polygons_geom, crs(tree_density))

# check CRS's match
st_crs(fires_projected)
crs(tree_density)

# crop live biomass for all fires
tree_densityList <- list() # initialize empty list
for (i in 1:length(fires_projected)) {
  extent <- as_Spatial(fires_projected[i])
  tree_densityList[[i]] <- crop(x = tree_density, y = extent)
}

# name the live_biomass list by Object IDs
names(tree_densityList) <- fire_polygons$OBJECTID

# ---------------------------------------------
# Veg Type
# Need to crop VEG TYPE file by each fire polygon

# match fire projection to veg type projection
fires_projected <- st_transform(fire_polygons_geom, crs(veg_type))

# check CRS's match
st_crs(fires_projected)
crs(veg_type)

# crop live biomass for all fires
veg_typeList <- list() # initialize empty list
for (i in 1:length(fires_projected)) {
  extent <- as_Spatial(fires_projected[i])
  veg_typeList[[i]] <- crop(x = veg_type, y = extent)
}

# name the live_biomass list by Object IDs
names(veg_typeList) <- fire_polygons$OBJECTID




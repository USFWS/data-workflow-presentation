## Get refuge boundary ----

get_tetlin <- function(saveit = FALSE) {
  source("./docs/R/get_sites.R")
  tetlin <- get_refuge("Tetlin National Wildlife Refuge")
  if (saveit) sf::st_write(tetlin, "./docs/data/shapefile/tetlin.shp")
  tetlin
}

## Get rasters of covariates ----

# Required packages
library(FedData)
library(terra)
library(tidyverse)

# Distance to forest cover
nlcd <- terra::rast("./docs/data/raster/nlcd/nlcd.tif")
forest <- terra::segregate(nlcd, classes = 42)  # Extract the forest layer
forest <- terra::classify(forest, rcl = matrix(c(1,0,1,NA), nrow = 2, ncol = 2))  # Reclassify 0 as NA
terra::writeRaster(forest, "./docs/data/raster/nlcd/forest.tif", overwrite = TRUE)  # Save it and load for efficiency
forest <- terra::rast("./docs/data/raster/nlcd/forest.tif")
forest_distance <- terra::distance(forest)
forest_distance <- project(forest_distance, "EPSG: 4326")  # Reproject to WGS84
# forest_distance <- mask(crop(forest_distance, ext(tetlin)), tetlin)
names(forest_distance) <- "forest"
terra::writeRaster(forest_distance, "./docs/data/raster/nlcd/forest_distance.tif", overwrite = TRUE)  # Save it and load for efficiency

# Distance to water
nlcd <- terra::rast("./docs/data/raster/nlcd/nlcd.tif")
water <- terra::segregate(nlcd, classes = 11)  # Extract the forest layer
water <- terra::classify(water, rcl = matrix(c(1,0,1,NA), nrow = 2, ncol = 2))  # Reclassify 0 as NA
terra::writeRaster(water, "./docs/data/raster/nlcd/water.tif", overwrite = TRUE)  # Save it and load for efficiency
water1 <- terra::rast("./docs/data/raster/nlcd/water.tif")
water_distance <- terra::distance(water)
water_distance <- project(water_distance, "EPSG: 4326")  # Reproject to WGS84
# water_distance <- mask(crop(water_distance, ext(tetlin)), tetlin)
names(water_distance) <- "water"
terra::writeRaster(water_distance, "./docs/data/raster/nlcd/water_distance.tif", overwrite = TRUE)  # Save it and load for efficiency



## Get refuge boundary ----
source("./docs/R/functions.R")
tetlin <- get_refuge("Tetlin National Wildlife Refuge")
sf::st_write(tetlin, "./docs/data/shapefile/tetlin.shp")

## Create rasters of covariates ----

# Required packages
library(FedData)
library(terra)
library(tidyverse)

# Elevation
# ned <- FedData::get_ned(template = tetlin[,1], label = "tetlin")
# ned <- project(ned, "EPSG: 4326")  # Reproject to WGS84
# names(ned) <- "elevation"
# terra::writeRaster(ned, filename = "./docs/data/raster/ned/ned.tif", overwrite = TRUE)

# Distance to forest cover
nlcd <- terra::rast("./docs/data/raster/nlcd/nlcd.tif")
forest <- terra::segregate(nlcd, classes = 42)  # Extract the forest layer
forest <- terra::classify(forest, rcl = matrix(c(1,0,1,NA), nrow = 2, ncol = 2))  # Reclassify 0 as NA
terra::writeRaster(forest, "./docs/data/raster/nlcd/forest.tif", overwrite = TRUE)  # Save it and load for efficiency
forest <- terra::rast("./docs/data/raster/nlcd/forest.tif")
forest_distance <- terra::distance(forest)
forest_distance <- project(forest_distance, "EPSG: 4326")  # Reproject to WGS84
forest_distance <- mask(crop(forest_distance, ext(tetlin)), tetlin)
names(forest_distance) <- "forest"
terra::writeRaster(forest_distance, "./docs/data/raster/nlcd/forest_distance.tif", overwrite = TRUE)  # Save it and load for efficiency

# Distance to water
nlcd <- terra::rast("./docs/data/raster/nlcd/nlcd.tif")
water <- terra::segregate(nlcd, classes = 11)  # Extract the forest layer
water <- terra::classify(water, rcl = matrix(c(1,0,1,NA), nrow = 2, ncol = 2))  # Reclassify 0 as NA
terra::writeRaster(water, "./docs/data/raster/nlcd/water.tif", overwrite = TRUE)  # Save it and load for efficiency
water <- terra::rast("./docs/data/raster/nlcd/water.tif")
water_distance <- terra::distance(water)
water_distance <- project(water_distance, "EPSG: 4326")  # Reproject to WGS84
water_distance <- mask(crop(water_distance, ext(tetlin)), tetlin)
names(water_distance) <- "water"
terra::writeRaster(water_distance, "./docs/data/raster/nlcd/water_distance.tif", overwrite = TRUE)  # Save it and load for efficiency


## Generate a sample of sites ----
sites <- st_sample(tetlin, 50, type = "random", exact = TRUE)
sites <- st_as_sf(sites)

## Extract covariate data to sites ----

sites <- data.frame(sf::st_coordinates(sites),
                    # elevation = terra::extract(ned, sites)$elevation,
                    forest = terra::extract(forest, sites)$forest,
                    water = terra::extract(water, sites)$water)


## Create maps ----
source("./docs/R/functions.R")

## Load covariate data
# ned <- terra::rast("./docs/data/raster/ned/ned.tif")
tetlin <- sf::st_read("./docs/data/shapefile/tetlin.shp")
forest <- terra::rast("./docs/data/raster/nlcd/forest_distance.tif")
water <- terra::rast("./docs/data/raster/nlcd/water_distance.tif")

# Plot elevation data
plot_map(tetlin, forest, layer.name = "Distance to forest (m)",
         maxpixels = terra::ncell(forest))


##########################################################
##########################################################
## Create simulated data ----
# Required packages
library(unmarked)
library(tidyverse)
library(terra)

## Load covariate data
ned <- terra::rast("./docs/data/raster/ned/ned.tif")
tetlin <- sf::st_read("./docs/data/shapefile/tetlin.shp")
forest <- terra::rast("./docs/data/raster/nlcd/forest_distance.tif")
water <- terra::rast("./docs/data/raster/nlcd/water_distance.tif")

set.seed(123)

# Sites
M <- 50

# Sampling occasions
J <- 8

# Create a matrix of NAs
y <- matrix(NA, M, J)

# Create covariate data
site_covs <- sites %>% select(forest, water)

# Create unmarked data frame
umf <- unmarkedFrameOccu(y = y, siteCovs = site_covs)
sc <- scale(site_covs)
siteCovs(umf) <- sc

# Specify the parameter values
simulate(umf, model = occu, formula = ~forest ~water)  # returns template of values to fill in

# Define our coefficients
cf <- list(state = c(0, 0.4), det = c(0, 0.2))  # Detection increases with distance to forest, Occu increases with distance to water.

# Run simulation
dat <- simulate(umf, model = occu, formula = ~forest ~water, coefs = cf)
cbind(sites, out[[1]]@y) |> 
  rename_with(~str_c("y.", .), -(1:4)) |>  # rename obs columns
  write.csv(file = "./docs/data/csv/dat.csv")

## Fit a model to our simulated dataset
mod <- occu(~forest ~water, data = dat[[1]])


## Map species occurrence probability ----

# load covariate rasters
# ned <- terra::rast("./docs/data/raster/ned/ned.tif")
forest <- terra::rast("./docs/data/raster/nlcd/forest_distance.tif")
water <- terra::rast("./docs/data/raster/nlcd/water_distance.tif")

# Attributes of scales dataset
s <- rbind(attr(sc, "scaled:center"), attr(sc, "scaled:scale"))

# Scale the raster data and combine into a single raster
water.s <- (water - s[[1,2]]) / s[[2,2]]
forest.s <- (forest - s[[1,1]]) / s[[1,2]]
wf <- c(water.s, forest.s)  # Combine rasters

# Extract beta values for the state model (detection)
beta <- coef(mod, type = "state")

logit.psi <- beta[1] + beta[2] * water.s
psi <- exp(logit.psi) / (1 + exp(logit.psi))

# View map of predicted occurrences
source("./docs/R/functions.R")
plot_map(tetlin, psi, layer.name = "Probability of occurrence",
         maxpixels = terra::ncell(psi))

E.psi <- predict(fm.occu, type = "state", newdata = wf)

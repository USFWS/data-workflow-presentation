

sim_data <- function(n.occ = 8, saveit = FALSE) {  # Create a simulated unmarked data frame for a single season occupancy model
  library(unmarked)
  library(tidyverse)
  library(terra)
  
  ## Load covariate data
  tetlin <- sf::st_read("./docs/data/shapefile/tetlin.shp", quiet = TRUE)
  forest <- terra::rast("./docs/data/raster/nlcd/forest_distance.tif")
  water <- terra::rast("./docs/data/raster/nlcd/water_distance.tif")
  sites <- read.csv("./docs/data/csv/sites.csv")
  
  set.seed(123)
  
  # Sites
  M <- nrow(sites)
  
  # Sampling occasions
  J <- n.occ
  
  # Create a matrix of NAs
  y <- matrix(NA, M, J)
  
  # Create covariate data
  site_covs <- sites %>% select(forest, water)
  
  # Create unmarked data frame
  umf <- unmarkedFrameOccu(y = y, siteCovs = site_covs)
  sc <- scale(site_covs)
  siteCovs(umf) <- sc
  
  # Define our coefficients (detection increases with forest dist., occupancy (state) increases with water dist.)
  cf <- list(det = c(0, 0.5), state = c(0, 0.4, -0.4))  
  
  # Run simulation
  unmarked_df <- simulate(umf, model = occu, formula = ~forest ~ water + forest, coefs = cf)
  
  if (saveit) {
    save(unmarked_df, file = "./docs/data/rdata/unmarked_df.Rdata")
    dat_csv <- cbind(sites, unmarked_df[[1]]@y) |> 
      rename_with(~str_c("y.", .), -(1:4))  # rename obs columns
    write.csv(dat_csv, file = "./docs/data/csv/dat.csv")
  }
  
  unmarked_df
}



fit_model <- function(unmarked_df) {  # Fit a single season occupancy model
  library(unmarked)
  # load("./docs/data/rdata/unmarked_df.Rdata")
  mod <- unmarked::occu(formula = ~forest ~ water + forest, 
                        data = unmarked_df[[1]])
}


map_occ <- function(m = mod,  # Create a raster of species occurrence probability from an unmarked single season occupancy model
                    pred = FALSE, 
                    saveit = FALSE) {  
  library(terra)
  library(unmarked)
  library(tidyverse)
  
  # Load covariate rasters
  forest <- terra::rast("./docs/data/raster/nlcd/forest_distance.tif")
  water <- terra::rast("./docs/data/raster/nlcd/water_distance.tif")
  sites <- read.csv("./docs/data/csv/sites.csv")
  
  # Crop to refuge boundary
  forest <- mask(crop(forest, ext(tetlin)), tetlin)
  water <- mask(crop(water, ext(tetlin)), tetlin)
  
  # Scale covariates
  sc <- sites %>% 
    select(forest, water) %>% 
    scale()
  
  # Attributes of scales dataset
  s <- rbind(attr(sc, "scaled:center"), attr(sc, "scaled:scale"))
  
  # Scale the raster data and combine into a single raster
  water.s <- (water - s[[1,2]]) / s[[2,2]]
  forest.s <- (forest - s[[1,1]]) / s[[1,2]]
  
  if (pred) {
    # Generate a raster of predicted values from the model
    wf <- c(water.s, forest.s)  # Combine rasters
    psi <- unmarked::predict(mod, type = "state", newdata = wf)
  } else {
    # Predict from the beta values for the state model (detection)
    beta <- coef(mod, type = "state")
    logit.psi <- beta[1] + beta[2]*water.s + beta[3]*forest.s
    psi <- exp(logit.psi) / (1 + exp(logit.psi))
  }
  
  if (saveit) terra::writeRaster(psi, "./docs/data/raster/psi/psi.tif", overwrite = TRUE)
  
  psi
}

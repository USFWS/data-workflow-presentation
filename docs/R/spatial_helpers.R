get_refuge <- function(orgname = "Tetlin National Wildlife Refuge") {  # Get sf multipolygon of refuge boundary
  library(httr)
  library(sf)
  
  orgname <- toupper(orgname)
  
  message(paste("Downloading boundary layer for", orgname))
  
  url <- httr::parse_url("https://services.arcgis.com/QVENGdaPbd4LUkLV/arcgis/rest/services")
  url$path <- paste(url$path, "National_Wildlife_Refuge_System_Boundaries/FeatureServer/0/query", sep = "/")
  url$query <- list(where = paste("ORGNAME =", paste0("'",orgname,"'")),  # Arctic Refuge, in this case
                    outFields = "*",
                    returnGeometry = "true",
                    f = "pgeojson"
  )
  request <- httr::build_url(url)
  prop <- sf::st_read(request)
  
  message("Done.")
  
  return(prop)
}


get_sites <- function(n = 100, saveit = FALSE) {  # Generate a data frame of a sample of sites 
  library(sf)
  library(terra)
  
  # Generate sample
  sites <- sf::st_sample(tetlin, n, type = "random", exact = TRUE) |>
    sf::st_as_sf(quite = TRUE)
  
  # Extract covariate data to sites
  tetlin <- sf::st_read("./docs/data/shapefile/tetlin.shp", quiet = TRUE)
  forest <- terra::rast("./docs/data/raster/nlcd/forest_distance.tif")
  water <- terra::rast("./docs/data/raster/nlcd/water_distance.tif")
  
  # Extract covariate data at sites
  sites <- data.frame(sf::st_coordinates(sites),
                      forest = terra::extract(forest, sites)$forest,
                      water = terra::extract(water, sites)$water)
  
  # Save it
  if (saveit) write.csv(sites, "./docs/data/csv/sites.csv", row.names = FALSE)
  
  sites
}


crop_nlcd <- function(nlcd, refuge) {  # Crop NLCD spatraster to sf multipoloygon boundary (refuge)
  library(terra)
  library(magrittr)
  library(sf)
  library(dplyr)
  library(FedData)
  
  nlcd <- nlcd %>%
    terra::crop(., sf::st_transform(refuge, sf::st_crs(terra::crs(.))),
                snap = "out") %>%
    terra::as.factor()
  
  levels(nlcd) <- FedData::nlcd_colors() %>% as.data.frame()
  
  terra::coltab(nlcd) %<>% 
    magrittr::extract2(1) %>% 
    dplyr::filter(value %in% FedData::nlcd_colors()$ID)
  
  return(nlcd)
}

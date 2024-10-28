#' Get a FWS refuge boundary from the FWS AGOL feature server
#'
#' @param orgname the name of the refuge
#'
#' @return an sf multipolygon object 
#' 
#' @import httr
#' @import sf
#' 
#' @export
#'
#' @example get_refuge("Tetlin National Wildlife Refuge")
get_refuge <- function(orgname) {
  
  orgname <- toupper(orgname)
  
  message(paste("Downloading boundary layer for", orgname))
  
  url <- httr::parse_url("https://services.arcgis.com/QVENGdaPbd4LUkLV/arcgis/rest/services")
  url$path <- paste(url$path, "National_Wildlife_Refuge_System_Boundaries/FeatureServer/0/query", sep = "/")
  url$query <- list(where = paste("ORGNAME =", paste0("'",orgname,"'")),
                    returnGeometry = "true",
                    f = "pgeojson"
  )
  request <- httr::build_url(url)
  prop <- sf::st_read(request)
  
  message("Done.")
  
  return(prop)
}


#' Generate a random sample of points within a multipolygon and extract raster data
#'
#' @param refuge a sf multipolygon boundary of a refuge
#' @param n the sample size
#'
#' @return
#' 
#' @import sf
#' @import terra
#' 
#' @export
#'
#' @example get_sites(tetlin)
get_sites <- function(refuge,
                      n = 100) {
  # Generate sample
  sites <- sf::st_sample(refuge, n, type = "random", exact = TRUE) |>
    sf::st_as_sf(quite = TRUE)
  }
  
  
extract_cov <- function(sites, forest, water) {
  # # Import rasters
  # forest <- terra::rast("./docs/data/raster/nlcd/forest_distance.tif")
  # water <- terra::rast("./docs/data/raster/nlcd/water_distance.tif")
  
  # Extract raster data at sites
  sites <- data.frame(sf::st_coordinates(sites),
                      forest = terra::extract(forest, sites)$forest,
                      water = terra::extract(water, sites)$water)
  }


#'  Crop a spatraster (NLCD) to an sf multipoloygon (refuge boundary)
#'
#' @param nlcd a `spatraster` object (NLCD)
#' @param refuge an `sf` multipolygon (refuge boundary)
#'
#' @return a `spatraster` cropped to the refuge boundary
#' 
#' @import terra
#' @import magrittr
#' @import sf
#' @import dplyr
#' @import FedData
#' 
#' @export
#'
#' @example crop_nlcd(nlcd, tetlin)
crop_nlcd <- function(nlcd, refuge) {
  
  # Crop raster
  nlcd <- nlcd %>%
    terra::crop(., sf::st_transform(refuge, sf::st_crs(terra::crs(.))),
                snap = "out") %>%
    terra::as.factor()
  
  # Assign NLCD colors to the classes
  levels(nlcd) <- FedData::nlcd_colors() %>% as.data.frame()
  terra::coltab(nlcd) %<>% 
    magrittr::extract2(1) %>% 
    dplyr::filter(value %in% FedData::nlcd_colors()$ID)
  
  return(nlcd)
}




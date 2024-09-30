

# Get refuge boundary
get_refuge <- function(orgname){
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


# Crop NLCD data to a refuge boundary
crop_nlcd <- function(nlcd, refuge) {
  library(terra)
  library(magrittr)
  library(sf)
  library(dplyr)
  library(FedData)
  
  nlcd <- nlcd %>% terra::crop(., sf::st_transform(refuge, sf::st_crs(terra::crs(.))), 
                                  snap = "out")
  nlcd <- nlcd %>% terra::as.factor()
  levels(nlcd) <- FedData::nlcd_colors() %>% as.data.frame()
  terra::coltab(nlcd) %<>% magrittr::extract2(1) %>% dplyr::filter(value %in% FedData::nlcd_colors()$ID)
  return(nlcd)
}


# Create a map of the refuge and raster data
plot_map <- function(refuge, r, ...) {
  library(terra)
  library(sf)
  library(tidyverse)
  library(mapview)
  library(leaflet)
  
  mapviewOptions(basemaps = c(),
                 legend = TRUE,
                 homebutton = FALSE,
                 query.position = "topright",
                 query.digits = 2,
                 query.prefix = "",
                 legend.pos = "bottomright",
                 platform = "leaflet",
                 fgb = TRUE,
                 georaster = TRUE,
                 na.color = NA)
  
  if (inherits(r, "SpatRaster")) {
    r %>%
      as("Raster")
  }
  
  bounds <- refuge %>%
    sf::st_bbox() %>%
    as.list()
  
  mapview::mapview(r, legend = TRUE)@map %>%
    # leaflet::removeLayersControl() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      data = refuge,
      color = "black",
      fill = FALSE,
      weight = 2,
      options = list(pointerEvents = "none"),
      highlightOptions = list(sendToBack = TRUE)
    ) %>%
    leaflet::fitBounds(
      lng1 = bounds$xmin,
      lng2 = bounds$xmax,
      lat1 = bounds$ymin,
      lat2 = bounds$ymax
    ) %>%
    addMiniMap(position = "bottomright",
               width = 100,
               height = 100,
               zoomLevelFixed = FALSE,
               zoomLevelOffset = -7)
}

# A simpler mapping function
pal <- colorbrew
leaflet() %>%
  addTiles() %>%
  addRasterImage(psi, maxBytes = 5000000) %>%
  addPolygons(data = tetlin[,1], weight = 1) %>%
  addLegend(pal = pal, values = values(psi), title = "Estimated occurrence") %>%
  addMiniMap(position = "bottomright",
             width = 100,
             height = 100,
             zoomLevelFixed = TRUE,
             zoomLevelOffset = -7)

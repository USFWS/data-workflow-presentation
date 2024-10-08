create_map <- function(ras = psi, 
                       s = sites, 
                       r = tetlin,
                       hgt = NULL) {
  
  library(RColorBrewer)
  library(leaflet)
  library(tidyverse)
  library(terra)

  x <- c(round(terra::minmax(ras)[[1]],2), round(minmax(ras)[[2]],2))
  pal_rev <- colorNumeric(RColorBrewer::brewer.pal(5,"Spectral"), x, reverse = TRUE, na.color = "#00000000")
  pal <- leaflet::colorNumeric(RColorBrewer::brewer.pal(5,"Spectral"), x)
  
  # sites <- as.data.frame(sites)
  
  leaflet(height = hgt) %>% 
    leaflet::addTiles() %>%
    leaflet::addRasterImage(ras, colors = pal_rev, maxBytes = 10168580, opacity = 0.75, group = "psi") %>%
    leaflet::addCircleMarkers(data = s, lat = ~Y, lng = ~X, 
                     radius = 0.5, 
                     color = "black", 
                     group = "sites") %>%
    leaflet::addPolygons(data = r,
                         fill = FALSE,
                         color = "black",
                         group = "Tetlin",
                         weight = 0.5) %>%
    leaflet::addLayersControl(overlayGroups = c("psi", "sites", "Tetlin"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    leaflet::addLegend(pal = pal,
              values = x,
              title = "Psi",
              labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
    leaflet::addMiniMap(height = 100, width = 100) %>%
    leaflet::addScaleBar()
}


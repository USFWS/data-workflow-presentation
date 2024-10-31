#' Create a leaflet map of occupancy within a refuge
#'
#' @param ras a `terra::ras` raster of psi estimates
#' @param s  an `sf::st_point` of the sites surveyed
#' @param r an `sf` multipolygon of the refuge boundary
#' @para p whether to map the predicted value of psi ("Predicted") or SEs ("SE")
#' @param h the height of the leaflet map returned
#' @param w the width of the leaflet map returned
#'
#' @return a leaflet map
#'
#' @import RColorBrewer
#' @import leaflet
#' @import terra
#' 
#' @export
#'
#' @example 
#' \dontrun{
#' create_map(ras = psi, s = sites, r = tetlin, p = "Predicted", h = 650, w = 300)
#' }

create_map <- function(ras, 
                       s, 
                       r,
                       p = "Predicted",
                       h = NULL,
                       w = NULL) {
  if (p == "Predicted") {
    x <- c(round(minmax(ras)[[1,1]],2), 
           round(minmax(ras)[[2,1]],2))
    grp <- "Psi"
    ras <- ras$Predicted
  } else if (p == "SE") {
    x <- c(round(minmax(ras)[[1,2]],2), 
           round(minmax(ras)[[2,2]],2))
    grp <-"SE"
    ras <- ras$SE
  }
  
    
  pal_rev <- colorNumeric(RColorBrewer::brewer.pal(5, "Spectral"), 
                          x, 
                          reverse = TRUE, 
                          na.color = "#00000000")
  pal <- colorNumeric(RColorBrewer::brewer.pal(5, "Spectral"), 
                      x)
  
  leaflet(height = h, width = w) |> 
    addTiles() |>
    addRasterImage(ras, 
                   colors = pal_rev, 
                   maxBytes = 10168580, 
                   opacity = 0.75, 
                   group = grp) |>
    addCircleMarkers(data = s, lat = ~Y, lng = ~X, 
                     radius = 0.5, 
                     color = "black", 
                     group = "sites") |>
    addPolygons(data = r,
                fill = FALSE,
                color = "black",
                group = "Tetlin",
                weight = 0.5) |>
    addLayersControl(overlayGroups = c(grp, 
                                       "sites", 
                                       "Tetlin"),
                     options = layersControlOptions(collapsed = FALSE)) |>
    addLegend(pal = pal,
              values = x,
              title = grp,
              labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) |>
    addMiniMap(height = 100, 
               width = 100) |>
    addScaleBar()
}



base_map <- function(s, r, h = NULL, w = NULL) {
  leaflet(height = h, width = w) |> 
    addTiles() |>
    addCircleMarkers(data = s, lat = ~Y, lng = ~X, 
                     radius = 0.5, 
                     color = "black", 
                     group = "sites") |>
    addPolygons(data = r,
                fill = FALSE,
                color = "black",
                group = "Tetlin",
                weight = 0.5) |>
    addLayersControl(overlayGroups = c("sites", 
                                       "Tetlin"),
                     options = layersControlOptions(collapsed = FALSE)) |>
    addMiniMap(height = 100, 
               width = 100) |>
    addScaleBar()
}

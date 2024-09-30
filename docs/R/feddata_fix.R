function (template, label, year = 2021, dataset = "landcover", 
          landmass = "L48", extraction.dir = file.path(tempdir(), "FedData", 
                                                       "extractions", "nlcd", label), raster.options = c("COMPRESS=DEFLATE", 
                                                                                                         "ZLEVEL=9"), force.redo = FALSE) 
{
  extraction.dir <- normalizePath(paste0(extraction.dir, "/."), 
                                  mustWork = FALSE)
  tmp <- tempfile(fileext = ".tif")
  template %>% template_to_sf()
  dataset <- match.arg(dataset, choices = c("landcover", "impervious", 
                                            "canopy"))
  dataset <- switch(dataset, landcover = "Land_Cover", impervious = "Impervious", 
                    canopy = "Tree_Canopy")
  dir.create(extraction.dir, showWarnings = FALSE, recursive = TRUE)
  outfile <- paste0(extraction.dir, "/", label, "_NLCD_", dataset, 
                    "_", year, ".tif")
  if (file.exists(outfile) & !force.redo) {
    return(terra::rast(outfile))
  }
  src <- "wcs"
  if (src == "wcs") {
    if (dataset == "Tree_Canopy" & landmass == "L48") {
      coverage <- paste0("nlcd_tcc_conus_", year, "_v2021-4")
      source <- paste0("https://www.mrlc.gov/geoserver/mrlc_download/", 
                       coverage, "/wcs")
    }
    else {
      coverage <- paste0("NLCD_", year, "_", dataset, "_", 
                         landmass)
      source <- paste0("https://www.mrlc.gov/geoserver/mrlc_download/", 
                       coverage, "/wcs")
    }
    if (source %>% httr::GET(query = list(service = "WCS", 
                                          version = "2.0.1", request = "DescribeCoverage", 
                                          coverageid = coverage)) %>% httr::status_code() %>% 
        identical(200L) %>% magrittr::not()) {
      stop("No web coverage service at ", source, ". See available services at https://www.mrlc.gov/geoserver/ows?service=WCS&version=2.0.1&request=GetCapabilities")
    }
    template %<>% sf::st_transform("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") %>% 
      sf::st_bbox()
    axis_labels <- source %>% httr::GET(query = list(service = "WCS", 
                                                     version = "2.0.1", request = "DescribeCoverage", 
                                                     coverageid = coverage)) %>% httr::content(encoding = "UTF-8") %>% 
      xml2::as_list() %$% CoverageDescriptions %$% CoverageDescription$boundedBy$Envelope %>% 
      attr("axisLabels") %>% stringr::str_split(" ") %>% 
      unlist()
    source %>% httr::GET(query = list(service = "WCS", version = "2.0.1", 
                                      request = "GetCoverage", coverageid = coverage, subset = paste0(axis_labels[[1]], 
                                                                                                      "(", template["xmin"], ",", template["xmax"], 
                                                                                                      ")"), subset = paste0(axis_labels[[2]], "(", 
                                                                                                                            template["ymin"], ",", template["ymax"], ")")), 
                         httr::write_disk(path = tmp, overwrite = TRUE))
  }
  if (src == "cog") {
    source <- "https://storage.googleapis.com/feddata-r/nlcd/"
    file <- paste0(year, "_", dataset, "_", landmass, ".tif")
    path <- paste0(source, file)
    if (path %>% httr::HEAD() %>% httr::status_code() %>% 
        identical(200L) %>% magrittr::not()) {
      stop("NLCD data are not available for dataset '", 
           dataset, "', year '", year, "', and landmass '", 
           landmass, "'. Please see available datasets at https://www.mrlc.gov/data.")
    }
    template %<>% template_to_sf()
    out <- paste0("/vsicurl/", path) %>% terra::rast() %>% 
      terra::crop(., sf::st_transform(template, sf::st_crs(terra::crs(.))), 
                  snap = "out", filename = outfile, datatype = "INT1U", 
                  gdal = raster.options, overwrite = TRUE)
  }
  if (dataset == "Land_Cover") {
    out <- tmp %>% terra::rast() %>% terra::as.factor()
    levels(out) <- nlcd_colors() %>% as.data.frame()
    terra::coltab(out) %<>% magrittr::extract2(1) %>% dplyr::filter(value %in% 
                                                                      nlcd_colors()$ID)
    terra::writeRaster(x = out, filename = outfile, datatype = "INT1U", 
                       gdal = raster.options, overwrite = TRUE)
  }
  else {
    file.copy(from = tmp, to = outfile, overwrite = TRUE)
  }
  return(terra::rast(outfile))
}
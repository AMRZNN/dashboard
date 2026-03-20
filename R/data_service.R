data_service <- function(cfg) {
  
  trend <- reactiveFileReader(
    5000, NULL,
    filePath = cfg$paths$trend,
    readFunc = readr::read_csv
  )
  
  micro <- reactiveFileReader(
    5000, NULL,
    filePath = cfg$paths$micro,
    readFunc = readr::read_csv
  )
  
  regio <- reactive({
    readr::read_csv(cfg$paths$regio)
  })
  
  shape <- reactive({
    shp <- readRDS(cfg$paths$shape)
    
    # Transformeer naar WGS84 voor leaflet
    if (sf::st_crs(shp)$epsg != 4326) {
      shp <- sf::st_transform(shp, 4326)
    }
    shp
  })
  
  list(
    trend = trend,
    micro = micro,
    regio = regio,
    shape = shape
  )
}
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
    sf::st_read(cfg$paths$shape, quiet = TRUE)
  })
  
  list(
    trend = trend,
    micro = micro,
    regio = regio,
    shape = shape
  )
}
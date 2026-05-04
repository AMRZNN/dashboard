library(here)

data_service <- function(cfg) {
  
  # --- Hulpfunctie: CSV inlezen met foutafhandeling ---
  .safe_read_csv <- function(path) {
    tryCatch(
      readr::read_csv(path, show_col_types = FALSE),
      error = function(e) {
        warning("Kon CSV niet inlezen: ", path, "\n  Fout: ", conditionMessage(e))
        NULL
      }
    )
  }
  
  # --- trend & micro: live file watching (5 seconden interval) ---
  trend <- reactiveFileReader(
    5000, NULL,
    filePath = cfg$paths$trend,
    readFunc  = .safe_read_csv
  )
  
  micro <- reactiveFileReader(
    5000, NULL,
    filePath = cfg$paths$micro,
    readFunc  = .safe_read_csv
  )
  
  # --- regio: consistent met trend/micro als reactiveFileReader ---
  regio <- reactiveFileReader(
    5000, NULL,
    filePath = cfg$paths$regio,
    readFunc  = .safe_read_csv
  )
  
  # --- shape: eenmalig inlezen bij opstart, niet reactief ---
  # Provinciegrenzen veranderen nooit tijdens een sessie.
  shape <- tryCatch({
    shp <- readRDS(cfg$paths$shape)
    shp
  }, error = function(e) {
    warning("Kon shape-bestand niet inlezen: ", cfg$paths$shape,
            "\n  Fout: ", conditionMessage(e))
    NULL
  })
  
  list(
    trend = trend,
    micro = micro,
    regio = regio,
    shape = shape
  )
}

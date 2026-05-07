library(here)

load_config <- function(path = here("config.yml")) {
  
  if (!file.exists(path)) {
    stop("config.yml niet gevonden. Verwachte locatie: ", path)
  }
  
  cfg <- yaml::read_yaml(path)
  
  # Resolve bestandspaden via here(); URL's worden overgeslagen
  cfg$paths <- lapply(cfg$paths, function(p) {
    if (grepl("^https?://", p)) p else here(p)
  })
  
  cfg
}
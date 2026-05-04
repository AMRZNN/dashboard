library(here)

load_config <- function(path = here("config.yml")) {

  if (!file.exists(path)) {
    stop("config.yml niet gevonden. Verwachte locatie: ", path)
  }

  cfg <- yaml::read_yaml(path)

  # Resolve alle bestandspaden relatief aan de projectroot via here()
  cfg$paths <- lapply(cfg$paths, function(p) here(p))

  cfg
}
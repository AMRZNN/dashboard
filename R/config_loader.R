library(yaml)

load_config <- function(path = "config.yml") {
  
  if (!file.exists(path)) {
    stop("config.yml niet gevonden in root.")
  }
  
  yaml::read_yaml(path)
}
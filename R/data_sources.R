read_source <- function(src) {
  type <- src$type
  
  if (type == "csv") {
    return(read.csv(src$path, stringsAsFactors = FALSE, check.names = FALSE))
  }
  
  if (type == "rds") {
    return(readRDS(src$path))
  }
  
  if (type == "geojson") {
    # sf leest geojson
    return(sf::st_read(src$path, quiet = TRUE))
  }
  
  if (type == "db") {
    # verwacht: src$driver, src$host, src$dbname, src$query, ...
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      host = src$host, dbname = src$dbname, user = src$user, password = src$password
    )
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    return(DBI::dbGetQuery(con, src$query))
  }
  
  if (type == "api") {
    # verwacht: src$url
    resp <- httr2::request(src$url) |> httr2::req_perform()
    return(httr2::resp_body_json(resp, simplifyVector = TRUE))
  }
  
  stop("Onbekend source type: ", type)
}

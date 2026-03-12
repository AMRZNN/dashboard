normalize_trend <- function(df) {
  df |>
    dplyr::transmute(
      jaar = as.integer(jaar),
      incidentie = as.numeric(gsub(",", ".", incidentie))
    ) |>
    dplyr::filter(!is.na(jaar), !is.na(incidentie)) |>
    dplyr::arrange(jaar)
}

normalize_micro <- function(df) {
  df |>
    dplyr::transmute(
      jaar = as.integer(jaar),
      type = as.character(type),
      waarde = as.numeric(gsub(",", ".", waarde))
    ) |>
    dplyr::filter(!is.na(jaar), !is.na(type), !is.na(waarde))
}

normalize_kpi <- function(df) {
  # verwacht kolommen: key, title, value, trend, dir, accent
  df |>
    dplyr::transmute(
      key = as.character(key),
      title = as.character(title),
      value = as.character(value),
      trend = as.character(trend),
      dir = dplyr::if_else(dir %in% c("up","down"), dir, "up"),
      accent = dplyr::if_else(accent %in% c("blue","green","red"), accent, "blue")
    )
}

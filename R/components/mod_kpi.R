library(shiny)
library(dplyr)

# -----------------------------
# Sparkline SVG
# -----------------------------

spark_svg <- function(values,
                      cfg,
                      width = 220,
                      height = 26
) {
  
  line_col <- cfg$colors$trend_line
  
  v <- as.numeric(values)
  v <- v[is.finite(v)]
  if (length(v) < 2)
    return(tags$svg(width="100%", height="100%"))
  
  pad_x <- 2; pad_y <- 2
  
  x <- seq(0, 1, length.out = length(v))
  ymin <- min(v); ymax <- max(v)
  if (ymax == ymin) ymax <- ymin + 1
  
  sx <- function(t) pad_x + t * (width - 2 * pad_x)
  sy <- function(val) pad_y + (1 - (val - ymin)/(ymax - ymin)) *
    (height - 2 * pad_y)
  
  xs <- sx(x); ys <- sy(v)
  
  line_path <- paste0(
    "M ", sprintf("%.2f %.2f", xs[1], ys[1]),
    paste0(" L ", sprintf("%.2f %.2f", xs[-1], ys[-1]), collapse="")
  )
  
  tags$svg(
    viewBox = paste("0 0", width, height),
    preserveAspectRatio = "none",
    tags$path(
      d = line_path,
      fill = "none",
      stroke = line_col,
      `stroke-width` = 3,
      `stroke-linecap` = "round",
      `stroke-linejoin` = "round"
    )
  )
}

# -----------------------------
# KPI Tile
# -----------------------------

kpi_tile <- function(title, value, trend, dir = "up",
                     accent = "blue", spark_vals, cfg) {
  
  arrow <- ifelse(dir == "up", "▲", "▼")
  
  tags$div(
    class = paste("amr-kpi", accent),
    tags$div(class="kpi-title", title),
    tags$div(class="kpi-value", value),
    tags$div(class=paste("kpi-trend", dir),
             paste0(arrow," ",trend)),
    tags$div(class="kpi-spark",
             spark_svg(spark_vals, cfg))
  )
}

# -----------------------------
# UI
# -----------------------------

mod_kpi_ui <- function(id) {
  ns <- NS(id)
  
  column(
    width = 4,
    class = "amr-kpi-col",
    uiOutput(ns("kpi_grid"))
  )
}

# -----------------------------
# SERVER
# -----------------------------

mod_kpi_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    output$kpi_grid <- renderUI({
      
      df <- data$trend() %>% arrange(jaar)
      
      latest <- tail(df$incidentie, 1)
      previous <- tail(df$incidentie, 2)[1]
      change <- round((latest - previous)/previous * 100, 1)
      
      dir_main <- ifelse(change >= 0, "up", "down")
      accent_main <- ifelse(change >= 0, "green", "red")
      
      spark_vals <- tail(df$incidentie, 10)
      
      tags$div(
        class = "amr-kpi-grid",
        
        kpi_tile(
          "BRMO meldingen",
          round(latest,1),
          paste0(abs(change), "%"),
          dir = dir_main,
          accent = accent_main,
          spark_vals = spark_vals,
          cfg = cfg
        ),
        
        kpi_tile(
          "ESBL incidentie",
          "14,1",
          "7%",
          dir = "up",
          accent = "green",
          spark_vals = spark_vals,
          cfg = cfg
        ),
        
        kpi_tile(
          "MRSA incidentie",
          "2,7",
          "7%",
          dir = "up",
          accent = "green",
          spark_vals = spark_vals,
          cfg = cfg
        ),
        
        kpi_tile(
          "CPE incidentie",
          "0,8",
          "4%",
          dir = "down",
          accent = "red",
          spark_vals = spark_vals,
          cfg = cfg
        )
      )
    })
  })
}
library(shiny)
library(dplyr)

# -----------------------------
# Sparkline SVG (met gradient)
# -----------------------------
spark_svg <- function(values, 
                      width = 220, 
                      height = 60) {
  
  # 🔥 vaste trendkleur (consistent met hoofdgrafiek)
  base_col <- "#6EA6CF"
  
  v <- as.numeric(values)
  v <- v[is.finite(v)]
  if (length(v) < 2)
    return(tags$svg(width="100%", height="100%"))
  
  pad_x <- 2; pad_y <- 5
  
  x <- seq(0, 1, length.out = length(v))
  ymin <- min(v); ymax <- max(v)
  if (ymax == ymin) ymax <- ymin + 1
  
  sx <- function(t) pad_x + t * (width - 2 * pad_x)
  
  scale_factor <- 1.8
  sy <- function(val) {
    base_sy <- pad_y + (1 - (val - ymin)/(ymax - ymin)) * (height - 2 * pad_y)
    pad_y + (base_sy - pad_y) / scale_factor
  }
  
  xs <- sx(x); ys <- sy(v)
  
  # lijn pad
  line_path <- paste0(
    "M ", sprintf("%.2f %.2f", xs[1], ys[1]),
    paste0(" L ", sprintf("%.2f %.2f", xs[-1], ys[-1]), collapse="")
  )
  
  # area (voor gradient)
  area_path <- paste0(
    "M ", sprintf("%.2f %.2f", xs[1], height),
    " L ", sprintf("%.2f %.2f", xs[1], ys[1]),
    paste0(" L ", sprintf("%.2f %.2f", xs[-1], ys[-1]), collapse=""),
    " L ", sprintf("%.2f %.2f", tail(xs,1), height),
    " Z"
  )
  
  gradient_id <- paste0("grad_", sample(1e6,1))
  
  tags$svg(
    viewBox = paste("0 0", width, height),
    preserveAspectRatio = "none",
    
    tags$defs(
      tags$linearGradient(
        id = gradient_id,
        x1 = "0%", y1 = "0%",
        x2 = "0%", y2 = "100%",
        
        tags$stop(
          offset = "0%",
          `stop-color` = base_col,
          `stop-opacity` = "0.35"
        ),
        
        tags$stop(
          offset = "100%",
          `stop-color` = base_col,
          `stop-opacity` = "0"
        )
      )
    ),
    
    # gradient fill
    tags$path(
      d = area_path,
      fill = paste0("url(#", gradient_id, ")")
    ),
    
    # lijn
    tags$path(
      d = line_path,
      fill = "none",
      stroke = base_col,
      `stroke-width` = 2.5,
      `stroke-linecap` = "round",
      `stroke-linejoin` = "round"
    )
  )
}

# -----------------------------
# KPI Tile
# -----------------------------
kpi_tile <- function(title, value, trend, dir = "up",
                     accent = "blue", spark_vals) {
  
  arrow <- ifelse(dir == "up", "▲", "▼")
  
  tags$div(
    class = paste("amr-kpi", accent),
    tags$div(class="kpi-title", title),
    tags$div(class="kpi-value", value),
    tags$div(class=paste("kpi-trend", dir),
             paste0(arrow," ",trend)),
    tags$div(
      class="kpi-spark",
      spark_svg(spark_vals)
    )
  )
}

# -----------------------------
# UI
# -----------------------------
mod_kpi_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 5,
    class = "amr-kpi-box",
    uiOutput(ns("kpi_grid"))
  )
}

# -----------------------------
# SERVER
# -----------------------------
mod_kpi_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    output$kpi_grid <- renderUI({
      
      # --- Modus 1: per-categorie data (data$kpi aanwezig) ---
      if (!is.null(data$kpi)) {
        df <- data$kpi()
        req(!is.null(df), nrow(df) >= 2)
        
        make_kpi <- function(col, label) {
          vals    <- df[[col]]
          latest  <- tail(vals, 1)
          prev    <- tail(vals, 2)[1]
          change  <- if (!is.na(prev) && prev > 0)
            round((latest - prev) / prev * 100, 1)
          else NA
          dir     <- if (!is.na(change) && change >= 0) "up" else "down"
          accent  <- if (!is.na(change) && change >= 0) "red" else "green"
          trend_txt <- if (is.na(change)) "–" else paste0(abs(change), "%")
          spark   <- tail(vals, 10)
          kpi_tile(label, latest, trend_txt, dir = dir, accent = accent, spark_vals = spark)
        }
        
        return(tags$div(
          class = "amr-kpi-grid",
          make_kpi("ESBL", "ESBL"),
          make_kpi("MRSA", "MRSA"),
          make_kpi("VRE",  "VRE"),
          make_kpi("CPE",  "CPE")
        ))
      }
      
      # --- Modus 2: jaardata via data$trend (GGD standaard) ---
      df <- data$trend() %>% dplyr::arrange(jaar)
      
      latest   <- tail(df$incidentie, 1)
      previous <- tail(df$incidentie, 2)[1]
      change   <- round((latest - previous) / previous * 100, 1)
      dir_main    <- ifelse(change >= 0, "up", "down")
      accent_main <- ifelse(change >= 0, "red", "green")
      spark_vals  <- tail(df$incidentie, 10)
      
      tags$div(
        class = "amr-kpi-grid",
        kpi_tile("BRMO meldingen", round(latest, 1),
                 paste0(abs(change), "%"),
                 dir = dir_main, accent = accent_main, spark_vals = spark_vals),
        kpi_tile("ESBL incidentie", "14,1", "7%",
                 dir = "up",   accent = "red", spark_vals = spark_vals),
        kpi_tile("MRSA incidentie", "2,7",  "7%",
                 dir = "up",   accent = "red", spark_vals = spark_vals),
        kpi_tile("CPE incidentie",  "0,8",  "4%",
                 dir = "down", accent = "green",   spark_vals = spark_vals)
      )
    })
  })
}
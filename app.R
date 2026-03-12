# app.R — Clean AMR dashboard (external CSS/JS + KPI grid locked)
# ------------------------------------------------------------
# Required:
#   www/styles.css
#   www/scripts.js
#   www/logo_amr.png
# Data:
#   data/trend.csv  (jaar, incidentie)
#   data/micro.csv  (jaar, type, waarde)
#   data/kpi.csv    (optional)

library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(readr)
library(tidyr)

PATH_TREND <- "data/trend.csv"
PATH_MICRO <- "data/micro.csv"
PATH_KPI   <- "data/kpi.csv"

LANDelijk_GEM <- 12.0

AMR_LINE_BLUE <- "#6EA6CF"
AMR_AVG_GREY  <- "#B9C6D6"

MICRO_COLORS <- c(
  ESBL   = "#6BB6E8",
  MRSA   = "#79C38A",
  VRE    = "#C7DA6B",
  CPE    = "#E2C15F",
  Overig = "#C9D6E8"
)
MICRO_ORDER <- c("ESBL","MRSA","VRE","CPE","Overig")

# ---------- robust number parsing (handles mixed decimal separators) ----------
parse_mixed_number <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\u00A0", "", x)
  x <- gsub("\\s+", "", x)
  x <- gsub("[^0-9,\\.\\-]", "", x)
  x[x == ""] <- NA_character_
  
  vapply(x, function(s) {
    if (is.na(s)) return(NA_real_)
    has_comma <- grepl(",", s, fixed = TRUE)
    has_dot   <- grepl("\\.", s)
    
    if (has_comma && has_dot) {
      last_comma <- max(gregexpr(",", s, fixed = TRUE)[[1]])
      last_dot   <- max(gregexpr("\\.", s)[[1]])
      if (last_comma > last_dot) {
        s <- gsub("\\.", "", s)
        s <- sub(",", ".", s, fixed = TRUE)
      } else {
        s <- gsub(",", "", s, fixed = TRUE)
      }
    } else if (has_comma) {
      pos <- gregexpr(",", s, fixed = TRUE)[[1]]
      if (length(pos) > 1) {
        last <- max(pos)
        left <- gsub(",", "", substr(s, 1, last - 1), fixed = TRUE)
        right <- substr(s, last + 1, nchar(s))
        s <- paste0(left, ".", right)
      } else {
        s <- sub(",", ".", s, fixed = TRUE)
      }
    } else if (has_dot) {
      pos <- gregexpr("\\.", s)[[1]]
      if (length(pos) > 1) {
        last <- max(pos)
        left <- gsub("\\.", "", substr(s, 1, last - 1))
        right <- substr(s, last + 1, nchar(s))
        s <- paste0(left, ".", right)
      }
    }
    suppressWarnings(as.numeric(s))
  }, numeric(1))
}

# ---------- read CSV with , or ; ----------
.count_char <- function(s, ch) {
  m <- gregexpr(ch, s, fixed = TRUE)[[1]]
  if (length(m) == 1 && m[1] == -1) 0 else length(m)
}
read_any_csv <- function(path) {
  first <- readr::read_lines(path, n_max = 1)
  if (length(first) == 0) return(tibble())
  if (.count_char(first, ";") > .count_char(first, ",")) {
    readr::read_csv2(path, show_col_types = FALSE)
  } else {
    readr::read_csv(path, show_col_types = FALSE)
  }
}

# ---------- KPI sparkline SVG ----------
spark_svg <- function(values,
                      width = 220, height = 22,
                      line_col = AMR_LINE_BLUE,
                      fill_col = AMR_LINE_BLUE,
                      avg_y = NULL) {
  v <- as.numeric(values)
  v <- v[is.finite(v)]
  if (length(v) < 2) return(tags$svg(width="100%", height="100%"))
  
  pad_x <- 2; pad_y <- 2
  x <- seq(0, 1, length.out = length(v))
  ymin <- min(v); ymax <- max(v)
  if (ymax == ymin) ymax <- ymin + 1
  
  sx <- function(t) pad_x + t * (width - 2 * pad_x)
  sy <- function(val) pad_y + (1 - (val - ymin)/(ymax - ymin)) * (height - 2 * pad_y)
  
  xs <- sx(x); ys <- sy(v)
  
  line_path <- paste0(
    "M ", sprintf("%.2f %.2f", xs[1], ys[1]),
    paste0(" L ", sprintf("%.2f %.2f", xs[-1], ys[-1]), collapse = "")
  )
  y_base <- height - pad_y
  area_path <- paste0(
    line_path,
    " L ", sprintf("%.2f %.2f", xs[length(xs)], y_base),
    " L ", sprintf("%.2f %.2f", xs[1], y_base),
    " Z"
  )
  
  avg_line <- NULL
  if (!is.null(avg_y) && is.finite(avg_y)) {
    ay <- sy(avg_y)
    avg_line <- tags$line(
      x1 = pad_x, x2 = width - pad_x,
      y1 = ay, y2 = ay,
      class = "spark-avg",
      stroke = AMR_AVG_GREY,
      `stroke-width` = 2
    )
  }
  
  gid <- paste0("g", as.integer(stats::runif(1, 1e6, 9e6)))
  
  tags$svg(
    viewBox = paste("0 0", width, height),
    preserveAspectRatio = "none",
    tags$defs(
      tags$linearGradient(
        id = gid, x1 = "0", y1 = "0", x2 = "0", y2 = "1",
        tags$stop(offset="0%", `stop-color`=fill_col, `stop-opacity`="0.26"),
        tags$stop(offset="100%", `stop-color`=fill_col, `stop-opacity`="0.00")
      )
    ),
    tags$path(d = area_path, class="spark-area", fill=paste0("url(#", gid, ")"), stroke="none"),
    avg_line,
    tags$path(
      d = line_path,
      class="spark-line",
      fill="none",
      stroke=line_col,
      `stroke-width`=3,
      `stroke-linecap`="round",
      `stroke-linejoin`="round"
    )
  )
}

kpi_tile_spark <- function(title, value, trend_text,
                           trend_dir = c("up","down"),
                           accent = c("blue","green","red"),
                           spark_values = c(1,2,3,4,5),
                           spark_avg = NA_real_) {
  trend_dir <- match.arg(trend_dir)
  accent <- match.arg(accent)
  
  cls <- "amr-kpi"
  if (accent == "green") cls <- paste(cls, "green")
  if (accent == "red")   cls <- paste(cls, "red")
  
  tags$div(
    class = cls,
    tags$div(class = "kpi-title", title),
    tags$div(
      class = "kpi-row",
      tags$div(class = "kpi-value", value),
      tags$div(class = paste("kpi-trend", if (trend_dir == "up") "up" else "down"), trend_text)
    ),
    tags$div(class = "kpi-spark",
             spark_svg(spark_values, avg_y = spark_avg)
    )
  )
}

parse_spark_series <- function(s) {
  if (is.null(s) || length(s) == 0) return(numeric(0))
  s <- trimws(as.character(s))
  if (is.na(s) || s == "") return(numeric(0))
  parts <- strsplit(s, "\\|")[[1]]
  parse_mixed_number(parts)
}

default_kpi_from_trend <- function(trend_df) {
  td <- trend_df %>%
    mutate(jaar = as.integer(as.character(jaar)),
           incidentie = parse_mixed_number(incidentie)) %>%
    filter(!is.na(jaar), !is.na(incidentie)) %>%
    arrange(jaar)
  
  spark <- td$incidentie
  if (length(spark) > 10) spark <- tail(spark, 10)
  
  tibble(
    title = c("BRMO meldingen","ESBL incidentie","MRSA incidentie","CPE incidentie"),
    value = c("1.034","14,1","2,7","0,8"),
    trend = c("▲ 11%","▲ 7%","▲ 7%","▼ 4%"),
    dir = c("up","up","up","down"),
    accent = c("blue","green","green","red"),
    spark_values = list(spark, spark, spark, spark),
    avg = c(LANDelijk_GEM, NA_real_, NA_real_, NA_real_)
  )
}

# ---------- Plotly ----------
make_trend_plotly <- function(df, y_landelijk = NULL, n_bands = 50) {
  df <- df %>%
    mutate(
      jaar = as.integer(as.character(jaar)),
      incidentie = parse_mixed_number(incidentie)
    ) %>%
    filter(!is.na(jaar), !is.na(incidentie)) %>%
    arrange(jaar)
  
  line_col <- AMR_LINE_BLUE
  fill_rgb <- c(110, 166, 207)
  grid_col <- "#E9EEF5"
  axis_col <- "#6B7C93"
  avg_col  <- AMR_AVG_GREY
  label_col <- "#1F3B63"
  
  x <- df$jaar
  y <- df$incidentie
  
  p <- plot_ly()
  max_alpha <- 0.30
  for (i in seq_len(n_bands)) {
    f1 <- (i - 1) / n_bands
    f2 <- i / n_bands
    y1 <- y * f1
    y2 <- y * f2
    a <- (f2^2) * max_alpha
    
    p <- p %>% add_trace(
      x = c(x, rev(x)),
      y = c(y2, rev(y1)),
      type = "scatter", mode = "lines",
      line = list(color = "rgba(0,0,0,0)", width = 0),
      fill = "toself",
      fillcolor = sprintf("rgba(%d,%d,%d,%.4f)", fill_rgb[1], fill_rgb[2], fill_rgb[3], a),
      hoverinfo = "skip",
      showlegend = FALSE
    )
  }
  
  if (!is.null(y_landelijk)) {
    p <- p %>% add_trace(
      x = x, y = rep(as.numeric(y_landelijk), length(x)),
      type = "scatter", mode = "lines",
      line = list(color = avg_col, width = 2, dash = "dash"),
      hoverinfo = "skip",
      showlegend = FALSE
    )
  }
  
  p <- p %>% add_trace(
    x = x, y = y,
    type = "scatter", mode = "lines+markers",
    line = list(color = line_col, width = 3),
    marker = list(color = line_col, size = 8),
    hovertemplate = "%{x}<br>%{y:.1f}<extra></extra>",
    showlegend = FALSE
  )
  
  last_x <- tail(x, 1)
  last_y <- tail(y, 1)
  last_label <- gsub("\\.", ",", format(round(last_y, 1), nsmall = 1))
  
  p %>%
    add_annotations(
      x = last_x, y = last_y,
      text = paste0("<b>", last_label, "</b>"),
      showarrow = FALSE,
      yshift = 18,
      font = list(family = "Inter", size = 26, color = label_col)
    ) %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      margin = list(l = 48, r = 18, t = 8, b = 60),
      xaxis = list(
        title = "",
        tickfont = list(family = "Inter", color = axis_col),
        gridcolor = grid_col,
        zeroline = FALSE,
        tickmode = "linear",
        dtick = 1
      ),
      yaxis = list(
        title = "",
        tickfont = list(family = "Inter", color = axis_col),
        gridcolor = grid_col,
        zeroline = FALSE,
        rangemode = "tozero"
      )
    )
}

make_micro_plotly <- function(df) {
  df2 <- df %>%
    mutate(
      jaar = as.integer(as.character(jaar)),
      type = factor(as.character(type), levels = MICRO_ORDER),
      waarde = parse_mixed_number(waarde)
    ) %>%
    filter(!is.na(jaar), !is.na(type)) %>%
    mutate(waarde = tidyr::replace_na(waarde, 0)) %>%
    group_by(jaar, type) %>%
    summarise(waarde = sum(waarde, na.rm = TRUE), .groups = "drop") %>%
    arrange(jaar, type)
  
  jaar_levels <- sort(unique(df2$jaar))
  df2 <- df2 %>% mutate(jaar = factor(jaar, levels = as.character(jaar_levels)))
  
  grid_col <- "#E9EEF5"
  axis_col <- "#6B7C93"
  
  plot_ly(
    df2, x = ~jaar, y = ~waarde,
    color = ~type, colors = MICRO_COLORS,
    type = "bar",
    hovertemplate = "%{x}<br>%{fullData.name}: %{y}<extra></extra>"
  ) %>%
    layout(
      barmode = "stack",
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      margin = list(l = 48, r = 18, t = 8, b = 70),
      xaxis = list(
        title = "",
        type = "category",
        categoryorder = "array",
        categoryarray = as.character(jaar_levels),
        tickmode = "array",
        tickvals = as.character(jaar_levels),
        ticktext = as.character(jaar_levels),
        tickfont = list(family = "Inter", color = axis_col),
        gridcolor = grid_col,
        zeroline = FALSE
      ),
      yaxis = list(
        title = "",
        tickfont = list(family = "Inter", color = axis_col),
        gridcolor = grid_col,
        zeroline = FALSE,
        rangemode = "tozero"
      ),
      legend = list(orientation = "h", x = 0, y = -0.25, font = list(family = "Inter"))
    )
}

# ---------- head: external CSS/JS ----------
amr_head <- tags$head(
  tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
  tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=NA),
  tags$link(rel="stylesheet",
            href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"),
  tags$link(rel="stylesheet", href="styles.css"),
  tags$script(src="scripts.js")
)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "AMR", titleWidth = 0),
  dashboardSidebar(sidebarMenu(menuItem("x", tabName = "x"))),
  dashboardBody(
    amr_head,
    
    tags$div(
      id = "amr-scale-root",
      div(
        class = "amr-tabs",
        tabsetPanel(
          type = "pills",
          
          tabPanel("GGD",
                   fluidRow(
                     class = "amr-top-row",
                     
                     box(
                       width = 8,
                       class = "amr-fixed-box",
                       title = "Incidentie van BRMO meldingen",
                       tags$div(class = "amr-subtitle",
                                "Aantal meldingen per 100.000 inwoners, Noord-Nederland, 2015–2024"),
                       plotlyOutput("trend_plot", height = "100%")
                     ),
                     
                     column(
                       width = 4,
                       class = "amr-kpi-col",
                       uiOutput("kpi_grid")
                     )
                   ),
                   
                   fluidRow(
                     box(
                       width = 8,
                       title = "Belangrijkste BRMO micro-organismen",
                       tags$div(class = "amr-subtitle", uiOutput("micro_subtitle")),
                       plotlyOutput("micro_plot", height = "290px")
                     ),
                     box(
                       width = 4,
                       title = "BRMO incidentie per regio",
                       tags$div(class = "amr-subtitle", "Aantal meldingen per 100.000 inwoners, Noord-Nederland"),
                       tags$div(
                         style = "
                    height: 290px;
                    border-radius: 12px;
                    background: linear-gradient(135deg, rgba(47,111,163,.08), rgba(110,166,207,.06));
                    border: 1px dashed #D6DEE8;
                    display:flex;
                    align-items:center;
                    justify-content:center;
                    color:#6B7C93;
                    font-weight:800;",
                         "Kaart (regio) placeholder"
                       )
                     )
                   ),
                   
                   tags$div(
                     class = "amr-footer",
                     tags$div(
                       class = "left",
                       "© AMR Zorgnetwerk Noord-Nederland, 2024. Laatste update: april 2024. BRMO = bijzonder resistente micro-organismen."
                     ),
                     tags$div(
                       class = "right",
                       "Meldplichtig: ESBL, MRSA, VRE, CPE."
                     )
                   )
          ),
          
          tabPanel("Laboratoria", box(width = 12, title = "Laboratoria", "Placeholder")),
          tabPanel("Verpleeghuizen", box(width = 12, title = "Verpleeghuizen", "Placeholder")),
          tabPanel("Huisartsen", box(width = 12, title = "Huisartsen", "Placeholder")),
          tabPanel("Ziekenhuizen", box(width = 12, title = "Ziekenhuizen", "Placeholder"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  trend_data <- reactiveFileReader(
    intervalMillis = 5000, session = session,
    filePath = PATH_TREND, readFunc = read_any_csv
  )
  
  micro_data <- reactiveFileReader(
    intervalMillis = 5000, session = session,
    filePath = PATH_MICRO, readFunc = read_any_csv
  )
  
  kpi_data <- reactive({
    if (file.exists(PATH_KPI)) {
      k <- read_any_csv(PATH_KPI)
      k %>%
        mutate(
          title = as.character(title),
          value = as.character(value),
          trend = as.character(trend),
          dir = ifelse(as.character(dir) %in% c("up","down"), as.character(dir), "up"),
          accent = ifelse(as.character(accent) %in% c("blue","green","red"), as.character(accent), "blue"),
          spark_values = if ("spark" %in% names(.)) lapply(spark, parse_spark_series) else replicate(n(), numeric(0), simplify = FALSE),
          avg = if ("avg" %in% names(.)) parse_mixed_number(avg) else NA_real_
        )
    } else {
      default_kpi_from_trend(trend_data())
    }
  })
  
  output$trend_plot <- renderPlotly({
    make_trend_plotly(trend_data(), y_landelijk = LANDelijk_GEM, n_bands = 50)
  })
  
  output$micro_plot <- renderPlotly({
    make_micro_plotly(micro_data())
  })
  
  output$micro_subtitle <- renderUI({
    df <- micro_data()
    yrs <- sort(unique(as.integer(as.character(df$jaar))))
    if (length(yrs) == 0) return(HTML("Verdeeld naar micro-organismen, Noord-Nederland"))
    if (length(yrs) == 1) return(HTML(paste0("Verdeeld naar micro-organismen, Noord-Nederland (", yrs[1], ")")))
    HTML(paste0("Verdeeld naar micro-organismen, Noord-Nederland (", min(yrs), "–", max(yrs), ")"))
  })
  
  output$kpi_grid <- renderUI({
    k <- kpi_data()
    
    # Zorg altijd 4 KPI's
    if (nrow(k) < 4) {
      need <- 4 - nrow(k)
      k <- bind_rows(k, tibble(
        title = rep("—", need),
        value = rep("—", need),
        trend = rep("", need),
        dir = rep("up", need),
        accent = rep("blue", need),
        spark_values = replicate(need, numeric(0), simplify = FALSE),
        avg = rep(NA_real_, need)
      ))
    }
    k <- k[1:4, , drop = FALSE]
    
    tiles <- lapply(seq_len(4), function(i) {
      sv <- k$spark_values[[i]]
      if (length(sv) < 2) sv <- c(1, 1.02, 1.03, 1.05, 1.04, 1.07, 1.08, 1.10)
      
      kpi_tile_spark(
        title = k$title[i],
        value = k$value[i],
        trend_text = k$trend[i],
        trend_dir = k$dir[i],
        accent = k$accent[i],
        spark_values = sv,
        spark_avg = k$avg[i]
      )
    })
    
    tags$div(class = "amr-kpi-grid", tiles[[1]], tiles[[2]], tiles[[3]], tiles[[4]])
  })
}

shinyApp(ui, server)

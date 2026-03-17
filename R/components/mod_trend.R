library(shiny)
library(plotly)
library(leaflet)
library(dplyr)

mod_trend_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 8,
    class = "amr-fixed-box",
    title = "Incidentie van BRMO meldingen",
    
    tags$div(
      class = "amr-subtitle",
      "Aantal meldingen per 100.000 inwoners, Noord-Nederland, 2015–2024"
    ),
    
    fluidRow(
      column(9, plotlyOutput(ns("plot"), height="320px")),
      column(
        3,
        div(
          style="position:relative;",
          leafletOutput(ns("mini_map"), height="320px"),
          div(
            id = ns("map_year"),
            style="
              position:absolute;
              top:12px;
              right:12px;
              background:white;
              padding:6px 10px;
              border-radius:6px;
              font-weight:700;
              font-size:14px;
              box-shadow:0 2px 6px rgba(0,0,0,0.15);
            "
          )
        )
      )
    )
  )
}

mod_trend_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    # -----------------------------
    # TRENDGRAFIEK
    # -----------------------------
    
    output$plot <- renderPlotly({
      
      df <- data$trend() %>% arrange(jaar)
      
      x <- df$jaar
      y <- df$incidentie
      
      p <- plot_ly()
      
      # Gradient shading
      n_bands <- 50
      max_alpha <- 0.30
      
      for (i in seq_len(n_bands)) {
        
        f1 <- (i-1)/n_bands
        f2 <- i/n_bands
        
        y1 <- y * f1
        y2 <- y * f2
        
        alpha <- (f2^2) * max_alpha
        
        p <- p %>% add_trace(
          x = c(x, rev(x)),
          y = c(y2, rev(y1)),
          type = "scatter",
          mode = "lines",
          line = list(color="rgba(0,0,0,0)", width=0),
          fill = "toself",
          fillcolor = sprintf("rgba(110,166,207,%.4f)", alpha),
          hoverinfo = "skip",
          showlegend = FALSE
        )
      }
      
      # Landelijk gemiddelde
      p <- p %>% add_trace(
        x = x,
        y = rep(cfg$constants$landelijk_gemiddelde, length(x)),
        type = "scatter",
        mode = "lines",
        line = list(
          color = cfg$colors$landelijk_gem,
          width = 2,
          dash = "dash"
        ),
        hoverinfo = "skip",
        showlegend = FALSE
      )
      
      # Hoofdlijn
      p <- p %>% add_trace(
        x = x,
        y = y,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = cfg$colors$trend_line, width = 3),
        marker = list(color = cfg$colors$trend_line, size = 8),
        hovertemplate = "%{x}<br>%{y:.1f}<extra></extra>",
        showlegend = FALSE
      )
      
      # Laatste waarde annoteren
      last_x <- tail(x, 1)
      last_y <- tail(y, 1)
      
      last_label <- gsub("\\.", ",",
                         format(round(last_y,1), nsmall=1))
      
      p %>%
        add_annotations(
          x = last_x,
          y = last_y,
          text = paste0("<b>", last_label, "</b>"),
          showarrow = FALSE,
          yshift = 20,
          font = list(
            family = "Inter",
            size = 26,
            color = "#1F3B63"
          )
        ) %>%
        layout(
          paper_bgcolor="rgba(0,0,0,0)",
          plot_bgcolor="rgba(0,0,0,0)",
          margin=list(l=48,r=18,t=8,b=60),
          xaxis=list(title="",gridcolor="#E9EEF5"),
          yaxis=list(title="",gridcolor="#E9EEF5",
                     rangemode="tozero")
        )
    })
    
    # -----------------------------
    # MINI MAP — HEEL NEDERLAND
    # -----------------------------
    
    output$mini_map <- renderLeaflet({
      
      shp <- data$shape()
      dat <- data$regio()
      
      df_map <- shp %>%
        left_join(dat, by = c("name"="regio"))
      
      pal <- colorBin(
        palette = cfg$colors$map_bins,
        domain  = df_map$incidentie,
        bins    = 4,
        na.color = "#E5E9F0"
      )
      
      leaflet(options = leafletOptions(
        zoomControl = FALSE,
        dragging = FALSE,
        scrollWheelZoom = FALSE
      )) %>%
        addPolygons(
          data = df_map,
          fillColor = ~pal(incidentie),
          weight = 1,
          color = "white",
          fillOpacity = 0.9
        ) %>%
        htmlwidgets::onRender("
          function(el,x){ el.style.background = 'white'; }
        ")
    })
    
    # -----------------------------
    # JAARTAL IN MINI MAP
    # -----------------------------
    
    output$map_year <- renderUI({
      
      df <- data$trend() %>% arrange(jaar)
      last_year <- tail(df$jaar, 1)
      
      HTML(last_year)
    })
    
  })
}
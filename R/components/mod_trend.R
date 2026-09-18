library(here)

# =========================
# Helper: Nederlandse maandnamen
# =========================
.maand_nl <- function(datum, afkorting = FALSE) {
  vol  <- c("januari","februari","maart","april","mei","juni",
            "juli","augustus","september","oktober","november","december")
  kort <- c("jan","feb","mrt","apr","mei","jun",
            "jul","aug","sep","okt","nov","dec")
  tabel <- if (afkorting) kort else vol
  paste(tabel[as.integer(format(datum, "%m"))], format(datum, "%Y"))
}

# =========================
# UI
# =========================
mod_trend_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 7,
    class = "amr-trend-box",
    title = textOutput(ns("box_titel"), inline = TRUE),
    
    textOutput(ns("subtitel"), inline = FALSE) |> tagAppendAttributes(class = "amr-subtitle"),
    
    tags$div(
      class = "trend-inner",
      tags$div(class = "trend-plot-area",
               plotlyOutput(ns("plot"), width = "100%", height = "100%")),
      tags$div(class = "trend-map-area",
               tags$div(class = "trend-map-year", textOutput(ns("map_year"))),
               leafletOutput(ns("mini_map"), width = "100%", height = "100%"))
    )
  )
}

# =========================
# SERVER
# =========================
mod_trend_server <- function(id, data, cfg, eenheid = "per100k",
                             dataset = reactive({ "brmo" })) {
  moduleServer(id, function(input, output, session) {
    
    .d <- reactive({ if (is.reactive(data)) data() else data })
    
    output$box_titel <- renderText({
      ds <- if (is.reactive(dataset)) dataset() else "brmo"
      if (ds == "respiratoir") "Incidentie van respiratoire virussen"
      else "Incidentie van BRMO meldingen"
    })
    
    trend_df <- reactive({
      df <- .d()$trend()
      if ("regio" %in% names(df)) {
        df <- dplyr::filter(df, regio == "Noord-Nederland")
      }
      if ("datum" %in% names(df)) {
        dplyr::arrange(df, datum)
      } else {
        dplyr::arrange(df, jaar)
      }
    })
    
    output$subtitel <- renderText({
      df <- trend_df()
      req(nrow(df) > 0)
      
      eenheid_val <- if (is.function(eenheid) || is.reactive(eenheid)) eenheid() else eenheid
      eenheid_txt <- if (eenheid_val == "per100k") "per 100.000 inwoners" else "absoluut aantal"
      
      if ("datum" %in% names(df)) {
        eerste <- .maand_nl(min(df$datum, na.rm = TRUE), afkorting = TRUE)
        laatste <- .maand_nl(max(df$datum, na.rm = TRUE), afkorting = TRUE)
        paste0("Aantal meldingen (", eenheid_txt, "), Noord-Nederland, ", eerste, " \u2013 ", laatste)
      } else {
        eerste <- min(df$jaar, na.rm = TRUE)
        laatste <- max(df$jaar, na.rm = TRUE)
        paste0("Aantal meldingen (", eenheid_txt, "), Noord-Nederland, ", eerste, "\u2013", laatste)
      }
    })
    
    # -------------------------
    # TREND GRAFIEK via plotly
    # -------------------------
    output$plot <- renderPlotly({
      df <- trend_df()
      req(nrow(df) > 0)
      
      # Gebruik datum als x-as als die beschikbaar is, anders jaar
      gebruik_datum <- "datum" %in% names(df)
      
      if (gebruik_datum) {
        # Sorteer op datum en maak gesorteerde labels
        df <- dplyr::arrange(df, datum)
        df$x_as <- .maand_nl(df$datum, afkorting = TRUE)
      } else {
        df <- dplyr::arrange(df, jaar)
        df$x_as <- as.character(df$jaar)
      }
      
      x_label  <- if (gebruik_datum) "Maand" else "Jaar"
      last     <- df[nrow(df), ]
      # Bewaar volgorde voor plotly categoryorder
      x_volgorde <- df$x_as
      
      plotly::plot_ly(df, x = ~x_as, y = ~incidentie,
                      type = "scatter", mode = "lines+markers+text",
                      line  = list(color = "#6EA6CF", width = 2.5),
                      marker = list(color = "#6EA6CF", size = 7),
                      fill  = "tozeroy",
                      fillcolor = "rgba(110,166,207,0.15)",
                      text = c(rep("", nrow(df) - 1), as.character(last$incidentie)),
                      textposition = "middle right",
                      textfont = list(size = 13, color = "#1F3B63", family = "Inter"),
                      hovertemplate = paste0(x_label, ": %{x}<br>Meldingen: %{y}<extra></extra>")) |>
        plotly::layout(
          xaxis = list(
            tickfont = list(family = "Inter", size = 10, color = "#6B7C93"),
            showgrid = FALSE, zeroline = FALSE, title = "",
            tickangle = 0,
            type = if (gebruik_datum) "category" else "-",
            categoryorder = if (gebruik_datum) "array" else NULL,
            categoryarray = if (gebruik_datum) x_volgorde else NULL,
            nticks = 7
          ),
          yaxis = list(
            tickfont = list(family = "Inter", size = 11, color = "#6B7C93"),
            showgrid = TRUE,
            gridcolor = "#E9EEF5",
            zeroline = FALSE,
            title = ""
          ),
          margin  = list(t = 5, r = 20, b = 30, l = 35),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          showlegend = FALSE,
          font = list(family = "Inter")
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
    
    # -------------------------
    # JAARLABEL MINIMAP
    # -------------------------
    output$map_year <- renderText({
      df <- trend_df()
      req(nrow(df) > 0)
      
      if ("datum" %in% names(df)) {
        # Maanddata: toon "jan 2026" formaat
        .maand_nl(max(df$datum, na.rm = TRUE))
      } else {
        as.character(max(df$jaar, na.rm = TRUE))
      }
    })
    
    # -------------------------
    # MINIMAP via leaflet
    # -------------------------
    output$mini_map <- renderLeaflet({
      d <- .d()
      req(d$shape)
      
      regio_data <- tryCatch(d$regio(), error = function(e) NULL)
      req(!is.null(regio_data), nrow(regio_data) > 0)
      
      noord_nuts3 <- c(
        "Delfzijl en omgeving", "Oost-Groningen", "Overig Groningen",
        "Noord-Friesland", "Zuidoost-Friesland", "Zuidwest-Friesland",
        "Noord-Drenthe", "Zuidoost-Drenthe", "Zuidwest-Drenthe"
      )
      
      # Zorg dat de join-kolom bestaat — regio of nuts3
      if (!"regio" %in% names(regio_data) && "nuts3" %in% names(regio_data)) {
        regio_data <- dplyr::rename(regio_data, regio = nuts3)
      }
      
      regio_noord <- dplyr::filter(regio_data, regio %in% noord_nuts3)
      
      df <- d$shape |>
        dplyr::left_join(regio_noord, by = c("nuts3" = "regio")) |>
        sf::st_transform(4326)
      
      pal  <- leaflet::colorBin(cfg$colors$map_bins, df$incidentie,
                                bins = 4, na.color = "#D0D5DC")
      bbox <- sf::st_bbox(df)
      
      leaflet::leaflet(df,
                       options = leaflet::leafletOptions(
                         zoomControl        = FALSE,
                         scrollWheelZoom    = FALSE,
                         doubleClickZoom    = FALSE,
                         dragging           = FALSE,
                         touchZoom          = FALSE,
                         attributionControl = FALSE
                       )
      ) |>
        leaflet::addPolygons(
          fillColor    = ~pal(incidentie),
          fillOpacity  = 0.9,
          color        = "white",
          weight       = 1,
          smoothFactor = 1
        ) |>
        leaflet::fitBounds(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
    })
  })
}
library(here)

# =========================
# UI
# =========================
mod_micro_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 7,
    class = "amr-micro-box",
    title = textOutput(ns("micro_titel"), inline = TRUE),
    
    tags$div(class = "amr-subtitle",
             "Verdeeld naar type, Noord-Nederland"),
    
    tags$div(
      class = "amr-micro-plot-wrapper",
      plotlyOutput(ns("plot"), width = "100%", height = "100%")
    )
  )
}

# =========================
# SERVER
# =========================
mod_micro_server <- function(id, data, cfg,
                             dataset = reactive({ "brmo" })) {
  moduleServer(id, function(input, output, session) {
    
    .d <- reactive({ if (is.reactive(data)) data() else data })
    
    output$micro_titel <- renderText({
      ds <- if (is.reactive(dataset)) dataset() else "brmo"
      if (ds == "respiratoir") "Respiratoire virussen" else "Belangrijkste BRMO micro-organismen"
    })
    
    output$plot <- renderPlotly({
      d  <- .d()
      df <- d$micro()
      req(!is.null(df), nrow(df) > 0)
      
      ds <- if (is.reactive(dataset)) dataset() else "brmo"
      
      # Kleuren: BRMO = vaste config-kleuren, respiratoir = automatisch palet
      if (ds == "respiratoir") {
        alle_types <- unique(df$type)
        pal <- c("#6EA6CF","#95B9C7","#ACCCBB","#C2DEAF","#D1E6C9",
                 "#A8C5DA","#7FB3CC","#B8D4E8","#9EC9DC","#E8F0F7",
                 "#6B9EB8","#84B2C9","#F0F5FA","#D6E8F2")
        kleur <- setNames(pal[seq_along(alle_types)], alle_types)
        types <- alle_types
      } else {
        kleur <- unlist(cfg$colors$micro)
        types <- names(kleur)
      }
      
      # Bouw gestapelde staafgrafiek per type
      p <- plotly::plot_ly()
      for (type in types) {
        sub <- dplyr::filter(df, .data$type == !!type)
        p <- plotly::add_trace(p,
                               data = sub,
                               x = ~factor(jaar), y = ~waarde,
                               type = "bar", name = type,
                               marker = list(color = kleur[[type]]),
                               hovertemplate = paste0(type, ": %{y}<extra></extra>")
        )
      }
      
      p |>
        plotly::layout(
          barmode = "stack",
          xaxis = list(
            title = "",
            tickfont = list(family = "Inter", size = 11, color = "#6B7C93"),
            showgrid = FALSE
          ),
          yaxis = list(
            title = "",
            tickfont = list(family = "Inter", size = 11, color = "#6B7C93"),
            gridcolor = "#E9EEF5",
            zeroline = FALSE
          ),
          legend = list(
            orientation = "v",
            font = list(family = "Inter", size = 11),
            bgcolor = "rgba(0,0,0,0)"
          ),
          margin = list(t = 5, r = 10, b = 30, l = 35),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          font = list(family = "Inter")
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
  })
}
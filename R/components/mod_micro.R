library(here)

# =========================
# UI
# =========================
mod_micro_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 7,
    class = "amr-micro-box",
    title = "Belangrijkste BRMO micro-organismen",
    
    tags$div(class = "amr-subtitle",
             "Verdeeld naar micro-organismen, Noord-Nederland"),
    
    tags$div(
      class = "amr-micro-plot-wrapper",
      plotlyOutput(ns("plot"), width = "100%", height = "100%")
    )
  )
}

# =========================
# SERVER
# =========================
mod_micro_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    output$plot <- renderPlotly({
      df <- data$micro()
      req(!is.null(df), nrow(df) > 0)
      
      kleur <- unlist(cfg$colors$micro)
      types <- names(kleur)
      
      # Bouw gestapelde staafgrafiek per type
      p <- plotly::plot_ly()
      for (type in types) {
        sub <- dplyr::filter(df, type == !!type)
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
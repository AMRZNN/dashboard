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
      girafeOutput(ns("plot"), width = "100%", height = "100%")
    )
  )
}

# =========================
# SERVER
# =========================
mod_micro_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    output$plot <- renderGirafe({
      df <- data$micro()
      req(!is.null(df), nrow(df) > 0)
      
      kleur <- unlist(cfg$colors$micro)
      
      p <- ggplot(df, aes(x = factor(jaar), y = waarde,
                          fill = type,
                          tooltip = paste0(type, ": ", waarde),
                          data_id = paste0(jaar, type))) +
        geom_col_interactive(position = "stack", width = 0.7) +
        scale_fill_manual(values = kleur, name = NULL) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        theme_minimal() +
        theme(
          text            = element_text(family = "Inter"),
          axis.title      = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "right",
          legend.text     = element_text(size = 10),
          plot.margin     = margin(5, 5, 2, 5)
        )
      
      girafe(
        ggobj = p,
        width_svg  = 6,
        height_svg = 3,
        options = list(
          opts_sizing(rescale = TRUE),
          opts_toolbar(saveaspng = FALSE),
          opts_hover(css = "opacity:0.8;")
        )
      )
    })
  })
}
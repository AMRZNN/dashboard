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
    
    textOutput(ns("subtitel"), inline = FALSE) |> tagAppendAttributes(class = "amr-subtitle"),
    
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
                             dataset           = reactive({ "brmo" }),
                             alle_geselecteerd = reactive({ FALSE }),
                             gebied            = reactive({ "Noord-Nederland" })) {
  moduleServer(id, function(input, output, session) {
    
    .d <- reactive({ if (is.reactive(data)) data() else data })
    
    output$subtitel <- renderText({ paste0("Verdeeld naar type, ", gebied()) })
    
    output$micro_titel <- renderText({
      ds <- if (is.reactive(dataset)) dataset() else "brmo"
      if (ds == "respiratoir") "Respiratoire virussen" else "Belangrijkste BRMO micro-organismen"
    })
    
    output$plot <- renderPlotly({
      d    <- .d()
      df   <- d$micro()
      req(!is.null(df), nrow(df) > 0)
      
      ds       <- if (is.reactive(dataset))           dataset()           else "brmo"
      alle_sel <- if (is.reactive(alle_geselecteerd)) alle_geselecteerd() else FALSE
      
      # Zorg dat type altijd character is, verwijder lege/NA types
      df$type <- as.character(df$type)
      df <- df[!is.na(df$type) & df$type != "", ]
      req(nrow(df) > 0)
      
      # ----------------------------------------------------------
      # Als ALLES aangevinkt: top-4 afzonderlijk, rest = "Overig"
      # ----------------------------------------------------------
      if (isTRUE(alle_sel)) {
        # Rangschik op het LAATSTE beschikbare jaar (consistent met KPI)
        laatste_jaar <- max(df$jaar, na.rm = TRUE)
        totalen <- df |>
          dplyr::filter(jaar == laatste_jaar) |>
          dplyr::group_by(type) |>
          dplyr::summarise(totaal = sum(waarde, na.rm = TRUE), .groups = "drop") |>
          dplyr::arrange(dplyr::desc(totaal))
        
        top4 <- as.character(totalen$type[seq_len(min(4L, nrow(totalen)))])
        
        df <- df |>
          dplyr::mutate(type = dplyr::if_else(type %in% top4, type, "Overig")) |>
          dplyr::group_by(jaar, type) |>
          dplyr::summarise(waarde = sum(waarde, na.rm = TRUE), .groups = "drop") |>
          dplyr::mutate(type = as.character(type))
        
        heeft_overig  <- "Overig" %in% df$type
        type_volgorde <- c(top4, if (heeft_overig) "Overig")
      } else {
        top4          <- NULL
        heeft_overig  <- FALSE
        type_volgorde <- unique(as.character(df$type))
      }
      
      # ----------------------------------------------------------
      # Kleuren
      # ----------------------------------------------------------
      if (ds == "respiratoir") {
        pal <- c("#6EA6CF","#95B9C7","#ACCCBB","#C2DEAF","#D1E6C9",
                 "#A8C5DA","#7FB3CC","#B8D4E8","#9EC9DC","#E8F0F7",
                 "#6B9EB8","#84B2C9","#F0F5FA","#D6E8F2")
        kleur <- setNames(pal[seq_along(type_volgorde)], type_volgorde)
      } else {
        cfg_kleur <- unlist(cfg$colors$micro)
        kleur     <- setNames(
          vapply(type_volgorde, function(t) {
            v <- cfg_kleur[t]
            if (is.na(v)) "#C8D6E0" else v
          }, character(1)),
          type_volgorde
        )
      }
      # Overig altijd grijs
      if (heeft_overig) kleur[["Overig"]] <- "#C8D6E0"
      
      # ----------------------------------------------------------
      # Bouw gestapelde staafgrafiek
      # ----------------------------------------------------------
      p <- plotly::plot_ly()
      for (type in type_volgorde) {
        sub <- df[df$type == type, ]
        if (nrow(sub) == 0L) next
        p <- plotly::add_trace(
          p,
          data          = sub,
          x             = ~factor(jaar),
          y             = ~waarde,
          type          = "bar",
          name          = type,
          marker        = list(color = kleur[[type]]),
          hovertemplate = paste0(type, ": %{y}<extra></extra>")
        )
      }
      
      p |>
        plotly::layout(
          barmode = "stack",
          xaxis   = list(
            title    = "",
            tickfont = list(family = "Inter", size = 11, color = "#6B7C93"),
            showgrid = FALSE
          ),
          yaxis   = list(
            title     = "",
            tickfont  = list(family = "Inter", size = 11, color = "#6B7C93"),
            gridcolor = "#E9EEF5",
            zeroline  = FALSE
          ),
          legend = list(
            orientation = "v",
            font        = list(family = "Inter", size = 11),
            bgcolor     = "rgba(0,0,0,0)"
          ),
          margin        = list(t = 5, r = 10, b = 30, l = 35),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          font          = list(family = "Inter")
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
  })
}

mod_regio_map_ui <- function(id) {
  ns <- NS(id)
  
  box(
    width = 5,
    title = "BRMO incidentie per regio",
    # Gebruik een class om de plot heen voor CSS controle
    tags$div(
      style = "display: flex; flex-direction: column; align-items: center; justify-content: center;",
      tags$div(
        class="amr-subtitle",
        style = "width: 100%; text-align: left;",
        "Aantal meldingen per 100.000 inwoners"
      ),
      plotOutput(ns("map_plot"), height = "415px", width = "100%")
    )
  )
}



mod_regio_map_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    output$map_plot <- renderPlot({
      
      # Data ophalen en filteren
      req(!is.null(data$shape))
      shp <- dplyr::filter(data$shape,
                           provincie %in% cfg$geo$noord_provincies)

      dat <- dplyr::filter(data$regio(),
                           regio %in% cfg$geo$noord_provincies)
      
      # Koppelen van geometrie aan data
      df <- dplyr::left_join(shp, dat, by = c("provincie" = "regio"))
      
      ggplot(data = df) +
        # STAP 1: Voeg 'key_glyph = draw_key_dotplot' toe
        geom_sf(aes(fill = incidentie), 
                color = "white", 
                size = 0.4, 
                key_glyph = draw_key_dotplot) + 
        coord_sf(expand = FALSE) + 
        scale_fill_stepsn(
          colors = cfg$colors$map_bins,
          n.breaks = 4,
          na.value = "#E5E9F0",
          name = "Incidentie",
          guide = guide_legend(
            direction = "vertical",
            override.aes = list(
              shape = 21,          # Bolletje
              size = 16,            # Grootte
              stroke = 0.5,        # Dikte rand
              color = "white"      # Kleur rand
            )
          )
        ) +
        theme_void() +
        theme(
          text = element_text(family = "Inter"),
          legend.position = "right",
          legend.justification = "center",
          legend.key = element_blank(),
          
          # STAP 1: Geef de titel meer ruimte aan de onderkant en lijn hem links uit
          legend.title = element_text(
            size = 12, 
            face = "bold", 
            hjust = 0,         # Lijn titel links uit t.o.v. de bolletjes
            margin = margin(b = 10) 
          ),
          
          # STAP 2: Voeg extra ruimte toe aan de bovenkant van de hele plot (t = 20)
          # zodat de titel niet meer tegen het kader aanbotst
          plot.margin = margin(t = 30, r = 10, b = 10, l = 10, unit = "pt"),
          
          legend.background = element_blank()
        )
      
    }, res = 100, bg = "transparent") # 'res = 100' maakt de kaart scherper en vullender
  })
}
library(shiny)
library(shinydashboard)

app_ui <- function(cfg) {
  
  dashboardPage(
    skin = "blue",
    
    # ==============================
    # HEADER
    # ==============================
    dashboardHeader(
      title = NULL
    ),
    
    # ==============================
    # SIDEBAR (leeg, verplicht)
    # ==============================
    dashboardSidebar(
      sidebarMenu(id = "tabs")
    ),
    
    # ==============================
    # BODY
    # ==============================
    dashboardBody(
      includeCSS("www/styles.css"),
      includeScript("www/scripts.js"),
      
      tags$div(
        class = "amr-header-bar",
        
        tags$a(
          href = "https://www.amrznn.nl/",
          target = "_blank",
          class = "amr-logo-link",
          
          tags$img(src = "logo_amr.png", class = "amr-logo")
        ),
        
        tags$div(
          class = "amr-header-text",
          
          tags$div(
            "AMR Surveillance Noord-Nederland",
            class = "amr-header-title"
          ),
          
          tags$a(
            "Geaggregeerde trends, geen patiëntniveau-data",
            href = "https://www.amrznn.nl/",
            target = "_blank",
            class = "amr-header-link"
          )
        )
      ),
      
      
      tags$div(
        id = "amr-scale-root",
        
        tags$div(
          class = "amr-container",
          
          # ==========================
          # TABS
          # ==========================
          tags$ul(
            class = "nav nav-pills amr-tabs-nav",
            
            tags$li(class = "active",
                    tags$a(href = "#ggd", `data-toggle` = "tab", "GGD")),
            
            tags$li(tags$a(href = "#zh", `data-toggle` = "tab", "Ziekenhuizen")),
            tags$li(tags$a(href = "#lab", `data-toggle` = "tab", "Laboratoria")),
            tags$li(tags$a(href = "#ha", `data-toggle` = "tab", "Huisartsen")),
            tags$li(tags$a(href = "#vh", `data-toggle` = "tab", "Verpleeghuizen"))
          ),
          
          # ==========================
          # CONTENT
          # ==========================
          tags$div(
            class = "tab-content",
            
            tags$div(class = "tab-pane active", id = "ggd",
                     mod_tab_ggd_ui("ggd")),
            
            tags$div(class = "tab-pane", id = "zh",
                     mod_tab_ziekenhuizen_ui("zh")),
            
            tags$div(class = "tab-pane", id = "lab",
                     mod_tab_laboratoria_ui("lab")),
            
            tags$div(class = "tab-pane", id = "ha",
                     mod_tab_huisartsen_ui("ha")),
            
            tags$div(class = "tab-pane", id = "vh",
                     mod_tab_verpleeghuizen_ui("vh"))
          )
        )
      )
    ),
    
    # ==============================
    # HEAD
    # ==============================
    tags$head(
      tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
      tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=NA),
      tags$link(
        rel="stylesheet",
        href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"
      )
    )
  )
}

library(shiny)
library(shinydashboard)

app_ui <- function(cfg) {
  
  dashboardPage(
    skin = "blue",
    
    # ==============================
    # HEADER
    # ==============================
    dashboardHeader(
      titleWidth = 260,
      title = tags$div(
        class = "amr-logo-wrapper",
        tags$img(src = "logo_amr.png", class = "amr-logo")
      )
    ),
    
    # ==============================
    # SIDEBAR
    # ==============================
    dashboardSidebar(
      width = 260,
      sidebarMenu(
        id = "tabs",
        menuItem("GGD", tabName = "ggd", icon = icon("chart-line")),
        menuItem("Ziekenhuizen", tabName = "ziekenhuizen", icon = icon("hospital")),
        menuItem("Laboratoria", tabName = "laboratoria", icon = icon("flask")),
        menuItem("Huisartsen", tabName = "huisartsen", icon = icon("user-md")),
        menuItem("Verpleeghuizen", tabName = "verpleeghuizen", icon = icon("home"))
      )
    ),
    
    # ==============================
    # BODY
    # ==============================
    dashboardBody(
      includeCSS("www/styles.css"),
      includeScript("www/scripts.js"),

      
      # hoofdcontainer voor schaal/layout
      tags$div(
        id = "amr-scale-root",
        
        tabItems(
          
          tabItem(
            tabName = "ggd",
            mod_tab_ggd_ui("ggd")
          ),
          
          tabItem(
            tabName = "ziekenhuizen",
            mod_tab_ziekenhuizen_ui("zh")
          ),
          
          tabItem(
            tabName = "laboratoria",
            mod_tab_laboratoria_ui("lab")
          ),
          
          tabItem(
            tabName = "huisartsen",
            mod_tab_huisartsen_ui("ha")
          ),
          
          tabItem(
            tabName = "verpleeghuizen",
            mod_tab_verpleeghuizen_ui("vh")
          )
          
        )
      )
    ),
    
    # ==============================
    # HEAD (BELANGRIJK: hier plaatsen!)
    # ==============================
    tags$head(
      
      # Google fonts
      tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
      tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=NA),
      tags$link(
        rel="stylesheet",
        href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"
      )
      
    )
    
    
  )
}
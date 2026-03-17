app_ui <- function(cfg) {
  
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = cfg$app$title, titleWidth = 0),
    dashboardSidebar(sidebarMenu(menuItem("x", tabName = "x"))),
    
    dashboardBody(
      
      tags$head(
        tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
        tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=NA),
        tags$link(rel="stylesheet",
                  href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"),
        tags$link(rel="stylesheet", href="styles.css"),
        tags$script(src="scripts.js")
      ),
      
      tags$div(
        id = "amr-scale-root",
        div(
          class = "amr-tabs",
          tabsetPanel(
            type = "pills",
            
            tabPanel("GGD", mod_tab_ggd_ui("ggd")),
            tabPanel("Laboratoria", mod_tab_laboratoria_ui("lab")),
            tabPanel("Verpleeghuizen", mod_tab_verpleeghuizen_ui("vh")),
            tabPanel("Huisartsen", mod_tab_huisartsen_ui("ha")),
            tabPanel("Ziekenhuizen", mod_tab_ziekenhuizen_ui("zh"))
          )
        )
      )
    )
  )
}
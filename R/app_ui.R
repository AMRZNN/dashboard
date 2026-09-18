library(here)

app_ui <- function(cfg) {
  
  brmo_items <- c("ESBL" = "esbl", "MRSA" = "mrsa", "VRE" = "vre",
                  "CPE" = "cpe", "MRPA" = "mrpa", "FACRE" = "facre",
                  "CRE" = "cre", "FARA" = "fara", "CPA" = "cpa", "CA" = "ca")
  
  resp_items <- c("Influenza A", "Influenza B", "RSV", "SARS-CoV-2",
                  "Rhinovirus", "Rhinovirus/enterovirus", "Mycoplasma pneumoniae",
                  "Humaan metapneumovirus", "Adenovirus", "Enterovirus",
                  "Parainfluenzavirus type 1", "Parainfluenzavirus type 2",
                  "Parainfluenzavirus type 3", "Parainfluenzavirus type 4")
  
  sidebar_content <- tags$div(
    class = "amr-sidebar",
    
    tags$div(class = "amr-sidebar-section",
             tags$div(class = "amr-sidebar-label", "Dataset"),
             tags$div(class = "amr-sidebar-btngroup",
                      tags$a(id = "btn-dataset-brmo", class = "amr-sidebar-btn amr-dataset-btn active",
                             href = "#", onclick = "setDataset('brmo'); return false;", "BRMO"),
                      tags$a(id = "btn-dataset-respiratoir", class = "amr-sidebar-btn amr-dataset-btn",
                             href = "#", onclick = "setDataset('respiratoir'); return false;", "Respiratoir")
             ),
             tags$input(type = "text", id = "dataset", value = "brmo", style = "display:none;")
    ),
    
    tags$hr(class = "amr-sidebar-divider"),
    
    tags$div(class = "amr-sidebar-section",
             tags$div(class = "amr-sidebar-label", "Weergave"),
             tags$div(class = "amr-sidebar-btngroup",
                      tags$a(id = "btn-absoluut", class = "amr-sidebar-btn amr-weergave-btn active",
                             href = "#", onclick = "setWeergave('absoluut'); return false;", "Absoluut"),
                      tags$a(id = "btn-per100k", class = "amr-sidebar-btn amr-weergave-btn",
                             href = "#", onclick = "setWeergave('per100k'); return false;", "Per 100.000")
             ),
             tags$input(type = "text", id = "weergave", value = "absoluut", style = "display:none;")
    ),
    
    tags$hr(class = "amr-sidebar-divider"),
    
    tags$div(id = "sidebar-brmo-filter", class = "amr-sidebar-section",
             tags$div(class = "amr-sidebar-label",
                      "Pathogenen",
                      tags$a(href = "#", class = "amr-sidebar-selectall",
                             onclick = "setSelectAll('brmo', true); return false;", "Alle"),
                      tags$span(class = "amr-sidebar-selectall-sep", "/"),
                      tags$a(href = "#", class = "amr-sidebar-selectall",
                             onclick = "setSelectAll('brmo', false); return false;", "Geen")
             ),
             tags$div(class = "amr-sidebar-checks",
                      lapply(names(brmo_items), function(nm) {
                        val <- brmo_items[[nm]]
                        tags$label(class = "amr-check-label",
                                   tags$input(type = "checkbox", class = "amr-pathogeen-check",
                                              `data-group` = "brmo", `data-value` = val,
                                              checked = ifelse(val %in% c("esbl","mrsa","vre","cpe"), "checked", NA),
                                              onchange = "onPathogenChange()"),
                                   tags$span(nm)
                        )
                      })
             )
    ),
    
    tags$div(id = "sidebar-resp-filter", class = "amr-sidebar-section",
             style = "display:none;",
             tags$div(class = "amr-sidebar-label",
                      "Virussen",
                      tags$a(href = "#", class = "amr-sidebar-selectall",
                             onclick = "setSelectAll('resp', true); return false;", "Alle"),
                      tags$span(class = "amr-sidebar-selectall-sep", "/"),
                      tags$a(href = "#", class = "amr-sidebar-selectall",
                             onclick = "setSelectAll('resp', false); return false;", "Geen")
             ),
             tags$div(class = "amr-sidebar-checks",
                      lapply(resp_items, function(nm) {
                        tags$label(class = "amr-check-label",
                                   tags$input(type = "checkbox", class = "amr-pathogeen-check",
                                              `data-group` = "resp", `data-value` = nm,
                                              checked = ifelse(nm %in% c("Influenza A","Influenza B",
                                                                         "RSV","SARS-CoV-2"), "checked", NA),
                                              onchange = "onPathogenChange()"),
                                   tags$span(nm)
                        )
                      })
             )
    ),
    
    tags$hr(class = "amr-sidebar-divider"),
    tags$div(class = "amr-sidebar-section",
             tags$a(href = "https://github.com/AMRZNN", target = "_blank",
                    class = "amr-sidebar-info", "\u24d8 Info & bronnen")
    )
  )
  
  dashboardPage(
    skin = "blue",
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),   # AdminLTE sidebar volledig uit
    
    dashboardBody(
      includeCSS(here("www", "styles.css")),
      includeScript(here("www", "scripts.js")),
      
      tags$head(
        tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
        tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
        tags$link(rel = "stylesheet",
                  href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap")
      ),
      
      # Header — volledige breedte
      tags$div(class = "amr-header-bar",
               tags$a(href = "https://www.amrznn.nl/", target = "_blank",
                      tags$img(src = "logo_amr.png", class = "amr-logo")),
               tags$div(class = "amr-header-text",
                        tags$div("AMR Surveillance Noord-Nederland", class = "amr-header-title"),
                        tags$span("Geaggregeerde trends, geen patiëntniveau-data", class = "amr-header-sub")
               )
      ),
      
      # Tabbar — volledige breedte
      tags$div(class = "amr-tabbar",
               tags$ul(
                 class = "nav nav-pills amr-tabs-nav",
                 tags$li(class = "active",
                         tags$a(href = "#ggd", `data-toggle` = "tab", "GGD (FICTIEF)")),
                 tags$li(tags$a(href = "#zh",  `data-toggle` = "tab", "Ziekenhuizen")),
                 tags$li(tags$a(href = "#lab", `data-toggle` = "tab", "Laboratoria")),
                 tags$li(tags$a(href = "#ha",  `data-toggle` = "tab", "Huisartsen")),
                 tags$li(tags$a(href = "#vh",  `data-toggle` = "tab", "Verpleeghuizen"))
               ),
               tags$a(href = "https://github.com/AMRZNN", target = "_blank",
                      class = "amr-info-btn", "\u24d8 Info")
      ),
      
      # Hidden inputs
      tags$input(type = "text", id = "pathogenen_brmo",
                 value = "esbl,mrsa,vre,cpe", style = "display:none;"),
      tags$input(type = "text", id = "pathogenen_resp",
                 value = "Influenza A,Influenza B,RSV,SARS-CoV-2", style = "display:none;"),
      
      # Hoofd-layout: sidebar + content naast elkaar in één flex-container
      tags$div(
        class = "amr-layout",
        
        # Eigen sidebar (geen AdminLTE)
        sidebar_content,
        
        # Content
        tags$div(
          class = "amr-content",
          tags$div(
            class = "tab-content",
            tags$div(class = "tab-pane active", id = "ggd",  mod_tab_ggd_ui("ggd")),
            tags$div(class = "tab-pane",        id = "zh",   mod_tab_ziekenhuizen_ui("zh")),
            tags$div(class = "tab-pane",        id = "lab",  mod_tab_laboratoria_ui("lab")),
            tags$div(class = "tab-pane",        id = "ha",   mod_tab_huisartsen_ui("ha")),
            tags$div(class = "tab-pane",        id = "vh",   mod_tab_verpleeghuizen_ui("vh"))
          )
        )
      ),
      
      # Footer — volledige breedte
      tags$div(class = "amr-footer-bar",
               tags$span(
                 "\u00a9 AMR Zorgnetwerk Noord-Nederland. Bron: Certe laboratorium. ",
                 tags$a(href = "https://github.com/AMRZNN/dashboard_data/blob/main/TERMS_OF_USE.md",
                        target = "_blank", "Gebruiksvoorwaarden")
               ),
               tags$span("Meldplichtig: ESBL, MRSA, VRE, CPE.")
      )
    )
  )
}
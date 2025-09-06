## Module: mod_header.R
# Header module implemented in R (replaces www/componentes/header.js)
mod_header_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$header(class = "barra-logo",
                div(class = "logo-titulo",
                    tags$img(src = "images/Horizontal Gobierno de México_01.png", alt = "Logo del gobierno", style = "height: 80px;")),
                tags$nav(class = "header-nav",
                         tags$a(href = "#home", class = "nav-link", "Ocupación por Centro de Atención"),
                         tags$a(href = "#origen", class = "nav-link", "Origen y Destino")
                )
    )
  )
}

mod_header_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # no server-side logic required for static header (kept for future enhancements)
    invisible(NULL)
  })
}

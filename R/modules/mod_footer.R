## Module: mod_footer.R
# (Usa funciones de shiny: NS, tags, div, moduleServer)
# Footer module implemented in R (replaces www/componentes/footer.js)
mod_footer_ui <- function(id) {
  ns <- NS(id)
  tags$footer(id = ns("footer"), class = "footer-gob",
              div(class = "footer-logo",
                  tags$img(src = "images/2025_IMAGOTIPO_HORIZONTAL PARA FONDO OBSCURO.png",
                           alt = "Logo Gob",
                           style = "height:100px; max-width:100%; display:block; margin:0 auto;"))
  )
}

mod_footer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic required for static footer
    invisible(NULL)
  })
}

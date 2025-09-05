library(shiny)

ui <- navbarPage(
  "Tablero",
  tabPanel(
    "Ocupación por centro",
    htmlTemplate(
      "www/index.html",
      fecha_corte_placeholder = textOutput("fecha_corte_texto", inline = TRUE)
    )
  ),
  tabPanel(
    "Origen y Destino",
    htmlTemplate(
      "www/origen_destino.html",
      fecha_corte_placeholder = textOutput("fecha_corte_texto", inline = TRUE)
    )
  )
)
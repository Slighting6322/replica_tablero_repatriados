library(shiny)

ui <- htmlTemplate(
  "www/index.html",
  fecha_corte_placeholder = textOutput("fecha_corte_texto", inline = TRUE)
)
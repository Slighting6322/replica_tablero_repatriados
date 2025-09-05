library(shiny)

# Usamos htmlTemplate para conectar nuestro index.html con los outputs de R
# Solo necesitamos definir el marcador para la fecha.
ui <- htmlTemplate(
  "www/index.html",
  fecha_corte_placeholder = textOutput("fecha_corte_texto", inline = TRUE)
)
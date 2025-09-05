# server.R
# Lógica del servidor con el nuevo enfoque de htmlTemplate.

source("global.R", local = TRUE)

function(input, output, session) {

  # Renderizar el texto de la fecha de corte
  output$fecha_corte_texto <- renderText({
    fecha_corte_texto
  })

}

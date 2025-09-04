# server.R
# Lógica del servidor con el nuevo enfoque de htmlTemplate.

source("global.R", local = TRUE)

server <- function(input, output, session) {
  
  # Este output renderiza un texto de bienvenida.
  # Shiny lo insertará en el elemento HTML cuyo id sea "mensaje_prueba".
  output$mensaje_prueba <- renderText({
    "¡La conexión entre R y la plantilla HTML funciona!"
  })
  
  # Más adelante, aquí irán los servidores de tus módulos
  # y la lógica para renderizar gráficos y tablas en otros
  # elementos de la plantilla.
  
}

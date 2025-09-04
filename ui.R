# ui.R
# Este archivo ahora simplemente apunta a la plantilla HTML.

source("global.R", local = TRUE)

# Usamos htmlTemplate para decirle a Shiny que use nuestro archivo HTML como UI.
# Shiny buscará automáticamente los marcadores de posición como {{ headContent() }}
# y los IDs de los elementos para insertar los outputs.
ui <- htmlTemplate("www/index.html")


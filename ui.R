library(shiny)

# Intentar cargar módulos si existen (no falla si no están)
if (file.exists("R/modules/mod_home.R")) source("R/modules/mod_home.R", local = TRUE)
if (file.exists("R/modules/mod_origen.R")) source("R/modules/mod_origen.R", local = TRUE)
if (file.exists("R/modules/mod_fecha.R")) source("R/modules/mod_fecha.R", local = TRUE)

# Construir UI de secciones: si existe la función del módulo, usarla; si no, un placeholder
home_section_ui <- if (exists("mod_home_ui")) mod_home_ui("home1") else div(id = "home-module-placeholder")
origen_section_ui <- if (exists("mod_origen_ui")) mod_origen_ui("origen1") else div(id = "origen-module-placeholder")

ui <- htmlTemplate(
  "www/index.html",
  fecha_corte_placeholder = textOutput("fecha_corte_texto", inline = TRUE),
  home_section = home_section_ui,
  origen_section = origen_section_ui
)
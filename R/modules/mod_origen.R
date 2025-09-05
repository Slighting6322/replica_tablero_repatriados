## Module: mod_origen.R
# Minimal 'origen' page module. Keep layout similar to original origen_destino.html
mod_origen_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "origen-module",
        h2("Deportaciones desde EEUU"),
        p(class = "page-subtitle", "Fecha de corte: ", textOutput(ns("fecha"), inline = TRUE)),

        # Bloque de filtros / controles
        div(class = "origen-controls",
            fluidRow(
              column(4, selectInput(ns("filtro_estado"), "Estado / Estado de deportación", choices = NULL)),
              column(4, selectInput(ns("filtro_sexo"), "Sexo", choices = c("Todos" = "", "Hombre" = "M", "Mujer" = "F"))),
              column(4, uiOutput(ns("filtro_adicional")))
            )
        ),

        # Texto descriptivo que puede ser dinámico
        div(class = "origen-descripcion", uiOutput(ns("descripcion"))),

        # Placeholders para KPIs / tablas / mapas — se migrarán más tarde
        div(class = "origen-placeholders",
            div(class = "placeholder-kpis", id = ns("kpi_placeholder"), "[KPIs (no migrados)]"),
            div(class = "placeholder-tablas", id = ns("tabla_placeholder"), "[Tablas (no migradas)]"),
            div(class = "placeholder-mapa", id = ns("mapa_placeholder"), "[Mapa (no migrado)]")
        )
    )
  )
}

mod_origen_server <- function(id, fecha_reactivo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # iniciar submódulo fecha (renderiza textOutput ns("fecha"))
    if (exists("mod_fecha_server")) {
      try(mod_fecha_server("fecha", fecha_reactivo = fecha_reactivo), silent = TRUE)
    }

    # Poblar filtros desde repatriados_data si está disponible
    if (exists("repatriados_data")) {
      try({
        estados <- unique(repatriados_data$estado_origen)
        estados <- sort(na.omit(estados))
        updateSelectInput(session, "filtro_estado", choices = c("Todos" = "", estados), selected = "")
      }, silent = TRUE)
    }

    # Descripción dinámica simple
    output$descripcion <- renderUI({
      tagList(
        p("En esta sección puedes explorar las deportaciones por estado de origen. Selecciona filtros para acotar la vista."),
        p("Los KPIs, tablas y mapas siguen siendo mostrados como marcadores de posición hasta que los migremos.")
      )
    })

    # UI adicional para mostrar contextualmente (placeholder)
    output$filtro_adicional <- renderUI({
      # ejemplo: un control de rango de fechas si lo deseas luego
      dateRangeInput(ns("rango_fecha"), "Rango de fechas", start = NULL, end = NULL)
    })

    # Exponer reactivos simples que otros módulos o el server pueden usar
    origen_filtro_reactivo <- reactive({
      list(
        estado = if (nzchar(input$filtro_estado)) input$filtro_estado else NULL,
        sexo = if (nzchar(input$filtro_sexo)) input$filtro_sexo else NULL,
        rango = input$rango_fecha
      )
    })

    # exportar (si se quiere) añadiendo al entorno del session$ns() o retornando invisiblemente
    # aquí lo guardamos en session$userData para que otras partes de la app puedan accederlo
    session$userData$origen_filtros <- origen_filtro_reactivo

    # (Not implemented) Cuando se migren KPIs/tablas/mapas se usarán estos reactivos para filtrar
  })
}

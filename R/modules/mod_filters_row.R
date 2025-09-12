## Módulo contenedor: fila de 5 filtros (2 dropdown, number, buscar, refrescar)

## Note: helper modules (mod_filters_dropdown_*, mod_filters_number_*,
## mod_filters_buttons_*) are defined in separate files and are required at
## startup. Under the project's policy we don't keep local fallbacks here.

mod_filters_row_ui <- function(id) {
  ns <- shiny::NS(id)

  # Call helper UI functions directly (they must be defined and sourced at startup)
  ui1 <- mod_filters_dropdown_ui(ns("d1"), label = "Entidad:", choices = NULL)
  ui2 <- mod_filters_dropdown_ui(ns("d2"), label = "Municipio:", choices = NULL)
  ui3 <- mod_filters_number_ui(ns("n1"), label = "Capacidad:")
  ui4 <- mod_filters_buttons_ui(ns("btns"))

  shiny::div(class = "filters-row", ui1, ui2, ui3, ui4)
}

mod_filters_row_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
  # Call helper server functions directly; they must be available.
  sel1 <- mod_filters_dropdown_server("d1")
  sel2 <- mod_filters_dropdown_server("d2")
  num  <- mod_filters_number_server("n1")
  btns <- mod_filters_buttons_server("btns")

    # Exponer reactivas (consistent shape)
    list(sel1 = sel1, sel2 = sel2, num = num, buscar = btns$buscar, refrescar = btns$refrescar)
  })
}

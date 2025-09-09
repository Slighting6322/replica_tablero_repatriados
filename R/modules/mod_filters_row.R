## Módulo contenedor: fila de 5 filtros (2 dropdown, number, buscar, refrescar)

# The following prototypes are present only to satisfy static code checkers
# (they are inside an always-false branch so they never execute at runtime).
# They declare the signatures of helper module functions implemented in other
# files in this project so linters don't report "no visible global function".
if (FALSE) {
  mod_filters_dropdown_ui <- function(id, label = NULL, choices = NULL, placeholder = "Seleccionar...") {}
  mod_filters_dropdown_server <- function(id) {}
  mod_filters_number_ui <- function(id, label = NULL) {}
  mod_filters_number_server <- function(id) {}
  mod_filters_buttons_ui <- function(id) {}
  mod_filters_buttons_server <- function(id) {}
}

# If the helper modules are not yet loaded (static checks / partial sourcing),
# define minimal fallbacks so this file can be checked in isolation.
if (!exists("mod_filters_dropdown_ui", mode = "function")) {
  mod_filters_dropdown_ui <- function(id, label = NULL, choices = NULL, placeholder = "Seleccionar...") {
    ns <- shiny::NS(id)
    shiny::div(class = "filter-dropdown",
      if (!is.null(label)) shiny::div(class = "filter-label", label),
      shiny::selectInput(ns("select"), label = NULL, choices = if (is.null(choices)) list(placeholder) else choices, selected = NULL, width = "100%")
    )
  }
}
if (!exists("mod_filters_dropdown_server", mode = "function")) {
  mod_filters_dropdown_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      shiny::reactive({ input$select })
    })
  }
}
if (!exists("mod_filters_number_ui", mode = "function")) {
  mod_filters_number_ui <- function(id, label = NULL) {
    ns <- shiny::NS(id)
    shiny::div(class = "filter-number",
      if (!is.null(label)) shiny::div(class = "filter-label", label),
      shiny::numericInput(ns("num"), label = NULL, value = NA, min = 0, width = "100%")
    )
  }
}
if (!exists("mod_filters_number_server", mode = "function")) {
  mod_filters_number_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      shiny::reactive({ input$num })
    })
  }
}
if (!exists("mod_filters_buttons_ui", mode = "function")) {
  mod_filters_buttons_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::div(class = "filter-buttons",
      shiny::actionButton(ns("buscar"), "Buscar", class = "btn-primary"),
      shiny::actionButton(ns("refrescar"), "Refrescar", class = "btn-secondary")
    )
  }
}
if (!exists("mod_filters_buttons_server", mode = "function")) {
  mod_filters_buttons_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      list(buscar = shiny::reactive({ input$buscar }), refrescar = shiny::reactive({ input$refrescar }))
    })
  }
}

mod_filters_row_ui <- function(id) {
  ns <- shiny::NS(id)

  # Use get0() to retrieve helper UI functions if they are loaded; otherwise
  # render minimal fallbacks so the UI remains usable during development.
  dd_ui_fn <- get0("mod_filters_dropdown_ui", mode = "function")
  num_ui_fn <- get0("mod_filters_number_ui", mode = "function")
  btns_ui_fn <- get0("mod_filters_buttons_ui", mode = "function")

  ui1 <- if (!is.null(dd_ui_fn)) dd_ui_fn(ns("d1"), label = "Entidad:", choices = NULL) else
    shiny::div(class = "filter-dropdown", shiny::div(class = "filter-label", "Entidad:"), shiny::selectInput(ns("d1_select"), NULL, choices = list("Seleccionar..."), width = "100%"))
  ui2 <- if (!is.null(dd_ui_fn)) dd_ui_fn(ns("d2"), label = "Municipio:", choices = NULL) else
    shiny::div(class = "filter-dropdown", shiny::div(class = "filter-label", "Municipio:"), shiny::selectInput(ns("d2_select"), NULL, choices = list("Seleccionar..."), width = "100%"))
  ui3 <- if (!is.null(num_ui_fn)) num_ui_fn(ns("n1"), label = "Capacidad:") else
    shiny::div(class = "filter-number", shiny::div(class = "filter-label", "Capacidad:"), shiny::numericInput(ns("n1_num"), NULL, value = NA, min = 0, width = "100%"))
  ui4 <- if (!is.null(btns_ui_fn)) btns_ui_fn(ns("btns")) else
    shiny::div(class = "filter-buttons", shiny::actionButton(ns("buscar"), "Buscar"), shiny::actionButton(ns("refrescar"), "Refrescar"))

  shiny::div(class = "filters-row", ui1, ui2, ui3, ui4)
}

mod_filters_row_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Safely retrieve submodule server functions if loaded; otherwise provide
    # simple reactive fallbacks so callers can rely on the returned shape.
    dd_srv_fn <- get0("mod_filters_dropdown_server", mode = "function")
    num_srv_fn <- get0("mod_filters_number_server", mode = "function")
    btns_srv_fn <- get0("mod_filters_buttons_server", mode = "function")

    sel1 <- if (!is.null(dd_srv_fn)) dd_srv_fn("d1") else shiny::reactive({ input$d1_select })
    sel2 <- if (!is.null(dd_srv_fn)) dd_srv_fn("d2") else shiny::reactive({ input$d2_select })
    num  <- if (!is.null(num_srv_fn)) num_srv_fn("n1") else shiny::reactive({ input$n1_num })
    btns <- if (!is.null(btns_srv_fn)) btns_srv_fn("btns") else list(buscar = shiny::reactive({ input$buscar }), refrescar = shiny::reactive({ input$refrescar }))

    # Exponer reactivas (consistent shape)
    list(sel1 = sel1, sel2 = sel2, num = num, buscar = btns$buscar, refrescar = btns$refrescar)
  })
}

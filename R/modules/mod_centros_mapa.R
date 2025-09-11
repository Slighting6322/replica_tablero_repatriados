#' @import magrittr
if (getRversion() >= "2.15.1") utils::globalVariables("%>%")
#' Módulo UI para el mapa de centros de atención
#' @param id id del módulo
#' @importFrom shiny NS moduleServer
#' @importFrom leaflet leafletOutput renderLeaflet leaflet addTiles addCircleMarkers
#' @importFrom magrittr %>%
mod_centros_mapa_ui <- function(id) {
  ns <- shiny::NS(id)
  div(
    style = "max-width:800px;margin:0 auto;", # ancho máximo y centrado
    leaflet::leafletOutput(ns("mapa_centros"), height = 500)
  )
}

#' Módulo Server para el mapa de centros de atención
#' @param id id del módulo
#' @param data dataframe con columnas Latitud, Longitud, Entidad, Municipio, Capacidad
mod_centros_mapa_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    output$mapa_centros <- leaflet::renderLeaflet({
      icon_personas <- leaflet::makeIcon(
        iconUrl = "images/iconos_centros.png", # icono local de 3 personas
        iconWidth = 20, iconHeight = 20,
        iconAnchorX = 10, iconAnchorY = 20
      )
      leaflet::leaflet(data) %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::addMarkers(
          lng = ~Longitud,
          lat = ~Latitud,
          label = ~Responsable,
          popup = ~paste0("<b>", Entidad, ", ", Municipio, "</b><br>Capacidad: ", Capacidad, "<br>Responsable: ", Responsable),
          icon = icon_personas
        )
    })
  })
}

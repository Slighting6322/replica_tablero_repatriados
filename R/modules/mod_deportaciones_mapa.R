#' Módulo UI para el mapa de deportaciones (EEUU)
library(leaflet)
#' @param id id del módulo
mod_deportaciones_mapa_ui <- function(id) {
  ns <- shiny::NS(id)
  div(
    style = "max-width:800px;margin:0 auto;",
  leaflet::leafletOutput(ns("mapa_deportaciones"), height = 500),
  shiny::tags$div(class = "deportaciones-subtitulo", "Repatriaciones: Destino en México")
  )
}

#' Módulo Server para el mapa de deportaciones
#' @param id id del módulo
#' @param data dataframe con columnas Estados, Repatriados
mod_deportaciones_mapa_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    # Cargar tabla de estados de EEUU con lat/lon centroides
    estados_coords <- data.frame(
      Estados = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"),
      Latitud = c(32.8067,61.3707,33.7298,34.9697,36.1162,39.0598,41.5978,39.3185,27.7663,33.0406,21.0943,44.2405,40.3495,39.8494,42.0115,38.5266,37.6681,31.1695,44.6939,39.0639,42.2302,43.3266,45.6945,32.7416,38.4561,46.9219,41.1254,38.3135,43.4525,40.2989,34.8405,42.1657,35.6301,47.5289,40.3888,35.5653,44.5720,41.2033,41.6809,33.8569,44.2998,35.7478,31.0545,40.1500,44.0459,37.7693,47.4009,38.4912,44.2685,42.7559),
      Longitud = c(-86.7911,-152.4044,-111.4312,-92.3731,-119.6816,-105.3111,-72.7554,-75.5071,-81.6868,-83.6431,-157.4983,-114.4788,-88.9861,-86.2583,-93.2105,-96.7265,-84.6701,-91.8678,-69.3819,-76.8021,-71.5301,-84.5361,-93.9002,-89.6787,-92.2884,-110.4544,-98.2681,-117.0554,-71.5639,-74.5210,-106.2485,-74.9481,-79.8064,-99.7840,-82.7649,-96.9289,-122.0709,-77.1945,-71.5118,-80.9450,-99.4388,-86.6923,-97.5635,-111.8624,-72.7107,-78.1699,-121.4905,-80.9546,-100.2307,-107.3025)
    )
    # Helper: normalizar nombres (quitar acentos, trim, minúsculas)
    norm_name <- function(x) {
      if (is.null(x)) return(x)
      x2 <- iconv(as.character(x), from = "UTF-8", to = "ASCII//TRANSLIT")
      x2 <- tolower(trimws(x2))
      x2
    }
    # Intentar unir data con coordenadas de EEUU (sin normalizar aún)
    data_map_us <- merge(data, estados_coords, by = "Estados", all.x = TRUE)
    # Tabla de centroides aproximados para estados de México (uso como fallback si los datos son mexicanos)
    mexico_coords <- data.frame(
      Estados = c("Aguascalientes","Baja California","Baja California Sur","Campeche","Chiapas","Chihuahua","Coahuila","Colima","Durango","Estado de México","Guanajuato","Guerrero","Hidalgo","Jalisco","Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas","Ciudad de México"),
      Latitud = c(21.8823,31.7719,24.1426,19.8301,16.7569,28.6320,27.0587,19.1227,24.0277,19.4969,21.0190,17.5065,20.1031,20.6597,19.5665,18.6815,21.7514,25.6678,17.0732,19.0413,20.5888,19.1816,22.1565,24.0193,17.8409,22.1566,23.6674,19.3139,19.1738,20.7090,19.4326),
      Longitud = c(-102.2826,-115.5766,-110.3128,-90.5349,-92.6818,-106.0691,-101.7068,-103.6770,-104.6532,-99.7233,-101.2574,-99.4975,-98.7590,-103.3496,-101.7068,-99.1332,-104.8455,-100.3108,-96.7266,-98.2063,-100.3899,-87.0739,-101.6645,-110.9540,-93.0640,-97.8694,-98.2062,-97.7475,-96.1420,-102.5728,-99.1332)
    )
    # Intentar unir data con centroides de México
    data_map_mx <- merge(data, mexico_coords, by = "Estados", all.x = TRUE)
  # Contar coincidencias por latitud encontradas (normalizando nombres para comparar)
  nm_data <- norm_name(data$Estados)
  nm_us <- norm_name(as.character(estados_coords$Estados))
  nm_mx <- norm_name(as.character(mexico_coords$Estados))
  matches_us <- sum(nm_data %in% nm_us)
  matches_mx <- sum(nm_data %in% nm_mx)
    # Elegir la unión con más coincidencias
    if (matches_us >= matches_mx) {
      data_map <- data_map_us
      source_used <- "US"
    } else {
      data_map <- data_map_mx
      source_used <- "MX"
    }
    message(sprintf("[mod_deportaciones_mapa] coincidencias US=%d MX=%d, usando: %s", matches_us, matches_mx, source_used))
    # Normalizar nombre de columna de conteo: aceptar 'Repatriados' o 'Repatriaciones' u otras variantes
    if (!"Repatriados" %in% names(data_map)) {
      if ("Repatriaciones" %in% names(data_map)) {
        data_map$Repatriados <- data_map$Repatriaciones
      } else if ("Repatriacion" %in% names(data_map)) {
        data_map$Repatriados <- data_map$Repatriacion
      } else {
        # Buscar columna cuyo nombre en minúsculas sea 'count' o 'cantidad'
        lname <- tolower(names(data_map))
        if ("count" %in% lname) {
          cnt_col <- names(data_map)[which(lname == "count")[1]]
          data_map$Repatriados <- data_map[[cnt_col]]
        } else if ("cantidad" %in% lname) {
          cnt_col <- names(data_map)[which(lname == "cantidad")[1]]
          data_map$Repatriados <- data_map[[cnt_col]]
        } else {
          # Si no existe ninguna, crear columna vacía para evitar errores en las fórmulas
          data_map$Repatriados <- NA
        }
      }
    }
    output$mapa_deportaciones <- leaflet::renderLeaflet({
      has_sf <- requireNamespace("sf", quietly = TRUE)
      has_maps <- requireNamespace("maps", quietly = TRUE)

      # Mensaje base para instrucciones de instalación (R + SO)
      install_msg <- function() {
        paste0(
          "<div style='padding:8px; font-size:12px; line-height:1.2;'>",
          "<b>Polígonos no disponibles:</b> faltan paquetes necesarios para crear el choropleth.<br>",
          "Instala en R: <code>install.packages(\"sf\")</code> y <code>install.packages(\"maps\")</code>.<br>",
          "Si usas Linux (Debian/Ubuntu), instala antes: <code>sudo apt install libgdal-dev libproj-dev libgeos-dev libudunits2-dev</code>.<br>",
          "En macOS: <code>brew install gdal proj geos udunits</code>",
          "</div>"
        )
      }

  # Intentar crear un choropleth usando tigris (datos del US Census) si está disponible y los datos parecen de EEUU
  is_us <- identical(source_used, "US")
  if (is_us && requireNamespace("tigris", quietly = TRUE) && requireNamespace("sf", quietly = TRUE)) {
        tryCatch({
          states_sf <- tigris::states(cb = TRUE)
          name_col <- if ("NAME" %in% names(states_sf)) "NAME" else names(states_sf)[1]
          states_sf$region <- tolower(states_sf[[name_col]])
          # Normalizar y deduplicar claves antes de merge para evitar mismatch de filas
          data_map$region <- tolower(iconv(as.character(data_map$Estados), from = "UTF-8", to = "ASCII//TRANSLIT"))
          states_sf$region <- tolower(iconv(as.character(states_sf$region), from = "UTF-8", to = "ASCII//TRANSLIT"))
          # En data_map puede haber duplicados — agregarlos por suma para tener una fila por región
          agg <- aggregate(Repatriados ~ region, data = data_map, FUN = function(x) if (all(is.na(x))) NA else sum(as.numeric(x), na.rm = TRUE))
          states_sf <- merge(states_sf, agg, by = "region", all.x = TRUE)
          pal <- leaflet::colorNumeric("YlOrRd", domain = states_sf$Repatriados, na.color = "#EEEEEE")
          choropleth_map <- leaflet::leaflet(states_sf) %>%
            leaflet::addProviderTiles("CartoDB.Positron") %>%
            leaflet::addPolygons(fillColor = ~pal(Repatriados), fillOpacity = 0.75, color = "#444", weight = 1,
                                 label = ~paste0(region, ": ", ifelse(is.na(Repatriados), "N/A", Repatriados), " repatriaciones"),
                                 highlight = leaflet::highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)) %>%
            leaflet::addLegend(pal = pal, values = ~Repatriados, title = "Repatriaciones", position = "bottomright")
          return(choropleth_map)
        }, error = function(e) {
          message("[mod_deportaciones_mapa] Error con tigris: ", conditionMessage(e))
        })
      }

  # Si los datos son de EEUU y tigris no está disponible o falla, intentar usar maps + sf
  if (is_us && requireNamespace("sf", quietly = TRUE) && requireNamespace("maps", quietly = TRUE)) {
        tryCatch({
          m <- maps::map("state", fill = TRUE, plot = FALSE)
          states_sf <- sf::st_as_sf(m)
          region_names <- sapply(strsplit(m$names, ":"), function(x) x[1])
          region_names <- tolower(region_names)
          states_sf$region <- region_names
          data_map$region <- tolower(iconv(as.character(data_map$Estados), from = "UTF-8", to = "ASCII//TRANSLIT"))
          states_sf$region <- tolower(iconv(as.character(states_sf$region), from = "UTF-8", to = "ASCII//TRANSLIT"))
          agg <- aggregate(Repatriados ~ region, data = data_map, FUN = function(x) if (all(is.na(x))) NA else sum(as.numeric(x), na.rm = TRUE))
          states_sf <- merge(states_sf, agg, by.x = "region", by.y = "region", all.x = TRUE)
          pal <- leaflet::colorNumeric("YlOrRd", domain = states_sf$Repatriados, na.color = "#EEEEEE")
          choropleth_map <- leaflet::leaflet(states_sf) %>%
            leaflet::addProviderTiles("CartoDB.Positron") %>%
            leaflet::addPolygons(fillColor = ~pal(Repatriados), fillOpacity = 0.75, color = "#444", weight = 1,
                                 label = ~paste0(region, ": ", ifelse(is.na(Repatriados), "N/A", Repatriados), " repatriaciones"),
                                 highlight = leaflet::highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)) %>%
            leaflet::addLegend(pal = pal, values = ~Repatriados, title = "Repatriaciones", position = "bottomright")
          return(choropleth_map)
        }, error = function(e) {
          message("[mod_deportaciones_mapa] Error creando choropleth con maps+sf: ", conditionMessage(e))
        })
      }

  if (is_us && has_sf && has_maps) {
        choropleth_ok <- FALSE
        choropleth_map <- NULL
        choropleth_err <- NULL
        tryCatch({
          m <- maps::map("state", fill = TRUE, plot = FALSE)
          # Convertir a sf
          states_sf <- sf::st_as_sf(m)
          # Normalizar nombres de región (map$names contiene strings como 'new york:main')
          region_names <- sapply(strsplit(m$names, ":"), function(x) x[1])
          region_names <- tolower(region_names)
          states_sf$region <- region_names
          # Normalizar nombres en data_map
          data_map$region <- tolower(data_map$Estados)
          # Unir datos de repatriaciones por región
          states_sf <- merge(states_sf, data_map[, c("region", "Repatriados")], by.x = "region", by.y = "region", all.x = TRUE)
          # Paleta
          pal <- leaflet::colorNumeric("YlOrRd", domain = states_sf$Repatriados, na.color = "#EEEEEE")
          choropleth_map <- leaflet::leaflet(states_sf) %>%
            leaflet::addProviderTiles("CartoDB.Positron") %>%
            leaflet::addPolygons(fillColor = ~pal(Repatriados), fillOpacity = 0.75, color = "#444", weight = 1,
                                 label = ~paste0(region, ": ", ifelse(is.na(Repatriados), "N/A", Repatriados), " repatriaciones"),
                                 highlight = leaflet::highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)) %>%
            leaflet::addLegend(pal = pal, values = ~Repatriados, title = "Repatriaciones", position = "bottomright")
          choropleth_ok <- TRUE
        }, error = function(e) {
          choropleth_err <<- conditionMessage(e)
          # Registrar en consola del servidor para facilitar debugging
          message("[mod_deportaciones_mapa] Error creando choropleth: ", choropleth_err)
          # Si el error es por mismatch de filas, intentar una conversión alternativa usando maptools + sp
          if (grepl("replacement has", choropleth_err, ignore.case = TRUE)) {
            if (requireNamespace("maptools", quietly = TRUE) && requireNamespace("sp", quietly = TRUE)) {
              try({
                # Reconstruir IDs por región y convertir a SpatialPolygons
                IDs <- sapply(strsplit(m$names, ":"), function(x) x[1])
                sp_polys <- maptools::map2SpatialPolygons(m, IDs = IDs, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
                states_sf <- sf::st_as_sf(sp_polys)
                # rownames(states_sf) suelen contener los IDs (regiones)
                states_sf$region <- tolower(rownames(states_sf))
                data_map$region <- tolower(iconv(as.character(data_map$Estados), from = "UTF-8", to = "ASCII//TRANSLIT"))
                states_sf$region <- tolower(iconv(as.character(states_sf$region), from = "UTF-8", to = "ASCII//TRANSLIT"))
                agg <- aggregate(Repatriados ~ region, data = data_map, FUN = function(x) if (all(is.na(x))) NA else sum(as.numeric(x), na.rm = TRUE))
                states_sf <- merge(states_sf, agg, by.x = "region", by.y = "region", all.x = TRUE)
                pal <- leaflet::colorNumeric("YlOrRd", domain = states_sf$Repatriados, na.color = "#EEEEEE")
                choropleth_map <<- leaflet::leaflet(states_sf) %>%
                  leaflet::addProviderTiles("CartoDB.Positron") %>%
                  leaflet::addPolygons(fillColor = ~pal(Repatriados), fillOpacity = 0.75, color = "#444", weight = 1,
                                       label = ~paste0(region, ": ", ifelse(is.na(Repatriados), "N/A", Repatriados), " repatriaciones"),
                                       highlight = leaflet::highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)) %>%
                  leaflet::addLegend(pal = pal, values = ~Repatriados, title = "Repatriaciones", position = "bottomright")
                choropleth_ok <<- TRUE
                message("[mod_deportaciones_mapa] Choropleth generado usando maptools::map2SpatialPolygons")
              }, silent = TRUE)
            } else {
              message("[mod_deportaciones_mapa] maptools/sp no disponibles, no se puede intentar conversión alternativa. Considera instalar 'maptools' y 'sp'.")
            }
          }
        })

        if (choropleth_ok) {
          return(choropleth_map)
        } else {
          # Si falló la creación del choropleth, mostrar aviso encima del mapa con el mensaje de error
          err_html <- if (!is.null(choropleth_err)) {
            paste0("<div style='padding:6px; font-size:12px;'><b>Error creando polígonos:</b> ", htmltools::htmlEscape(choropleth_err),
                   "<br>Se mostrará fallback con marcadores.</div>")
          } else {
            "<div style='padding:6px; font-size:12px;'><b>Error creando polígonos.</b> Se mostrará fallback con marcadores.</div>"
          }
          # Mostrar marcadores con la unión seleccionada (US o MX)
          # Filtrar filas con coordenadas válidas
          data_coords <- data_map[!is.na(data_map$Latitud) & !is.na(data_map$Longitud) & is.finite(data_map$Latitud) & is.finite(data_map$Longitud), ]
          if (nrow(data_coords) == 0) {
            # Si no hay coordenadas, mostrar solo el mensaje de error
            return(
              leaflet::leaflet() %>%
                leaflet::addProviderTiles("CartoDB.Positron") %>%
                leaflet::addControl(html = err_html, position = "topright")
            )
          }
          leaflet::leaflet(data_coords) %>%
            leaflet::addProviderTiles("CartoDB.Positron") %>%
            leaflet::addControl(html = err_html, position = "topright") %>%
            leaflet::addCircleMarkers(
              lng = ~Longitud,
              lat = ~Latitud,
              radius = 8,
              color = "#1b5c4f",
              fillColor = "#1b5c4f",
              fillOpacity = 0.7,
              label = ~paste0(Estados, ": ", ifelse(is.na(Repatriados), "N/A", Repatriados), " repatriaciones")
            )
        }
      }
      # Si los datos no son EEUU o faltan paquetes, informar y mostrar fallback con marcadores
      if (!is_us || !has_sf || !has_maps) {
        msg_html <- install_msg()
        data_coords <- data_map[!is.na(data_map$Latitud) & !is.na(data_map$Longitud) & is.finite(data_map$Latitud) & is.finite(data_map$Longitud), ]
        if (nrow(data_coords) == 0) {
          return(
            leaflet::leaflet() %>%
              leaflet::addProviderTiles("CartoDB.Positron") %>%
              leaflet::addControl(html = msg_html, position = "topright")
          )
        }
        leaflet::leaflet(data_coords) %>%
          leaflet::addProviderTiles("CartoDB.Positron") %>%
          leaflet::addControl(html = msg_html, position = "topright") %>%
          leaflet::addCircleMarkers(
            lng = ~Longitud,
            lat = ~Latitud,
            radius = 8,
            color = "#1b5c4f",
            fillColor = "#1b5c4f",
            fillOpacity = 0.7,
            label = ~paste0(Estados, ": ", ifelse(is.na(Repatriados), "N/A", Repatriados), " repatriaciones")
          )
      } else {
        # Fallback por seguridad (si algo no retornó antes)
        data_coords <- data_map[!is.na(data_map$Latitud) & !is.na(data_map$Longitud) & is.finite(data_map$Latitud) & is.finite(data_map$Longitud), ]
        if (nrow(data_coords) == 0) {
          return(
            leaflet::leaflet() %>%
              leaflet::addProviderTiles("CartoDB.Positron") %>%
              leaflet::addControl(html = "<div style='padding:8px; font-size:12px;'><b>No hay coordenadas válidas.</b></div>", position = "topright")
          )
        }
        leaflet::leaflet(data_coords) %>%
          leaflet::addProviderTiles("CartoDB.Positron") %>%
          leaflet::addCircleMarkers(
            lng = ~Longitud,
            lat = ~Latitud,
            radius = 8,
            color = "#1b5c4f",
            fillColor = "#1b5c4f",
            fillOpacity = 0.7,
            label = ~paste0(Estados, ": ", ifelse(is.na(Repatriados), "N/A", Repatriados), " repatriaciones")
          )
      }
    })
  })
}

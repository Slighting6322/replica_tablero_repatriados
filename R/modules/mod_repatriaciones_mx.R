## Módulo para mapa de repatriaciones (México)
library(leaflet)

mod_repatriaciones_mx_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    style = "max-width:900px;margin:0 auto;",
  leaflet::leafletOutput(ns("mapa_repatriaciones"), height = 520)
  )
}

mod_repatriaciones_mx_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    output$mapa_repatriaciones <- leaflet::renderLeaflet({
      # Asegurar columna de conteo (aceptar variantes)
      cnt_col <- NULL
      for (c in c("Repatriaciones", "Repatriados", "Repatriacion", "count", "cantidad")) if (c %in% names(data)) { cnt_col <- c; break }
      if (is.null(cnt_col)) {
        data$Repatriaciones <- NA
        cnt_col <- "Repatriaciones"
      } else if (cnt_col != "Repatriaciones") {
        data$Repatriaciones <- data[[cnt_col]]
      }

      # Normalizar posibles nombres de columnas de coordenadas y estado
      colnames_lower <- tolower(names(data))
      # posibles nombres para lat/lon
      lat_candidates <- c("latitud", "lat", "latitude")
      lon_candidates <- c("longitud", "lon", "lng", "long", "longitude")
      found_lat <- names(data)[which(colnames_lower %in% lat_candidates)[1]]
      found_lon <- names(data)[which(colnames_lower %in% lon_candidates)[1]]
      if (!is.null(found_lat) && !is.na(found_lat)) data$Latitud <- as.numeric(data[[found_lat]])
      if (!is.null(found_lon) && !is.na(found_lon)) data$Longitud <- as.numeric(data[[found_lon]])
      # Aceptar variantes en nombre de la columna de estado
      if (!"Estados" %in% names(data)) {
        state_candidates <- c("estado","entidad","state","nombre")
        found_state <- names(data)[which(colnames_lower %in% state_candidates)[1]]
        if (!is.null(found_state) && !is.na(found_state)) {
          data$Estados <- as.character(data[[found_state]])
        }
      }
      # Preparar nombre a mostrar en etiquetas: preferir 'Estados_es' si existe
      if ("Estados_es" %in% names(data)) {
        data$Estados_display <- data$Estados_es
      } else {
        data$Estados_display <- data$Estados
      }
      # Si faltan coordenadas en el CSV, intentar añadir centroides aproximados para estados de México
      need_coords <- !("Latitud" %in% names(data) && "Longitud" %in% names(data)) || all(is.na(data$Latitud)) || all(is.na(data$Longitud))
      if (need_coords) {
        mexico_centroids <- data.frame(
          Estados = c("Aguascalientes","Baja California","Baja California Sur","Campeche","Chiapas","Chihuahua","Coahuila","Colima","Durango","Estado de México","Guanajuato","Guerrero","Hidalgo","Jalisco","Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas","Ciudad de México"),
          Latitud = c(21.8823,31.7719,24.1426,19.8301,16.7569,28.6320,27.0587,19.1227,24.0277,19.4969,21.0190,17.5065,20.1031,20.6597,19.5665,18.6815,21.7514,25.6678,17.0732,19.0413,20.5888,19.1816,22.1565,24.0193,17.8409,22.1566,23.6674,19.3139,19.1738,20.7090,22.7709,19.4326),
          Longitud = c(-102.2826,-115.5766,-110.3128,-90.5349,-92.6818,-106.0691,-101.7068,-103.6770,-104.6532,-99.7233,-101.2574,-99.4975,-98.7590,-103.3496,-101.7068,-99.1332,-104.8455,-100.3108,-96.7266,-98.2063,-100.3899,-87.0739,-101.6645,-110.9540,-93.0640,-97.8694,-98.2062,-97.7475,-96.1420,-102.5728,-102.5832,-99.1332)
        )
        norm <- function(x) tolower(iconv(as.character(x), from = "UTF-8", to = "ASCII//TRANSLIT"))
  # asegurar columna Estados
  if (!"Estados" %in% names(data)) data$Estados <- NA
  data$.__estado_norm <- norm(data$Estados)
        mexico_centroids$.__estado_norm <- norm(mexico_centroids$Estados)
  # Preparar sub-DF de centroides, renombrar lat/long para evitar colisiones y luego hacer coalesce
  cent_sub <- mexico_centroids[, c(".__estado_norm", "Latitud", "Longitud"), drop = FALSE]
  names(cent_sub)[names(cent_sub) == "Latitud"] <- "Lat_centro"
  names(cent_sub)[names(cent_sub) == "Longitud"] <- "Lon_centro"
  # eliminar cualquier otra columna en cent_sub que colisione con 'data' (excepto la llave)
  overlap <- intersect(names(data), names(cent_sub))
  overlap <- setdiff(overlap, ".__estado_norm")
  if (length(overlap) > 0) cent_sub[overlap] <- NULL
  # Usar sufijos en el merge para evitar duplicación de nombres; luego limpiar columnas '.cent'
  data <- merge(data, cent_sub, by.x = ".__estado_norm", by.y = ".__estado_norm", all.x = TRUE, sort = FALSE, suffixes = c("", ".cent"))
  # Coalesce lat/long: prefer columnas originales si existen, sino usar las del centroide
        if (!"Latitud" %in% names(data)) data$Latitud <- NA_real_
        if (!"Longitud" %in% names(data)) data$Longitud <- NA_real_
        if ("Lat_centro" %in% names(data)) data$Latitud <- ifelse(is.na(data$Latitud), data$Lat_centro, data$Latitud)
        if ("Lon_centro" %in% names(data)) data$Longitud <- ifelse(is.na(data$Longitud), data$Lon_centro, data$Longitud)
        # eliminar columnas temporales y cualquier columna con sufijo .cent
        rm_cols <- grep("\\.cent$", names(data), value = TRUE)
        # también eliminar las columnas Lat_centro/Lon_centro si existen (ya fueron coalesced)
        rm_cols <- unique(c(rm_cols, intersect(names(data), c("Lat_centro", "Lon_centro"))))
        if (length(rm_cols) > 0) data[rm_cols] <- NULL
        data$.__estado_norm <- NULL
      }

  # Asumir que paquetes requeridos (sf, rnaturalearth) están disponibles (ver global.R)

    # Intentar usar rnaturalearth para obtener polígonos de estados de México (no requiere archivos locales)
  tryCatch({
        # Intentar obtener polígonos con rnaturalearth (asume 'rnaturalearth' y 'sf' instalados)
        states_sf <- rnaturalearth::ne_states(country = "Mexico", returnclass = "sf")
        # continuar con pipeline de construcción de choropleth
        
        
        
          states_sf <- rnaturalearth::ne_states(country = "Mexico", returnclass = "sf")
          # encontrar columna con nombre del estado
          nm_col <- NULL
          possible <- c("name", "NAME", "name_en", "name_long", "nombre", "admin")
          for (c in possible) if (c %in% names(states_sf)) { nm_col <- c; break }
          if (is.null(nm_col)) nm_col <- names(states_sf)[1]
          states_sf$estado_norm <- tolower(iconv(as.character(states_sf[[nm_col]]), from = "UTF-8", to = "ASCII//TRANSLIT"))
          data$estado_norm <- tolower(iconv(as.character(data$Estados), from = "UTF-8", to = "ASCII//TRANSLIT"))
          # Agregar por estado en input para evitar duplicados
          agg <- aggregate(Repatriaciones ~ estado_norm, data = data, FUN = function(x) if (all(is.na(x))) NA else sum(as.numeric(x), na.rm = TRUE))
          # Usar match para preservar el orden y número de filas de states_sf
          states_sf$Repatriaciones <- NA_real_
          mi <- match(states_sf$estado_norm, agg$estado_norm)
          states_sf$Repatriaciones[!is.na(mi)] <- agg$Repatriaciones[mi[!is.na(mi)]]
          # Asegurar que los estados que no aparecen en 'agg' tengan valor 0 en lugar de NA
          nas <- which(is.na(states_sf$Repatriaciones))
          if (length(nas) > 0) states_sf$Repatriaciones[nas] <- 0L
          # Construir nombres de display para cada agg (usar data$Estados_display si posible)
          agg$display <- sapply(agg$estado_norm, function(en) {
            idx <- which(data$estado_norm == en)
            if (length(idx) >= 1) return(unique(data$Estados_display[idx])[1])
            # fallback title case
            parts <- strsplit(en, " ")[[1]]
            paste(sapply(parts, function(w) paste0(toupper(substring(w,1,1)), substring(w,2))), collapse = " ")
          })
          states_sf$display <- NA_character_
          states_sf$display[!is.na(mi)] <- agg$display[mi[!is.na(mi)]]
          # Para estados sin 'display' asignado, usar el nombre del polígono en Title Case
          missing_disp <- which(is.na(states_sf$display) | states_sf$display == "")
          if (length(missing_disp) > 0) {
            choice_names <- as.character(states_sf[[nm_col]])
            title_case <- sapply(choice_names, function(x) {
              parts <- strsplit(tolower(iconv(as.character(x), from = "UTF-8", to = "ASCII//TRANSLIT")), "[[:space:]]+")[[1]]
              paste(sapply(parts, function(w) paste0(toupper(substring(w,1,1)), substring(w,2))), collapse = " ")
            })
            states_sf$display[missing_disp] <- title_case[missing_disp]
          }
          pal <- leaflet::colorNumeric("YlOrRd", domain = states_sf$Repatriaciones, na.color = "#EEEEEE")
          return(
            leaflet::leaflet(states_sf) %>%
              leaflet::addProviderTiles("CartoDB.Positron") %>%
              leaflet::addPolygons(fillColor = ~pal(Repatriaciones), fillOpacity = 0.8, color = "#444", weight = 1,
                                   label = ~paste0(ifelse(is.na(Repatriaciones), paste0(display, ": N/A"), paste0(display, ": ", Repatriaciones)))) %>%
              leaflet::addLegend(pal = pal, values = ~Repatriaciones, title = "Repatriaciones", position = "bottomright") %>%
              leaflet::addControl(html = paste0("<div style='padding:6px; font-size:12px;'><b>Polígonos:</b> rnaturalearth</div>"), position = "topright")
          )
        }, error = function(e) {
          message("[mod_repatriaciones_mx] rnaturalearth error: ", e$message)
        })

  # If polygon rendering via rnaturalearth failed, fall back to circle markers using available coordinates.

  # Si no se pudo crear choropleth vía rnaturalearth, usar fallback de marcadores / centroides
      data_coords <- data[!is.na(data$Latitud) & !is.na(data$Longitud) & is.finite(data$Latitud) & is.finite(data$Longitud), ]
      if (nrow(data_coords) == 0) {
        msg_html <- paste0("<div style='padding:8px; font-size:12px;'><b>No hay coordenadas válidas.</b> Provee columnas 'Latitud'/'Longitud' en el CSV o asegúrate que los nombres de estados coinciden con los centroids.</div>")
        return(
          leaflet::leaflet() %>%
            leaflet::addProviderTiles("CartoDB.Positron") %>%
            leaflet::addControl(html = msg_html, position = "topright")
        )
      }
      data_coords$Estados_display <- if ("Estados_display" %in% names(data_coords)) data_coords$Estados_display else data_coords$Estados
      leaflet::leaflet(data_coords) %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::addCircleMarkers(lng = ~Longitud, lat = ~Latitud, label = ~paste0(Estados_display, ": ", ifelse(is.na(Repatriaciones), "N/A", Repatriaciones)))
    })
  })
}

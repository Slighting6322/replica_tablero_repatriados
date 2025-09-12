#' Módulo UI para el mapa de deportaciones (EEUU)
library(leaflet)
#' @param id id del módulo
mod_deportaciones_mapa_ui <- function(id) {
  ns <- shiny::NS(id)
  div(
    style = "max-width:900px;margin:0 auto;",
  leaflet::leafletOutput(ns("mapa_deportaciones"), height = 500),
  shiny::tags$div(class = "deportaciones-subtitulo", "Repatriaciones: Destino en México")
  )
}

#' Módulo Server para el mapa de deportaciones
#' @param id id del módulo
#' @param data dataframe con columnas Estados, Repatriados
#' @param xlsx_path opcional: ruta al archivo xlsx con los datos (se usa si `data` es NULL)
#' @param xlsx_sheet nombre de la hoja dentro del xlsx (por defecto: "Repatriados")
mod_deportaciones_mapa_server <- function(id, data = NULL, xlsx_path = "data/repatriados.xlsx", xlsx_sheet = "Repatriados") {
  shiny::moduleServer(id, function(input, output, session) {
    # Si no se pasa 'data' o está vacío, intentar leer y agregar desde el xlsx usando el helper
    if ((is.null(data) || (is.data.frame(data) && nrow(data) == 0))) {
      tryCatch({
        if (exists("agg_repatriados_from_xlsx", where = globalenv()) || exists("agg_repatriados_from_xlsx", where = asNamespace("R"))) {
          # llamar la función del data_loader si está disponible
          data_x <- tryCatch(agg_repatriados_from_xlsx(path = xlsx_path, sheet = xlsx_sheet), error = function(e) NULL)
        } else {
          # fallback: intentar cargar localmente si readxl está disponible
          if (file.exists(xlsx_path) && requireNamespace("readxl", quietly = TRUE)) {
            data_x <- tryCatch({
              df_x <- readxl::read_excel(xlsx_path, sheet = xlsx_sheet)
              # heurística simple: buscar columna 'estado' y agregar
              nm_low <- tolower(names(df_x))
              match_idx <- grep("estado", nm_low)
              if (length(match_idx) == 0) stop("no state column detected")
              state_col <- names(df_x)[match_idx[1]]
              df_u <- unique(df_x)
              vals <- as.character(df_u[[state_col]]); vals[is.na(vals)] <- "(sin_estado)"
              tb <- as.data.frame(table(vals), stringsAsFactors = FALSE); names(tb) <- c("Estados","Repatriados"); tb$Repatriados <- as.integer(tb$Repatriados)
              tb
            }, error = function(e) NULL)
          } else {
            data_x <- NULL
          }
        }
        if (!is.null(data_x) && is.data.frame(data_x) && nrow(data_x) > 0) {
          data <- data_x
          message(sprintf("[mod_deportaciones_mapa] Cargados %d estados agregados desde '%s' (hoja: %s)", nrow(data), xlsx_path, xlsx_sheet))
        }
      }, error = function(e) {
        message("[mod_deportaciones_mapa] Error obteniendo datos desde xlsx: ", conditionMessage(e))
      })
    }

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
  # Intentar unir data con coordenadas de EEUU (merge por nombres en inglés)
  # Evitar duplicados al unir con centroides: usar solo columnas necesarias y renombrar si hace falta
  cent_us <- estados_coords[, c("Estados", "Latitud", "Longitud"), drop = FALSE]
  overlap_us <- intersect(names(data), names(cent_us))
  overlap_us <- setdiff(overlap_us, "Estados")
  if (length(overlap_us) > 0) cent_us[overlap_us] <- NULL
  data_map_us <- merge(data, cent_us, by = "Estados", all.x = TRUE)
    # Tabla de centroides aproximados para estados de México (uso como fallback si los datos son mexicanos)
      mexico_coords <- data.frame(
      Estados = c("Aguascalientes","Baja California","Baja California Sur","Campeche","Chiapas","Chihuahua","Coahuila","Colima","Durango","Estado de México","Guanajuato","Guerrero","Hidalgo","Jalisco","Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas","Ciudad de México"),
      Latitud = c(21.8823,31.7719,24.1426,19.8301,16.7569,28.6320,27.0587,19.1227,24.0277,19.4969,21.0190,17.5065,20.1031,20.6597,19.5665,18.6815,21.7514,25.6678,17.0732,19.0413,20.5888,19.1816,22.1565,24.0193,17.8409,22.1566,23.6674,19.3139,19.1738,20.7090,22.7709,19.4326),
      Longitud = c(-102.2826,-115.5766,-110.3128,-90.5349,-92.6818,-106.0691,-101.7068,-103.6770,-104.6532,-99.7233,-101.2574,-99.4975,-98.7590,-103.3496,-101.7068,-99.1332,-104.8455,-100.3108,-96.7266,-98.2063,-100.3899,-87.0739,-101.6645,-110.9540,-93.0640,-97.8694,-98.2062,-97.7475,-96.1420,-102.5728,-102.5832,-99.1332)
    )
  # Intentar unir data con centroides de México
  cent_mx <- mexico_coords[, c("Estados", "Latitud", "Longitud"), drop = FALSE]
  overlap_mx <- intersect(names(data), names(cent_mx))
  overlap_mx <- setdiff(overlap_mx, "Estados")
  if (length(overlap_mx) > 0) cent_mx[overlap_mx] <- NULL
  data_map_mx <- merge(data, cent_mx, by = "Estados", all.x = TRUE)
  # Contar coincidencias por latitud encontradas (normalizando nombres para comparar)
  # Normalizar nombres en ambos lados para matching
  if (exists("normalize_state_names", where = globalenv()) || exists("normalize_state_names", where = asNamespace("R"))) {
    nm_data <- normalize_state_names(data$Estados)
    nm_us <- normalize_state_names(as.character(estados_coords$Estados))
    nm_mx <- normalize_state_names(as.character(mexico_coords$Estados))
  } else {
    nm_data <- norm_name(data$Estados)
    nm_us <- norm_name(as.character(estados_coords$Estados))
    nm_mx <- norm_name(as.character(mexico_coords$Estados))
  }
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
    # Preparar nombre a mostrar en etiquetas: preferir 'Estados_es' si existe
    if ("Estados_es" %in% names(data_map)) {
      data_map$Estados_display <- data_map$Estados_es
    } else {
      data_map$Estados_display <- data_map$Estados
    }
    # Asegurar que Repatriados no tenga NA (usar 0 para faltantes)
    if ("Repatriados" %in% names(data_map)) {
      data_map$Repatriados <- as.integer(ifelse(is.na(data_map$Repatriados), 0, data_map$Repatriados))
    }

    output$mapa_deportaciones <- leaflet::renderLeaflet({
  # Asumir paquetes requeridos instalados (ver global.R): sf, maps, tigris cuando aplique

  # Intentar crear un choropleth usando tigris (datos del US Census) si está disponible y los datos parecen de EEUU
  is_us <- identical(source_used, "US")
  if (is_us && requireNamespace("tigris", quietly = TRUE) && requireNamespace("sf", quietly = TRUE)) {
        tryCatch({
          states_sf <- tigris::states(cb = TRUE)
          # forzar CRS a WGS84 para evitar warnings de datum inconsistente
          if (inherits(states_sf, "sf")) {
            try({ states_sf <- sf::st_transform(states_sf, crs = 4326) }, silent = TRUE)
          }
          name_col <- if ("NAME" %in% names(states_sf)) "NAME" else names(states_sf)[1]
          states_sf$region <- tolower(states_sf[[name_col]])
          # Normalizar y deduplicar claves antes de merge para evitar mismatch de filas
          data_map$region <- tolower(iconv(as.character(data_map$Estados), from = "UTF-8", to = "ASCII//TRANSLIT"))
          states_sf$region <- tolower(iconv(as.character(states_sf$region), from = "UTF-8", to = "ASCII//TRANSLIT"))
          # En data_map puede haber duplicados — agregarlos por suma para tener una fila por región
          agg <- aggregate(Repatriados ~ region, data = data_map, FUN = function(x) if (all(is.na(x))) NA else sum(as.numeric(x), na.rm = TRUE))
          # preserve order/rows of states_sf using match instead of merge
          states_sf$Repatriados <- NA_real_
          mi <- match(states_sf$region, agg$region)
          states_sf$Repatriados[!is.na(mi)] <- agg$Repatriados[mi[!is.na(mi)]]
          # Construir nombre a mostrar para cada región (usar Estados_display si está disponible)
          agg$display <- sapply(agg$region, function(r) {
            idx <- which(data_map$region == r)
            if (length(idx) >= 1 && "Estados_display" %in% names(data_map)) return(unique(data_map$Estados_display[idx])[1])
            # fallback: convertir a Title Case
            parts <- strsplit(r, " ")[[1]]
            paste(sapply(parts, function(w) paste0(toupper(substring(w,1,1)), substring(w,2))), collapse = " ")
          })
          states_sf$display <- NA_character_
          states_sf$display[!is.na(mi)] <- agg$display[mi[!is.na(mi)]]
          pal <- leaflet::colorNumeric("YlOrRd", domain = states_sf$Repatriados, na.color = "#EEEEEE")
          choropleth_map <- leaflet::leaflet(states_sf) %>%
            leaflet::addProviderTiles("CartoDB.Positron") %>%
            leaflet::addPolygons(fillColor = ~pal(Repatriados), fillOpacity = 0.75, color = "#444", weight = 1,
                                 label = ~paste0(display, ": ", ifelse(is.na(Repatriados), "N/A", Repatriados), " repatriaciones"),
                                 highlight = leaflet::highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)) %>%
            leaflet::addLegend(pal = pal, values = ~Repatriados, title = "Repatriaciones", position = "bottomright")
            # Centrar en EEUU continental por defecto
          return(choropleth_map %>% leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 4))
        }, error = function(e) {
          message("[mod_deportaciones_mapa] Error con tigris: ", conditionMessage(e))
        })
      }

  # Si los datos son de EEUU y tigris no está disponible o falla, intentar usar maps + sf (una sola rama simplificada)
  if (is_us && requireNamespace("sf", quietly = TRUE) && requireNamespace("maps", quietly = TRUE)) {
    tryCatch({
      m <- maps::map("state", fill = TRUE, plot = FALSE)
      states_sf <- sf::st_as_sf(m)
      try({ states_sf <- sf::st_transform(states_sf, crs = 4326) }, silent = TRUE)
      region_names <- sapply(strsplit(m$names, ":"), function(x) x[1])
      states_sf$region <- tolower(iconv(region_names, from = "UTF-8", to = "ASCII//TRANSLIT"))
      data_map$region <- tolower(iconv(as.character(data_map$Estados), from = "UTF-8", to = "ASCII//TRANSLIT"))
      agg <- aggregate(Repatriados ~ region, data = data_map, FUN = function(x) if (all(is.na(x))) NA else sum(as.numeric(x), na.rm = TRUE))
      states_sf$Repatriados <- NA_real_
      mi <- match(states_sf$region, agg$region)
      states_sf$Repatriados[!is.na(mi)] <- agg$Repatriados[mi[!is.na(mi)]]
      # Construir display names usando Estados_display si existe
      agg$display <- sapply(agg$region, function(r) {
        idx <- which(data_map$region == r)
        if (length(idx) >= 1 && "Estados_display" %in% names(data_map)) return(unique(data_map$Estados_display[idx])[1])
        parts <- strsplit(r, " ")[[1]]
        paste(sapply(parts, function(w) paste0(toupper(substring(w,1,1)), substring(w,2))), collapse = " ")
      })
      states_sf$display <- NA_character_
      states_sf$display[!is.na(mi)] <- agg$display[mi[!is.na(mi)]]
      pal <- leaflet::colorNumeric("YlOrRd", domain = states_sf$Repatriados, na.color = "#EEEEEE")
      choropleth_map <- leaflet::leaflet(states_sf) %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::addPolygons(fillColor = ~pal(Repatriados), fillOpacity = 0.75, color = "#444", weight = 1,
                             label = ~paste0(display, ": ", ifelse(is.na(Repatriados), "N/A", Repatriados), " repatriaciones"),
                             highlight = leaflet::highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)) %>%
        leaflet::addLegend(pal = pal, values = ~Repatriados, title = "Repatriaciones", position = "bottomright")
      return(choropleth_map %>% leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 4))
    }, error = function(e) {
      message("[mod_deportaciones_mapa] maps+sf choropleth error: ", conditionMessage(e))
    })
  }
      # Si los datos no son EEUU o faltan paquetes, informar y mostrar fallback con marcadores
      if (!is_us) {
        # Si los datos no son EEUU, mostrar fallback con marcadores (las dependencias requieren estar instaladas según global.R)
        msg_html <- "<div style='padding:8px; font-size:12px;'><b>Mapa no disponible para la región proporcionada.</b></div>"
  data_coords <- data_map[!is.na(data_map$Latitud) & !is.na(data_map$Longitud) & is.finite(data_map$Latitud) & is.finite(data_map$Longitud), ]
  data_coords$Estados_display <- if ("Estados_es" %in% names(data_coords)) data_coords$Estados_es else data_coords$Estados
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
      label = ~paste0(Estados_display, ": ", ifelse(is.na(Repatriados), "N/A", Repatriados), " repatriaciones")
          ) %>%
          leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 4)
      } else {
        # Fallback por seguridad (si algo no retornó antes)
  data_coords <- data_map[!is.na(data_map$Latitud) & !is.na(data_map$Longitud) & is.finite(data_map$Latitud) & is.finite(data_map$Longitud), ]
  data_coords$Estados_display <- if ("Estados_es" %in% names(data_coords)) data_coords$Estados_es else data_coords$Estados
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
      label = ~paste0(Estados_display, ": ", ifelse(is.na(Repatriados), "N/A", Repatriados), " repatriaciones")
          ) %>%
          leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 4)
      }
    })
  })
}

library(shiny)

# Source modules so UI functions (mod_*_ui) are available
mods_dir <- "R/modules"
if (dir.exists(mods_dir)) {
  files <- list.files(mods_dir, pattern = "^mod_.*\\.R$", full.names = TRUE)
  for (f in files) {
    tryCatch(source(f), error = function(e) message("Error sourcing module UI file: ", f, " - ", e$message))
  }
}

# Build UI fully in R so body is reactive and modules are inserted directly
ui <- fluidPage(class = "app-root-full",
  tags$head(
    # Shiny dependencies are added by fluidPage; include fonts and CSS
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter&family=Montserrat&family=Noto+Sans&display=swap", rel = "stylesheet"),
  # Forzar recarga del CSS cuando se hacen cambios: se añade query string de versión
  tags$link(rel = "stylesheet", href = "css/style.css?v=4"),
    tags$title("Tablero")
  ),

  # Header and footer implemented as Shiny modules (migrated from web components)

  # Page structure (migrated from index.html body)
  div(class = "page",
  # header module
  if (exists("mod_header_ui")) mod_header_ui("header1") else HTML("<header-gob></header-gob>"),

      div(class = "page-wrapper",
          div(class = "container-xl mt-4",

              # HOME SECTION
              tags$section(id = "seccion-home", class = "seccion", `data-seccion` = "home",
                           h1(class = "page-title", "Ocupación por centro de atención"),
                           p(class = "page-subtitle",
                             "Fecha de corte: ",
                             if (exists("mod_fecha_ui")) mod_fecha_ui("fecha_home", inline = TRUE) else textOutput("fecha_corte_texto_home", inline = TRUE)
                           ),
                           h2(class = "titulo-barra", paste0(
                             "Ocupación General: ",
                             if (!is.na(porcentaje_ocupacion)) paste0(porcentaje_ocupacion, "%") else ""
                           )),
                           # Barra de progreso de ocupación (modular)
                           if (exists("mod_progress_bar_ui")) mod_progress_bar_ui("ocupacion_bar1") else div(class="progress-fallback", "(Barra de ocupación no disponible)"),
                           # Tarjetas KPI debajo de la barra
                           if (exists("mod_kpi_cards_grid_ui")) mod_kpi_cards_grid_ui("kpi_grid1") else div(class="kpi-cards-fallback", "(Indicadores no disponibles)"),
                           div(class = "subtitulo-filtro-centros", "Filtro de Centros de Atención"),
                           if (exists("mod_filters_row_ui")) mod_filters_row_ui("filtros1") else div(class = "filters-row-fallback", "(Controles no disponibles)"),
                           if (exists("mod_home_ui")) mod_home_ui("home1") else div(id = "home-module-placeholder"),
                           if (exists("mod_centros_mapa_ui")) mod_centros_mapa_ui("centrosmapa1") else div(class = "mapa-centros-fallback", "(Mapa de centros no disponible)"),
               ## FILTROS DE MAPA (debajo del mapa) - alineados horizontalmente
                          div(class = "filters-row mapa-filtros", style = "max-width:1100px;",
                 if (exists("mod_filters_dropdown_ui")) mod_filters_dropdown_ui("map_dd_entidad", label = "Selecciona un centro de atención:") else div(class = "filter-dropdown", "(Dropdown no disponible)"),
                 if (exists("mod_filters_buttons_ui")) mod_filters_buttons_ui("map_btns") else div(class = "filter-buttons", "(Botones no disponibles)")
               ),
               # Segunda barra de progreso (debajo de la fila de filtros del mapa)
               h2(class = "titulo-barra", paste0(
                 "Ocupación General: ",
                 if (!is.na(porcentaje_ocupacion)) paste0(porcentaje_ocupacion, "%") else ""
               )),
               if (exists("mod_progress_bar_ui")) mod_progress_bar_ui("ocupacion_bar2") else div(class = "progress-fallback", "(Barra secundaria no disponible)")
              ,
              # Segundo arreglo de tarjetas KPI (debajo de la segunda barra)
              if (exists("mod_kpi_cards_grid_ui")) mod_kpi_cards_grid_ui("kpi_grid2") else div(class = "kpi-cards-fallback", "(Indicadores secundarios no disponibles)")
              ),

              # ORIGEN SECTION (hidden by default)
              tags$section(id = "seccion-origen", class = "seccion", `data-seccion` = "origen", style = "display:none;",
                           h1(class = "page-title", "Deportaciones desde EEUU"),
                           p(class = "page-subtitle",
                             "Fecha de corte: ",
                             if (exists("mod_fecha_ui")) mod_fecha_ui("fecha_origen", inline = TRUE) else textOutput("fecha_corte_texto_origen", inline = TRUE)
                           ),
                           if (exists("mod_deportaciones_mapa_ui")) mod_deportaciones_mapa_ui("deportacionesmapa1") else div(class = "mapa-deportaciones-fallback", "(Mapa de deportaciones no disponible)"),
                          # Mapa secundario para repatriaciones (usar el mismo módulo si es posible)
                           if (exists("mod_repatriaciones_mx_ui")) mod_repatriaciones_mx_ui("repatriacionesmapa1") else if (exists("mod_deportaciones_mapa_ui")) mod_deportaciones_mapa_ui("repatriacionesmapa1") else div(class = "mapa-repatriaciones-fallback", "(Mapa de repatriaciones no disponible)"),
                           if (exists("mod_origen_ui")) mod_origen_ui("origen1") else div(id = "origen-module-placeholder")
              )

          )
    ),
  # footer module (sin clase experimental full-bleed)
  if (exists("mod_footer_ui")) mod_footer_ui("footer1") else HTML("<footer class='footer-gob'></footer>")
  ),

  # Router / section manager script (enhanced to notify Shiny and trigger resize/invalidate)
  tags$script(HTML(
    "(function () {\n      function showSection(target) {\n        document.querySelectorAll('.seccion').forEach(function(s) {\n          s.style.display = (s.dataset.seccion === target) ? '' : 'none';\n        });\n        document.querySelectorAll('.btn-seccion').forEach(function(b) {\n          b.setAttribute('aria-pressed', b.dataset.target === target ? 'true' : 'false');\n        });\n        history.replaceState(null, '', '#' + target);\n        // Trigger a resize so Leaflet maps refresh when shown\n        setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 150);\n        // Also attempt to call invalidateSize on any Leaflet containers (some wrappers expose the map object differently)\n        setTimeout(function(){\n          try {\n            document.querySelectorAll('.leaflet-container').forEach(function(el){\n              try { if (el._leaflet_map && typeof el._leaflet_map.invalidateSize === 'function') el._leaflet_map.invalidateSize(); } catch(e) {}\n              try { if (el._leaflet && typeof el._leaflet.invalidateSize === 'function') el._leaflet.invalidateSize(); } catch(e) {}\n            });\n          } catch(e) {}\n        }, 250);\n        if (window.Shiny && Shiny.setInputValue) { Shiny.setInputValue('section_shown', target, {priority: 'event'}); }\n      }\n\n      document.addEventListener('DOMContentLoaded', function () {\n        var headerNav = document.querySelector('.header-nav');\n        if (headerNav) {\n          headerNav.querySelectorAll('a').forEach(function(a) {\n            var href = a.getAttribute('href') || '';\n            if (href.startsWith('#')) {\n              a.addEventListener('click', function (ev) { ev.preventDefault(); showSection(href.replace('#','')); });\n            } else if (href.endsWith('index.html') || href === './' || href === '/') {\n              a.setAttribute('href', '#home');\n              a.addEventListener('click', function (ev) { ev.preventDefault(); showSection('home'); });\n            }\n          });\n        }\n\n        var hash = location.hash.replace('#', '');\n        if (hash === 'origen') showSection('origen'); else showSection('home');\n      });\n    })();"
  ))
)
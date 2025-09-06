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
ui <- fluidPage(
  tags$head(
    # Shiny dependencies are added by fluidPage; include fonts and CSS
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter&family=Montserrat&family=Noto+Sans&display=swap", rel = "stylesheet"),
    tags$link(rel = "stylesheet", href = "css/style.css"),
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
                           h2(class = "titulo-barra", "Ocupación general:"),
                           if (exists("mod_home_ui")) mod_home_ui("home1") else div(id = "home-module-placeholder")
              ),

              # ORIGEN SECTION (hidden by default)
              tags$section(id = "seccion-origen", class = "seccion", `data-seccion` = "origen", style = "display:none;",
                           h1(class = "page-title", "Deportaciones desde EEUU"),
                           p(class = "page-subtitle",
                             "Fecha de corte: ",
                             if (exists("mod_fecha_ui")) mod_fecha_ui("fecha_origen", inline = TRUE) else textOutput("fecha_corte_texto_origen", inline = TRUE)
                           ),
                           if (exists("mod_origen_ui")) mod_origen_ui("origen1") else div(id = "origen-module-placeholder")
              )

          )
      ),

  # footer module
  if (exists("mod_footer_ui")) mod_footer_ui("footer1") else HTML("<footer-gob></footer-gob>")
  ),

  # Router / section manager script (migrated from index.html; without DOM-based fecha sync)
  tags$script(HTML(
    "(function () {\n      function showSection(target) {\n        document.querySelectorAll('.seccion').forEach(function(s) {\n          s.style.display = (s.dataset.seccion === target) ? '' : 'none';\n        });\n        document.querySelectorAll('.btn-seccion').forEach(function(b) {\n          b.setAttribute('aria-pressed', b.dataset.target === target ? 'true' : 'false');\n        });\n        history.replaceState(null, '', '#' + target);\n      }\n\n      document.addEventListener('DOMContentLoaded', function () {\n        var headerNav = document.querySelector('.header-nav');\n        if (headerNav) {\n          headerNav.querySelectorAll('a').forEach(function(a) {\n            var href = a.getAttribute('href') || '';\n            if (href.startsWith('#')) {\n              a.addEventListener('click', function (ev) { ev.preventDefault(); showSection(href.replace('#','')); });\n            } else if (href.endsWith('index.html') || href === './' || href === '/') {\n              a.setAttribute('href', '#home');\n              a.addEventListener('click', function (ev) { ev.preventDefault(); showSection('home'); });\n            }\n          });\n        }\n\n        var hash = location.hash.replace('#', '');\n        if (hash === 'origen') showSection('origen'); else showSection('home');\n      });\n    })();"
  ))
)
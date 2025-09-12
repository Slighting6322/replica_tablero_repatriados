# Tablero Repatriados (replica)

Instrucciones para ejecutar la aplicación Shiny localmente y diagnosticar problemas con la fecha de corte.

## Requisitos

- R (>= 4.0)
- Paquetes: `shiny`, `readr`, `dplyr`, `lubridate` (puedes instalar con `install.packages()` si falta alguno)

# Ejecutar la app

Desde la raíz del repositorio (la carpeta que contiene `app.R`), en una consola R ejecuta:

```r
# Cargar e inicializar (recomendado)
source("app.R")

# Alternativa: arrancar Shiny explícitamente desde la raíz
shiny::runApp('.', launch.browser = TRUE)
```

Desde la terminal (no interactiva):

```bash
Rscript -e "shiny::runApp('.', launch.browser = TRUE)"
```

# Diagnóstico rápido

La app ahora carga los datos desde `data/repatriados.xlsx` (hoja `Repatriados`) para construir los mapas de origen (EEUU) y destino (México).

Si la app muestra la fecha de hoy en lugar de la fecha de los datos, revisa lo siguiente en R:

```r
# Ver working directory
getwd()

# Comprobar que el XLSX existe
file.exists('data/repatriados.xlsx')

# Cargar utilidades y comprobar qué lee el loader
source('R/data_loader.R')
tb <- agg_repatriados_from_xlsx(path = 'data/repatriados.xlsx', sheet = 'Repatriados')
head(tb); nrow(tb)

# Fecha máxima calculada (si prefieres usar un CSV local, pásalo explícitamente)
# get_fecha_corte(path = 'path/to/your_repatriados.csv')
```

Si `file.exists()` devuelve `FALSE`, cambia tu working directory a la raíz del repo.

## Notas

- `app.R` inicializa `fecha_corte` y la asigna en el entorno global; por eso recomiendo ejecutar la app con `app.R`.
- Si prefieres poder ejecutar sólo `server.R`, podemos añadir un fallback que calcule `fecha_corte` desde el CSV cuando no exista; pregúntame si quieres que lo implemente.

---

README generado automáticamente.

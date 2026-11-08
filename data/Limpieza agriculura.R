# =============================================================================
# LIMPIEZA DE DATOS: agricultura.csv  ->  agricultura_limpia.xlsx / .csv
# =============================================================================
# Qué hace este script:
#   1. Lee el CSV completo como texto (para no perder información al importar)
#   2. Arregla los nombres de columnas (el archivo trae caracteres dañados: ?a, Producci?n)
#   3. Convierte los números: en este archivo el PUNTO es separador de miles
#      (ej. "3.333" = 3333, "1.050.000" = 1050000), no decimal
#   4. Limpia texto (espacios dobles en nombres de departamentos)
#   5. Separa la ña "1969/70" en año de inicio y año de fin
#   6. Agrega banderas (flags) de calidad SIN borrar ni modificar datos originales
#   7. Exporta a Excel (datos + diccionario + control de calidad) y a CSV limpio
#
# Paquetes necesarios (instalar una sola vez):
#   install.packages(c("readr", "dplyr", "stringr", "openxlsx"))
#
# Uso:
#   - Desde RStudio: ajusta las dos rutas de abajo y ejecuta todo (Ctrl+Shift+Enter)
#   - Desde terminal: Rscript limpieza_agricultura.R ruta/agricultura.csv carpeta_salida
# =============================================================================

library(readr)
library(dplyr)
library(stringr)
library(openxlsx)

# ---- 0. Rutas ---------------------------------------------------------------
archivo_entrada <- "agricultura.csv"   
carpeta_salida  <- "."                 # <- carpeta donde se guardarán los resultados

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) archivo_entrada <- args[1]
if (length(args) >= 2) carpeta_salida  <- args[2]
dir.create(carpeta_salida, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Lectura -------------------------------------------------------------
# Todo como texto: así R no interpreta mal "3.333" como 3.333
crudo <- read_csv(
  archivo_entrada,
  col_types = cols(.default = col_character()),
  locale = locale(encoding = "UTF-8"),
  na = c("", "NA", "N/A", "s/d", "S/D", "-")
)
n_original <- nrow(crudo)
stopifnot(ncol(crudo) == 14)

# ---- 2. Nombres de columnas -------------------------------------------------
# Se asignan por posición porque los nombres originales traen caracteres dañados
names(crudo) <- c(
  "id_provincia", "provincia",
  "id_departamento", "departamento",
  "id_cultivo", "cultivo",
  "id_na", "na",
  "temperatura_c",
  "sup_sembrada_ha", "sup_cosechada_ha",
  "produccion_tn", "rendimiento_kg_ha",
  "calidad"
)

# ---- 3. Limpieza de texto ---------------------------------------------------
# str_squish: quita espacios al inicio/fin y reduce espacios dobles a uno
# (ej. "25 DE  MAYO" -> "25 DE MAYO")
texto <- c("provincia", "departamento", "cultivo", "na", "calidad")
datos <- crudo |>
  mutate(across(all_of(texto), str_squish))

# ---- 4. Conversión numérica -------------------------------------------------
# Formato del archivo: punto = separador de miles
# parse_number con grouping_mark = "." lo interpreta correctamente
loc_ar <- locale(decimal_mark = ",", grouping_mark = ".")

num_miles <- function(x) parse_number(x, locale = loc_ar)

datos <- datos |>
  mutate(
    across(c(id_provincia, id_departamento, id_cultivo, id_na, temperatura_c),
           \(x) as.integer(x)),
    across(c(sup_sembrada_ha, sup_cosechada_ha, produccion_tn, rendimiento_kg_ha),
           num_miles)
  )

# ---- 5. ña -> años -----------------------------------------------------
# "1969/70" -> anio_inicio = 1969, anio_fin = 1970 ; "1999/00" -> 1999 y 2000
datos <- datos |>
  mutate(
    anio_inicio = as.integer(str_sub(na, 1, 4)),
    anio_fin    = anio_inicio + 1L
  )

# ---- 6. Variables auxiliares ------------------------------------------------
datos <- datos |>
  mutate(
    # id_departamento NO es único a nivel país: se repite entre provincias.
    # Esta clave (provincia-departamento) sí identifica cada departamento.
    clave_departamento = sprintf("%02d-%03d", id_provincia, id_departamento),
    # Calidad como factor ordenado (útil para gráficos y modelos)
    calidad = factor(calidad, levels = c("Baja", "Promedio", "Alta"), ordered = TRUE)
  )

# ---- 7. Banderas de calidad (no se elimina ni cambia ningún dato) -----------
rend_calculado <- if_else(datos$sup_cosechada_ha > 0,
                          datos$produccion_tn / datos$sup_cosechada_ha * 1000,
                          NA_real_)

datos <- datos |>
  mutate(
    # Sin siembra ni cosecha (fila con ceros)
    flag_sin_superficie = sup_sembrada_ha == 0 & sup_cosechada_ha == 0,
    # Se cosechó más superficie de la sembrada (no debería pasar)
    flag_cosechada_mayor_sembrada = sup_cosechada_ha > sup_sembrada_ha,
    # Hay producción pero superficie cosechada = 0 (dato incompleto)
    flag_prod_sin_sup_cosechada = sup_cosechada_ha == 0 & produccion_tn > 0,
    # Superficie cosechada > 0 pero producción = 0 (pérdida total)
    flag_perdida_total = sup_cosechada_ha > 0 & produccion_tn == 0,
    # El rendimiento reportado no coincide con producción / superficie (tolerancia 2 kg/ha)
    flag_rendimiento_inconsistente = !is.na(rend_calculado) &
      abs(rend_calculado - rendimiento_kg_ha) > 2
  )

# ---- 8. Orden de columnas y filas -------------------------------------------
datos <- datos |>
  select(
    id_provincia, provincia, id_departamento, clave_departamento, departamento,
    id_cultivo, cultivo, id_na, na, anio_inicio, anio_fin,
    temperatura_c, calidad,
    sup_sembrada_ha, sup_cosechada_ha, produccion_tn, rendimiento_kg_ha,
    starts_with("flag_")
  ) |>
  arrange(provincia, departamento, cultivo, anio_inicio)

# ---- 9. Control de calidad --------------------------------------------------
n_duplicados <- sum(duplicated(datos[c("id_provincia", "id_departamento",
                                       "id_cultivo", "id_na")]))

control <- tibble::tribble(
  ~control, ~resultado,
  "Filas en el archivo original",                         n_original,
  "Filas en el archivo limpio",                           nrow(datos),
  "Filas duplicadas (misma provincia-depto-cultivo-ña)", n_duplicados,
  "Valores vacíos (NA) en todo el archivo",               sum(is.na(datos)),
  "Años de ña desde",                                min(datos$anio_inicio),
  "Años de ña hasta",                                max(datos$anio_inicio),
  "Provincias",                                           n_distinct(datos$provincia),
  "Departamentos (clave única provincia-depto)",          n_distinct(datos$clave_departamento),
  "Cultivos",                                             n_distinct(datos$cultivo),
  "Filas sin superficie (sembrada = cosechada = 0)",      sum(datos$flag_sin_superficie),
  "Filas con cosechada > sembrada",                       sum(datos$flag_cosechada_mayor_sembrada),
  "Filas con producción pero cosechada = 0",              sum(datos$flag_prod_sin_sup_cosechada),
  "Filas con cosechada > 0 y producción = 0",             sum(datos$flag_perdida_total),
  "Filas con rendimiento inconsistente",                  sum(datos$flag_rendimiento_inconsistente)
) |>
  mutate(resultado = as.numeric(resultado))

print(as.data.frame(control), row.names = FALSE)

# ---- 10. Diccionario de datos -----------------------------------------------
diccionario <- tibble::tribble(
  ~variable, ~tipo, ~descripcion,
  "id_provincia",       "entero",  "Código numérico de la provincia",
  "provincia",          "texto",   "Nombre de la provincia",
  "id_departamento",    "entero",  "Código del departamento (solo único dentro de su provincia)",
  "clave_departamento", "texto",   "Clave única provincia-departamento (formato PP-DDD)",
  "departamento",       "texto",   "Nombre del departamento",
  "id_cultivo",         "entero",  "Código numérico del cultivo",
  "cultivo",            "texto",   "Nombre del cultivo",
  "id_na",         "entero",  "Código numérico de la ña (1 = 1969/70)",
  "na",            "texto",   "ña agrícola (ej. 1969/70)",
  "anio_inicio",        "entero",  "Año en que inicia la ña",
  "anio_fin",           "entero",  "Año en que termina la ña",
  "temperatura_c",      "entero",  "Temperatura en grados Celsius (según el archivo original)",
  "calidad",            "factor",  "Calidad registrada: Baja < Promedio < Alta",
  "sup_sembrada_ha",    "numérico","Superficie sembrada (hectáreas)",
  "sup_cosechada_ha",   "numérico","Superficie cosechada (hectáreas)",
  "produccion_tn",      "numérico","Producción (toneladas)",
  "rendimiento_kg_ha",  "numérico","Rendimiento (kg por hectárea)",
  "flag_sin_superficie",             "lógico", "TRUE si sembrada y cosechada son 0",
  "flag_cosechada_mayor_sembrada",   "lógico", "TRUE si cosechada > sembrada",
  "flag_prod_sin_sup_cosechada",     "lógico", "TRUE si hay producción pero cosechada = 0",
  "flag_perdida_total",              "lógico", "TRUE si hay cosechada pero producción = 0",
  "flag_rendimiento_inconsistente",  "lógico", "TRUE si rendimiento ≠ producción/cosechada*1000 (tol. 2)"
)

# ---- 11. Exportación --------------------------------------------------------
# CSV limpio (UTF-8) - ideal para volver a leer rápido en R
write_csv(datos, file.path(carpeta_salida, "agricultura_limpia.csv"))

# Excel con tres hojas
wb <- createWorkbook()
estilo_enc <- createStyle(textDecoration = "bold", fgFill = "#1F4E78",
                          fontColour = "#FFFFFF", halign = "center")

agregar_hoja <- function(wb, nombre, df, anchos = "auto") {
  addWorksheet(wb, nombre)
  writeData(wb, nombre, df, headerStyle = estilo_enc)
  freezePane(wb, nombre, firstRow = TRUE)
  addFilter(wb, nombre, row = 1, cols = seq_len(ncol(df)))
  setColWidths(wb, nombre, cols = seq_len(ncol(df)), widths = anchos)
}

# Las banderas lógicas se escriben como SI/NO para que sean más legibles en Excel
datos_excel <- datos |>
  mutate(
    calidad = as.character(calidad),
    across(starts_with("flag_"), \(x) if_else(x, "SI", "NO"))
  )

agregar_hoja(wb, "datos", datos_excel)
agregar_hoja(wb, "diccionario", diccionario, anchos = c(32, 12, 70))
agregar_hoja(wb, "control_calidad", as.data.frame(control), anchos = c(60, 14))

# Formato de miles en columnas numéricas grandes
fmt_miles <- createStyle(numFmt = "#,##0")
cols_num <- which(names(datos_excel) %in%
                    c("sup_sembrada_ha", "sup_cosechada_ha",
                      "produccion_tn", "rendimiento_kg_ha"))
addStyle(wb, "datos", fmt_miles, rows = 2:(nrow(datos_excel) + 1),
         cols = cols_num, gridExpand = TRUE)

saveWorkbook(wb, file.path(carpeta_salida, "agricultura_limpia.xlsx"), overwrite = TRUE)

message("Listo. Archivos guardados en: ", normalizePath(carpeta_salida))


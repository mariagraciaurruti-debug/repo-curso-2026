
paquetes <- c("edgar", "dplyr", "tidytext", "stringr", "xml2", "rvest")

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]
if (length(faltantes) > 0) {
  install.packages(faltantes, repos = "https://cloud.r-project.org")
}

library(dplyr)
library(tidytext)

useragent <- "Maria Gracia Urruti mariagraciaurruti@gmail.com"
anio_presentacion <- 2025

# elegi 5 empresas argentinas que cotizan en el NYSE para realizar la tarea
# Grupo Financiero Galicia, Banco Macro, Banco BBVA Argentina, YPF, Vista Energy

empresas <- data.frame(
  empresa = c(
    "Grupo Financiero Galicia",
    "Banco Macro",
    "Banco BBVA Argentina",
    "YPF",
    "Vista Energy"
  ),
  ticker = c("GGAL", "BMA", "BBAR", "YPF", "VIST"),
  cik = c("1114700", "1347426", "913059", "904851", "1762506"),
  stringsAsFactors = FALSE
)

descargas <- edgar::getFilings(
  cik.no = empresas$cik,
  form.type = "20-F",
  filing.year = anio_presentacion,
  quarter = 1:4,
  downl.permit = "y",
  useragent = useragent
)

if (is.null(descargas) || nrow(descargas) == 0) {
  stop("No se encontraron informes 20-F. Revisá el año, los CIK y la conexión a internet.")
}

print(descargas)
write.csv(descargas, "estado_descargas.csv", row.names = FALSE)

if (!"status" %in% names(descargas)) {
  stop("La respuesta de edgar no incluye la columna 'status'; revisá el resultado de descargas.")
}

descargas_ok <- descargas[
  !is.na(descargas$status) &
    descargas$status == "Download success" &
    descargas$form.type == "20-F",
]

if (nrow(descargas_ok) == 0) {
  stop("No se descargó ningún informe correctamente. Revisá el User-Agent y la salida anterior.")
}

palabras_clave <- c(
  "inflation",
  "devaluation",
  "exchange controls",
  "default",
  "restructuring",
  "imf",
  "capital controls",
  "price controls",
  "country risk",
  "embi"
)

incidencia <- edgar::searchFilings(
  cik.no = empresas$cik,
  form.type = "20-F",
  filing.year = anio_presentacion,
  word.list = palabras_clave,
  useragent = useragent
)

if (is.null(incidencia) || nrow(incidencia) == 0) {
  stop("No se generó la tabla de palabras clave; revisá que haya informes descargados.")
}

if ("cik" %in% names(incidencia)) {
  incidencia$empresa <- empresas$empresa[
    match(as.character(incidencia$cik), empresas$cik)
  ]
}

print(incidencia)
write.csv(incidencia, "incidencia_palabras_clave.csv", row.names = FALSE)

sentimientos <- edgar::getSentiment(
  cik.no = empresas$cik,
  form.type = "20-F",
  filing.year = anio_presentacion,
  useragent = useragent
)

if (is.null(sentimientos) || nrow(sentimientos) == 0) {
  stop("No se generó el análisis de sentimiento; revisá los informes descargados.")
}

if ("cik" %in% names(sentimientos)) {
  sentimientos$empresa <- empresas$empresa[
    match(as.character(sentimientos$cik), empresas$cik)
  ]
}

categorias <- c(
  "lm.negative.count",
  "lm.positive.count",
  "lm.uncertainty.count",
  "lm.litigious.count"
)

if (!all(categorias %in% names(sentimientos))) {
  stop("La salida de edgar no contiene todas las columnas de sentimiento esperadas.")
}

for (columna in categorias) {
  sentimientos[[paste0(columna, "_cada_1000_palabras")]] <-
    1000 * sentimientos[[columna]] / sentimientos$word.count
}

etiquetas <- c(
  "lm.negative.count" = "negativo",
  "lm.positive.count" = "positivo",
  "lm.uncertainty.count" = "incertidumbre",
  "lm.litigious.count" = "litigioso"
)

sentimientos$sentimiento_principal <- apply(
  sentimientos[, categorias, drop = FALSE],
  1,
  function(x) {
    categoria <- names(etiquetas)[which.max(as.numeric(x))]
    unname(etiquetas[categoria])
  }
)

print(sentimientos)
write.csv(sentimientos, "analisis_sentimiento.csv", row.names = FALSE)

descargas_ok <- descargas_ok[
  order(descargas_ok$cik, descargas_ok$date.filed),
]

descargas_ok <- descargas_ok[
  !duplicated(descargas_ok$cik, fromLast = TRUE),
]

extraer_texto_20f <- function(ruta) {
  lineas <- readLines(ruta, warn = FALSE, encoding = "UTF-8")
  
  tipo <- grep(
    "^\\s*<TYPE>\\s*20-F\\s*$",
    lineas,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  if (length(tipo) == 0) {
    stop(paste("No encuentro la etiqueta <TYPE>20-F en:", ruta))
  }
  
  inicio_texto <- grep(
    "^\\s*<TEXT>\\s*$",
    lineas,
    ignore.case = TRUE,
    perl = TRUE
  )
  inicio_texto <- inicio_texto[inicio_texto > tipo[1]]
  
  if (length(inicio_texto) == 0) {
    stop(paste("No encuentro la etiqueta <TEXT> después del 20-F en:", ruta))
  }
  
  fin_texto <- grep(
    "^\\s*</TEXT>\\s*$",
    lineas,
    ignore.case = TRUE,
    perl = TRUE
  )
  fin_texto <- fin_texto[fin_texto > inicio_texto[1]]
  
  if (length(fin_texto) == 0) {
    stop(paste("No encuentro el cierre </TEXT> del 20-F en:", ruta))
  }
  
  contenido <- paste(
    lineas[(inicio_texto[1] + 1):(fin_texto[1] - 1)],
    collapse = "\n"
  )
  
  pagina <- xml2::read_html(charToRaw(contenido))
  rvest::html_text2(pagina)
}

getwd()

list.files(
  ".",
  recursive = TRUE,
  pattern = "20-F.*\\.txt$",
  full.names = TRUE
)

informes <- lapply(seq_len(nrow(descargas_ok)), function(i) {
  fila <- descargas_ok[i, ]
  cik <- as.character(fila$cik)
  
  carpeta <- file.path("edgar_Filings", "Form 20-F", cik)
  archivos <- list.files(
    carpeta,
    pattern = paste0("^", cik, "_20-F_.*\\.txt$"),
    full.names = TRUE
  )
  
  if (length(archivos) == 0) {
    stop(paste("No encuentro el informe 20-F en:", carpeta))
  }
  
  nombre_empresa <- empresas$empresa[match(cik, empresas$cik)]
  
  data.frame(
    empresa = nombre_empresa,
    texto = extraer_texto_20f(archivos[1]),
    stringsAsFactors = FALSE
  )
})

informes <- dplyr::bind_rows(informes)

ruta <- "edgar_Filings/Form 20-F/1114700/1114700_20-F_2025-04-28_0001114700-25-000049.txt"

lineas <- readLines(ruta, warn = FALSE, encoding = "UTF-8")

grep(
  "DOCUMENT|TYPE|TEXT|20-F",
  lineas,
  ignore.case = TRUE,
  value = TRUE
)[1:30]

ruta <- "edgar_Filings/Form 20-F/1114700/1114700_20-F_2025-04-28_0001114700-25-000049.txt"

lineas <- readLines(ruta, warn = FALSE, encoding = "UTF-8")

grep(
  "DOCUMENT|TYPE|TEXT|20-F",
  lineas,
  ignore.case = TRUE,
  value = TRUE
)[1:30]

# esto ultimo lo tuve que hacer porque tenia problema para encontrar los archivos

informes <- lapply(seq_len(nrow(descargas_ok)), function(i) {
  fila <- descargas_ok[i, ]
  
  cik <- as.character(fila$cik)
  fecha <- as.character(fila$date.filed)
  accession <- as.character(fila$accession.number)
  
  nombre_archivo <- paste0(
    cik, "_20-F_", fecha, "_", accession, ".txt"
  )
  
  ruta <- file.path(
    "Edgar filings_full text",
    "Form 20-F",
    cik,
    nombre_archivo
  )
  
  if (!file.exists(ruta)) {
    stop(paste("No encuentro el archivo descargado:", ruta))
  }
  nombre_empresa <- empresas$empresa[
    match(cik, empresas$cik)
  ]
  
  data.frame(
    empresa = nombre_empresa,
    texto = extraer_texto_20f(ruta),
    stringsAsFactors = FALSE
  )
})  

informes <- dplyr::bind_rows(informes)

if (nrow(informes) == 0) {
  stop("No quedó ningún informe para calcular TF-IDF.")
}
 

objetos_necesarios <- c("incidencia", "sentimientos", "top_tfidf")
faltan <- objetos_necesarios[
  !vapply(objetos_necesarios, exists, logical(1), envir = .GlobalEnv)
]

if (length(faltan) > 0) {
  stop(paste("Todavía no existen estos resultados:", paste(faltan, collapse = ", ")))
}

write.csv(incidencia, "incidencia_palabras_clave.csv", row.names = FALSE)
write.csv(sentimientos, "analisis_sentimiento.csv", row.names = FALSE)
write.csv(top_tfidf, "top_terminos_tfidf.csv", row.names = FALSE)

print(incidencia)
print(sentimientos)
print(top_tfidf)

cat("\nListo. Los CSV se guardaron en:", getwd(), "\n")

# Con tus resultados, **los bancos son los que más mencionan los temas económicos buscados**.
# En cantidad total lidera Banco Macro (404 menciones), seguido por Galicia (390) y BBVA Argentina (321).
# Pero como los informes tienen distinta extensión, la comparación por cada 1.000 palabras pone primero 
# a BBVA (~2,76), luego a Galicia (~2,60) y Macro (~2,53). Vista Energy (~1,07) e YPF (~0,93) tienen menos menciones relativas.

# En el análisis Loughran–McDonald predomina el sentimiento **negativo** en Galicia, Macro, Vista y BBVA. En YPF predomina 
# el componente **litigioso**. Esto describe las palabras del informe según el diccionario financiero; no significa por sí 
# solo que la empresa esté en una situación negativa.
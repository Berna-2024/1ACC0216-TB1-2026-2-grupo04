# ==========================================================
# UPC - Fundamentos de Data Science (1ACC0216) - Ciclo 2026-2
# TB1 - Análisis exploratorio: Hotel Booking Demand
# Grupo 03
#
# Preguntas analíticas:
#  P5: ¿Cómo se distribuyen las reservas y cancelaciones según
#      market_segment y customer_type?
#  P6: ¿Qué relación se observa entre lead_time y adr?
#
# Estructura del proyecto (carpeta raíz):
#   1ACC0216-TB1-2026-2-grupo03/
#     data/hotel_bookings.csv            
#     data/hotel_bookings_preparado.csv
#     code/upc-grupo03-tb1-codigo.R
#     output/gráficos/                   (PNG generados por este script)
#
# ==========================================================


# ==========================================================
# 0. CONFIGURACIÓN
# ==========================================================

# Ubica el directorio de trabajo en la raíz del proyecto
# a partir de la ubicación de este script (carpeta code/).
if (!file.exists("data/hotel_bookings.csv") &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))
}

if (!file.exists("data/hotel_bookings.csv")) {
  stop("No se encuentra data/hotel_bookings.csv. ",
       "Coloca el directorio de trabajo en la carpeta raíz del proyecto ",
       "(Session > Set Working Directory > Choose Directory).")
}

dir_datos  <- "data"
dir_graf   <- file.path("output", "gráficos")
dir.create(dir_graf, recursive = TRUE, showWarnings = FALSE)

# TRUE: cada gráfico se muestra en RStudio y además se guarda como PNG.
# Si aparece el error "figure margins too large", ampliar el panel
# Plots o poner FALSE (solo se guardan los PNG).
MOSTRAR_EN_PANTALLA <- TRUE

# Función auxiliar: dibuja el gráfico (pantalla) y lo guarda en PNG
grafico <- function(nombre, codigo, ancho = 1000, alto = 700) {
  expr <- substitute(codigo)
  env  <- parent.frame()
  if (MOSTRAR_EN_PANTALLA) eval(expr, env)
  png(file.path(dir_graf, nombre), width = ancho, height = alto, res = 120)
  eval(expr, env)
  invisible(dev.off())
}


# ==========================================================
# 1. CARGA DEL DATASET ORIGINAL (no se modifica el archivo)
# ==========================================================

hotel_bookings <- read.csv(
  file.path(dir_datos, "hotel_bookings.csv"),
  stringsAsFactors = FALSE
)

dim(hotel_bookings)
str(hotel_bookings)
head(hotel_bookings)


# ==========================================================
# 2. CALIDAD DE DATOS (sobre el dataset original)
# ==========================================================

# ----------------------------------------------------------
# 2.1 COMPLETITUD
# ----------------------------------------------------------

sum(is.na(hotel_bookings))

faltante <- colSums(is.na(hotel_bookings))
faltante[faltante > 0]

unique(hotel_bookings$agent)
unique(hotel_bookings$company)
unique(hotel_bookings$country)

# Revisar los valores de cada columna
table(hotel_bookings$country, useNA = "ifany")
table(hotel_bookings$agent, useNA = "ifany")
table(hotel_bookings$company, useNA = "ifany")

# Los que tienen NULL en las filas de cada columna
sum(hotel_bookings$country == "NULL", na.rm = TRUE)
sum(hotel_bookings$agent == "NULL", na.rm = TRUE)
sum(hotel_bookings$company == "NULL", na.rm = TRUE)

# sapply recorre cada una de las columnas
# Cantidad de valores escritos como NULL por columna
null_texto <- sapply(hotel_bookings, function(x) {
  if (is.character(x)) {
    sum(x == "NULL", na.rm = TRUE)
  } else {
    0
  }
})

null_texto[null_texto > 0]

# Cantidad de valores escritos como "" por columna
vacios <- sapply(hotel_bookings, function(x) {
  if (is.character(x)) {
    sum(trimws(x) == "", na.rm = TRUE)
  } else {
    0
  }
})

vacios[vacios > 0]

# Porcentaje de faltantes (NA, vacío o "NULL")
porcentaje_faltantes <- sapply(hotel_bookings, function(x) {
  
  if (is.character(x)) {
    
    faltantes <- sum(
      is.na(x) |
        trimws(x) == "" |
        x == "NULL",
      na.rm = TRUE
    )
    
  } else {
    
    faltantes <- sum(is.na(x))
    
  }
  
  faltantes / nrow(hotel_bookings) * 100
})

porcentaje_faltantes[porcentaje_faltantes > 0]

# Tabla de porcentaje de datos faltantes
tabla_faltantes <- data.frame(
  Variable = names(porcentaje_faltantes),
  Porcentaje = round(porcentaje_faltantes, 3)
)

tabla_faltantes[
  tabla_faltantes$Porcentaje > 0,
]

# ----------------------------------------------------------
# 2.2 UNICIDAD
# ----------------------------------------------------------

# Cantidad de filas completamente duplicadas
sum(duplicated(hotel_bookings))

head(hotel_bookings[duplicated(hotel_bookings), ])

# ----------------------------------------------------------
# 2.3 VALIDEZ
# ----------------------------------------------------------

# Comprobar si hay valores negativos
sum(hotel_bookings$adults < 0, na.rm = TRUE)
sum(hotel_bookings$children < 0, na.rm = TRUE)
sum(hotel_bookings$babies < 0, na.rm = TRUE)
sum(hotel_bookings$lead_time < 0, na.rm = TRUE)
sum(hotel_bookings$stays_in_weekend_nights < 0, na.rm = TRUE)
sum(hotel_bookings$stays_in_week_nights < 0, na.rm = TRUE)
sum(hotel_bookings$required_car_parking_spaces < 0, na.rm = TRUE)
sum(hotel_bookings$total_of_special_requests < 0, na.rm = TRUE)

# Valores extremos
summary(hotel_bookings$adr)

# Si los valores binarios están correctos
sum(
  !hotel_bookings$is_canceled %in% c(0, 1),
  na.rm = TRUE
)

# Si los meses están correctos
table(hotel_bookings$arrival_date_month)

# Días dentro del rango
sum(
  hotel_bookings$arrival_date_day_of_month < 1 |
    hotel_bookings$arrival_date_day_of_month > 31,
  na.rm = TRUE
)

# Reservas sin ninguna persona
total_huespedes <-
  hotel_bookings$adults +
  hotel_bookings$children +
  hotel_bookings$babies

sum(total_huespedes == 0, na.rm = TRUE)

# Reservas con cero noches
total_noches <-
  hotel_bookings$stays_in_weekend_nights +
  hotel_bookings$stays_in_week_nights

sum(total_noches == 0, na.rm = TRUE)

# ----------------------------------------------------------
# 2.4 CONSISTENCIA
# ----------------------------------------------------------

head(
  paste(
    hotel_bookings$arrival_date_year,
    hotel_bookings$arrival_date_month,
    hotel_bookings$arrival_date_day_of_month
  )
)

numero_mes <- match(
  hotel_bookings$arrival_date_month,
  month.name
)
head(numero_mes)

fecha_llegada <- as.Date(
  paste(
    hotel_bookings$arrival_date_year,
    numero_mes,
    hotel_bookings$arrival_date_day_of_month,
    sep = "-"
  ),
  format = "%Y-%m-%d"
)

head(fecha_llegada)
sum(is.na(fecha_llegada))


# ==========================================================
# 3. PREPARACIÓN Y TRANSFORMACIÓN DE DATOS
#    (se trabaja sobre una copia: hotel_preparado)
# ==========================================================

hotel_preparado <- hotel_bookings

head(hotel_preparado$hotel)

# ----------------------------------------------------------
# Conversión a factor
# ----------------------------------------------------------

hotel_preparado$hotel <- factor(hotel_preparado$hotel)

hotel_preparado$is_canceled <- factor(
  hotel_preparado$is_canceled,
  levels = c(0, 1),
  labels = c("No", "Sí")
)

hotel_preparado$meal <- factor(hotel_preparado$meal)

hotel_preparado$market_segment <- factor(
  hotel_preparado$market_segment
)

hotel_preparado$distribution_channel <- factor(
  hotel_preparado$distribution_channel
)

hotel_preparado$deposit_type <- factor(
  hotel_preparado$deposit_type
)

hotel_preparado$customer_type <- factor(
  hotel_preparado$customer_type
)

hotel_preparado$reservation_status <- factor(
  hotel_preparado$reservation_status
)

# ----------------------------------------------------------
# Convertir valores "NULL" a NA
# ----------------------------------------------------------

hotel_preparado$country[
  hotel_preparado$country == "NULL"
] <- NA

hotel_preparado$agent[
  hotel_preparado$agent == "NULL"
] <- NA

hotel_preparado$company[
  hotel_preparado$company == "NULL"
] <- NA

# ----------------------------------------------------------
# Crear fecha de llegada
# ----------------------------------------------------------

numero_mes <- match(
  hotel_preparado$arrival_date_month,
  month.name
)

hotel_preparado$arrival_date <- as.Date(
  paste(
    hotel_preparado$arrival_date_year,
    numero_mes,
    hotel_preparado$arrival_date_day_of_month,
    sep = "-"
  ),
  format = "%Y-%m-%d"
)

head(hotel_preparado$arrival_date)
class(hotel_preparado$arrival_date)

str(hotel_preparado)

# ----------------------------------------------------------
# Guardar dataset preparado (el original no se sobrescribe)
# ----------------------------------------------------------

write.csv(
  hotel_preparado,
  file.path(dir_datos, "hotel_bookings_preparado.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ==========================================================
# 4. ANÁLISIS UNIVARIADO
# ==========================================================

# ----------------------------------------------------------
# 4.1 MARKET SEGMENT
# ----------------------------------------------------------

frecuencia_segmento <- table(
  hotel_preparado$market_segment
)

frecuencia_segmento

porcentaje_segmento <- prop.table(
  frecuencia_segmento
) * 100

round(porcentaje_segmento, 2)

valores_miles <- frecuencia_segmento / 1000

grafico("01_reservas_por_segmento.png", {
  posiciones <- barplot(
    frecuencia_segmento / 1000,
    main = "Cantidad de reservas por segmento de mercado",
    xlab = "Segmento de mercado",
    ylab = "Cantidad de reservas (miles)",
    las = 2,
    ylim = c(0, max(valores_miles) * 1.15)
  )
  
  text(
    x = posiciones,
    y = frecuencia_segmento / 1000,
    labels = round(frecuencia_segmento / 1000, 1),
    pos = 3
  )
})

# ----------------------------------------------------------
# 4.2 CUSTOMER TYPE
# ----------------------------------------------------------

frecuencia_cliente <- table(
  hotel_preparado$customer_type
)

frecuencia_cliente

porcentaje_cliente <- prop.table(
  frecuencia_cliente
) * 100

round(porcentaje_cliente, 2)

tabla_cliente <- data.frame(
  Tipo_Cliente = names(frecuencia_cliente),
  Frecuencia = as.vector(frecuencia_cliente),
  Porcentaje = round(
    as.vector(porcentaje_cliente),
    2
  )
)

tabla_cliente

valores_cliente_miles <- frecuencia_cliente / 1000

grafico("02_reservas_por_tipo_cliente.png", {
  posiciones_cliente <- barplot(
    valores_cliente_miles,
    main = "Cantidad de reservas por tipo de cliente",
    xlab = "Tipo de cliente",
    ylab = "Cantidad de reservas (miles)",
    ylim = c(0, max(valores_cliente_miles) * 1.15)
  )
  
  text(
    x = posiciones_cliente,
    y = valores_cliente_miles,
    labels = round(valores_cliente_miles, 1),
    pos = 3
  )
})

# ----------------------------------------------------------
# 4.3 CANCELACIÓN (is_canceled)
# ----------------------------------------------------------

frecuencia_cancel <- table(hotel_preparado$is_canceled)

frecuencia_cancel

round(prop.table(frecuencia_cancel) * 100, 2)

valores_cancel_miles <- frecuencia_cancel / 1000

grafico("03_reservas_por_cancelacion.png", {
  posiciones_cancel <- barplot(
    valores_cancel_miles,
    main = "Cantidad de reservas según cancelación",
    xlab = "¿Reserva cancelada?",
    ylab = "Cantidad de reservas (miles)",
    ylim = c(0, max(valores_cancel_miles) * 1.15)
  )
  
  text(
    x = posiciones_cancel,
    y = valores_cancel_miles,
    labels = round(valores_cancel_miles, 1),
    pos = 3
  )
}, ancho = 800)


# ==========================================================
# 5. VALORES ATÍPICOS (método IQR: 1.5 * IQR)
# ==========================================================

# ----------------------------------------------------------
# 5.1 VALORES ATÍPICOS DE LEAD_TIME
# ----------------------------------------------------------

summary(hotel_preparado$lead_time)


# Calcular Q1 y Q3
Q1_lead <- quantile(
  hotel_preparado$lead_time,
  0.25,
  na.rm = TRUE
)

Q3_lead <- quantile(
  hotel_preparado$lead_time,
  0.75,
  na.rm = TRUE
)

Q1_lead
Q3_lead


# Calcular rango intercuartílico (IQR)
IQR_lead <- Q3_lead - Q1_lead

IQR_lead


# Calcular límites
limite_inferior_lead <- Q1_lead - 1.5 * IQR_lead
limite_superior_lead <- Q3_lead + 1.5 * IQR_lead

limite_inferior_lead
limite_superior_lead


# Identificar valores atípicos
atipicos_lead <- hotel_preparado$lead_time[
  !is.na(hotel_preparado$lead_time) &
    (
      hotel_preparado$lead_time < limite_inferior_lead |
        hotel_preparado$lead_time > limite_superior_lead
    )
]

length(atipicos_lead)


# Porcentaje de valores atípicos
porcentaje_atipicos_lead <- (
  length(atipicos_lead) /
    nrow(hotel_preparado)
) * 100

round(porcentaje_atipicos_lead, 2)


# Cantidad de reservas dentro de los límites
cantidad_dentro_lead <- sum(
  hotel_preparado$lead_time >= limite_inferior_lead &
    hotel_preparado$lead_time <= limite_superior_lead,
  na.rm = TRUE
)

cantidad_dentro_lead


# Cantidad de reservas fuera de los límites
cantidad_fuera_lead <- sum(
  hotel_preparado$lead_time < limite_inferior_lead |
    hotel_preparado$lead_time > limite_superior_lead,
  na.rm = TRUE
)

cantidad_fuera_lead


# Porcentaje dentro de los límites
porcentaje_dentro_lead <- (
  cantidad_dentro_lead /
    nrow(hotel_preparado)
) * 100

round(porcentaje_dentro_lead, 2)


# Valor máximo (referencia para interpretar si es plausible)
max(hotel_preparado$lead_time, na.rm = TRUE)


# Gráfico (la línea roja marca el límite superior)
grafico("04_boxplot_lead_time.png", {
  boxplot(
    hotel_preparado$lead_time,
    main = "Valores atípicos en la anticipación de reserva",
    ylab = "Días de anticipación"
  )
  
  abline(h = limite_superior_lead, lty = 2, col = "red", lwd = 2)
}, ancho = 800, alto = 700)


# ----------------------------------------------------------
# 5.2 VALORES ATÍPICOS DE ADR
# ----------------------------------------------------------

summary(hotel_preparado$adr)


# Calcular Q1 y Q3
Q1_adr <- quantile(
  hotel_preparado$adr,
  0.25,
  na.rm = TRUE
)

Q3_adr <- quantile(
  hotel_preparado$adr,
  0.75,
  na.rm = TRUE
)

Q1_adr
Q3_adr


# Calcular rango intercuartílico
IQR_adr <- Q3_adr - Q1_adr

IQR_adr


# Calcular límites
limite_inferior_adr <- Q1_adr - 1.5 * IQR_adr
limite_superior_adr <- Q3_adr + 1.5 * IQR_adr

limite_inferior_adr
limite_superior_adr


# Identificar valores atípicos
atipicos_adr <- hotel_preparado$adr[
  !is.na(hotel_preparado$adr) &
    (
      hotel_preparado$adr < limite_inferior_adr |
        hotel_preparado$adr > limite_superior_adr
    )
]

length(atipicos_adr)


# Porcentaje de valores atípicos
porcentaje_atipicos_adr <- (
  length(atipicos_adr) /
    nrow(hotel_preparado)
) * 100

round(porcentaje_atipicos_adr, 2)


# Cantidad dentro de los límites
cantidad_dentro_adr <- sum(
  hotel_preparado$adr >= limite_inferior_adr &
    hotel_preparado$adr <= limite_superior_adr,
  na.rm = TRUE
)

cantidad_dentro_adr


# Cantidad fuera de los límites
cantidad_fuera_adr <- sum(
  hotel_preparado$adr < limite_inferior_adr |
    hotel_preparado$adr > limite_superior_adr,
  na.rm = TRUE
)

cantidad_fuera_adr


# Porcentaje dentro de los límites
porcentaje_dentro_adr <- (
  cantidad_dentro_adr /
    nrow(hotel_preparado)
) * 100

round(porcentaje_dentro_adr, 2)


# Revisar valores atípicos extremos
head(
  sort(atipicos_adr),
  10
)

tail(
  sort(atipicos_adr),
  10
)


# Casos a revisar: ADR negativo, ADR cero y ADR extremo
sum(hotel_preparado$adr < 0, na.rm = TRUE)
sum(hotel_preparado$adr == 0, na.rm = TRUE)
sum(hotel_preparado$adr >= 1000, na.rm = TRUE)
max(hotel_preparado$adr, na.rm = TRUE)

# Registros del ADR negativo y del ADR extremo (para su revisión)
hotel_preparado[
  hotel_preparado$adr < 0 | hotel_preparado$adr >= 1000,
  c("hotel", "is_canceled", "lead_time", "arrival_date",
    "adults", "market_segment", "customer_type", "adr")
]


# Gráfico (eje truncado en 500 para ver la caja; la línea roja
# marca el límite superior)
grafico("05_boxplot_adr.png", {
  boxplot(
    hotel_preparado$adr,
    main = "Valores atípicos en la tarifa diaria promedio",
    ylab = "ADR",
    sub = "Eje truncado en 500; el valor máximo del dataset es 5400",
    ylim = c(0, 500)
  )
  
  abline(h = limite_superior_adr, lty = 2, col = "red", lwd = 2)
}, ancho = 800, alto = 700)


# ==========================================================
# 6. ANÁLISIS BIVARIADO Y MULTIVARIADO
# ==========================================================

# ----------------------------------------------------------
# 6.1 PREGUNTA 5: MARKET SEGMENT vs CANCELACIÓN
# ----------------------------------------------------------

tasa_global <- mean(hotel_preparado$is_canceled == "Sí") * 100
round(tasa_global, 2)


frecuencia_seg_cancel <- table(
  hotel_preparado$market_segment,
  hotel_preparado$is_canceled
)

tabla_seg_cancel <- data.frame(
  Segmento = rownames(frecuencia_seg_cancel),
  Reservas = rowSums(frecuencia_seg_cancel),
  Canceladas = frecuencia_seg_cancel[, "Sí"],
  Pct_Reservas = round(
    rowSums(frecuencia_seg_cancel) / sum(frecuencia_seg_cancel) * 100, 2
  ),
  Pct_Cancelaciones = round(
    frecuencia_seg_cancel[, "Sí"] / sum(frecuencia_seg_cancel[, "Sí"]) * 100, 2
  ),
  Tasa_Cancelacion = round(
    frecuencia_seg_cancel[, "Sí"] / rowSums(frecuencia_seg_cancel) * 100, 2
  ),
  row.names = NULL
)

tabla_seg_cancel

# Gráfico (se excluyen segmentos con 30 reservas o menos)
tasa_seg <- tabla_seg_cancel[tabla_seg_cancel$Reservas > 30, ]

grafico("06_tasa_cancelacion_por_segmento.png", {
  par(mar = c(9, 4, 4, 2))
  
  posiciones_seg <- barplot(
    tasa_seg$Tasa_Cancelacion,
    names.arg = tasa_seg$Segmento,
    main = "Tasa de cancelación por segmento de mercado",
    ylab = "Tasa de cancelación (%)",
    las = 2,
    ylim = c(0, max(tasa_seg$Tasa_Cancelacion) * 1.15)
  )
  
  text(
    x = posiciones_seg,
    y = tasa_seg$Tasa_Cancelacion,
    labels = tasa_seg$Tasa_Cancelacion,
    pos = 1,
    cex = 0.8
  )
  
  abline(h = tasa_global, lty = 2, lwd = 2)
  mtext("Segmento de mercado", side = 1, line = 7)
  
  par(mar = c(5, 4, 4, 2) + 0.1)
})

# ----------------------------------------------------------
# 6.2 PREGUNTA 5: CUSTOMER TYPE vs CANCELACIÓN
# ----------------------------------------------------------

frecuencia_cli_cancel <- table(
  hotel_preparado$customer_type,
  hotel_preparado$is_canceled
)

tabla_cli_cancel <- data.frame(
  Tipo_Cliente = rownames(frecuencia_cli_cancel),
  Reservas = rowSums(frecuencia_cli_cancel),
  Canceladas = frecuencia_cli_cancel[, "Sí"],
  Pct_Reservas = round(
    rowSums(frecuencia_cli_cancel) / sum(frecuencia_cli_cancel) * 100, 2
  ),
  Pct_Cancelaciones = round(
    frecuencia_cli_cancel[, "Sí"] / sum(frecuencia_cli_cancel[, "Sí"]) * 100, 2
  ),
  Tasa_Cancelacion = round(
    frecuencia_cli_cancel[, "Sí"] / rowSums(frecuencia_cli_cancel) * 100, 2
  ),
  row.names = NULL
)

tabla_cli_cancel

grafico("07_tasa_cancelacion_por_tipo_cliente.png", {
  posiciones_cli <- barplot(
    tabla_cli_cancel$Tasa_Cancelacion,
    names.arg = tabla_cli_cancel$Tipo_Cliente,
    main = "Tasa de cancelación por tipo de cliente",
    xlab = "Tipo de cliente",
    ylab = "Tasa de cancelación (%)",
    cex.names = 0.8,
    ylim = c(0, max(tabla_cli_cancel$Tasa_Cancelacion) * 1.15)
  )
  
  text(
    x = posiciones_cli,
    y = tabla_cli_cancel$Tasa_Cancelacion,
    labels = tabla_cli_cancel$Tasa_Cancelacion,
    pos = 3,
    cex = 0.8
  )
  
  abline(h = tasa_global, lty = 2, lwd = 2)
})

# ----------------------------------------------------------
# 6.3 MULTIVARIADO: segmento x tipo de cliente y segmento x depósito
# ----------------------------------------------------------

n_seg_cliente <- table(
  hotel_preparado$market_segment,
  hotel_preparado$customer_type
)

tasa_seg_cliente <- tapply(
  hotel_preparado$is_canceled == "Sí",
  list(hotel_preparado$market_segment, hotel_preparado$customer_type),
  mean
) * 100

# Se omiten combinaciones con menos de 100 reservas
tasa_seg_cliente[n_seg_cliente < 100] <- NA
round(tasa_seg_cliente, 1)

# Tipo de depósito
frecuencia_dep_cancel <- table(
  hotel_preparado$deposit_type,
  hotel_preparado$is_canceled
)

frecuencia_dep_cancel
round(prop.table(frecuencia_dep_cancel, 1) * 100, 2)

n_seg_dep <- table(
  hotel_preparado$market_segment,
  hotel_preparado$deposit_type
)

round(prop.table(n_seg_dep, 1) * 100, 1)

tasa_seg_dep <- tapply(
  hotel_preparado$is_canceled == "Sí",
  list(hotel_preparado$market_segment, hotel_preparado$deposit_type),
  mean
) * 100

tasa_seg_dep[n_seg_dep < 100] <- NA
round(tasa_seg_dep, 1)

# ----------------------------------------------------------
# 6.4 PREGUNTA 6: LEAD TIME vs ADR
# ----------------------------------------------------------

sum(hotel_preparado$adr < 0)
sum(hotel_preparado$adr == 0)
sum(hotel_preparado$adr >= 1000)

# Para esta pregunta se excluyen adr <= 0 y el valor extremo de 5400
datos_adr <- hotel_preparado[
  hotel_preparado$adr > 0 & hotel_preparado$adr < 1000,
]

nrow(datos_adr)

# Correlación (Spearman por la asimetría vista en los valores atípicos)
cor(datos_adr$lead_time, datos_adr$adr, method = "pearson")
cor(datos_adr$lead_time, datos_adr$adr, method = "spearman")

# Gráfico de dispersión
grafico("08_dispersion_lead_time_adr.png", {
  par(mfrow = c(1, 1))
  
  plot(
    datos_adr$lead_time,
    datos_adr$adr,
    main = "Relación entre anticipación de la reserva y ADR",
    xlab = "Anticipación de la reserva (días)",
    ylab = "ADR",
    pch = 16,
    cex = 0.4,
    col = rgb(0, 0, 0, 0.06)
  )
  
  lines(
    lowess(datos_adr$lead_time, datos_adr$adr, f = 0.2),
    col = "red",
    lwd = 3
  )
})

# ADR según rango de anticipación
datos_adr$rango_lead <- cut(
  datos_adr$lead_time,
  breaks = c(-1, 30, 90, 180, 365, Inf),
  labels = c("0-30", "31-90", "91-180", "181-365", "Más de 365")
)

tabla_rango_adr <- data.frame(
  Rango_Lead_Time = levels(datos_adr$rango_lead),
  Reservas = as.vector(table(datos_adr$rango_lead)),
  Mediana_ADR = as.vector(tapply(datos_adr$adr, datos_adr$rango_lead, median))
)

tabla_rango_adr

grafico("09_boxplot_adr_por_rango_anticipacion.png", {
  par(mfrow = c(1, 1), mar = c(7, 4, 4, 2))
  
  boxplot(
    adr ~ rango_lead,
    data = datos_adr,
    main = "ADR según rango de anticipación",
    xlab = "",
    ylab = "ADR",
    names = c("0-30", "31-90", "91-180", "181-365", "+365"),
    las = 2,
    cex.axis = 0.8,
    outline = FALSE
  )
  
  mtext("Rango de anticipación (días)", side = 1, line = 5.5)
  
  par(mar = c(5, 4, 4, 2) + 0.1)
})

# ----------------------------------------------------------
# 6.5 MULTIVARIADO: lead_time vs adr según tipo de hotel
# ----------------------------------------------------------

adr_city   <- datos_adr[datos_adr$hotel == "City Hotel", ]
adr_resort <- datos_adr[datos_adr$hotel == "Resort Hotel", ]

cor(adr_city$lead_time, adr_city$adr, method = "spearman")
cor(adr_resort$lead_time, adr_resort$adr, method = "spearman")

round(
  tapply(
    datos_adr$adr,
    list(datos_adr$rango_lead, datos_adr$hotel),
    median
  ),
  1
)

grafico("10_boxplot_adr_rango_anticipacion_por_hotel.png", {
  par(mfrow = c(1, 2), mar = c(7, 4, 4, 2))
  
  boxplot(
    adr ~ rango_lead,
    data = adr_city,
    main = "City Hotel",
    xlab = "",
    las = 2,
    ylab = "ADR",
    names = c("0-30", "31-90", "91-180", "181-365", "+365"),
    cex.axis = 0.8,
    outline = FALSE,
    ylim = c(0, 300)
  )
  
  mtext("Rango de anticipación (días)", side = 1, line = 5.5)
  
  boxplot(
    adr ~ rango_lead,
    data = adr_resort,
    main = "Resort Hotel",
    xlab = "",
    las = 2,
    ylab = "ADR",
    names = c("0-30", "31-90", "91-180", "181-365", "+365"),
    cex.axis = 0.8,
    outline = FALSE,
    ylim = c(0, 300)
  )
  
  mtext("Rango de anticipación (días)", side = 1, line = 5.5)
  
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
}, ancho = 1400, alto = 700)


# ==========================================================
# 7. VISUALIZACIÓN TEMPORAL
#    ADR mediano por mes de llegada y tipo de hotel
# ==========================================================

# ADR mayor a 0 y menor a 1000
datos_mes <- hotel_preparado[
  hotel_preparado$adr > 0 & hotel_preparado$adr < 1000,
]

nrow(datos_mes)

# Mes de llegada (1 a 12)
datos_mes$mes_llegada <- as.numeric(
  format(datos_mes$arrival_date, "%m")
)

table(datos_mes$mes_llegada)

# Mediana de ADR por mes y hotel
adr_mes <- tapply(
  datos_mes$adr,
  list(datos_mes$mes_llegada, datos_mes$hotel),
  median
)

meses_es <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
              "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

rownames(adr_mes) <- meses_es

round(adr_mes, 1)

grafico("11_adr_mediano_por_mes.png", {
  par(mfrow = c(1, 1))
  
  matplot(
    adr_mes,
    type = "b",
    pch = 16,
    lty = 1,
    lwd = 2,
    col = c("black", "red"),
    xaxt = "n",
    main = "ADR mediano por mes de llegada",
    xlab = "Mes de llegada",
    ylab = "ADR mediano",
    ylim = c(0, max(adr_mes) * 1.1)
  )
  
  axis(1, at = 1:12, labels = meses_es)
  
  legend(
    "topleft",
    legend = colnames(adr_mes),
    col = c("black", "red"),
    lty = 1,
    lwd = 2,
    pch = 16,
    bty = "n"
  )
})


# ==========================================================
# FIN DEL SCRIPT
# Salidas: data/hotel_bookings_preparado.csv y PNG en output/gráficos/
# ==========================================================

install.packages(c("flexdashboard", "plotly", "tidyverse"))

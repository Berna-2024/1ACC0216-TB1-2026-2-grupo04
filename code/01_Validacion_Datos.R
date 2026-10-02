cat("\014")
#Inspeccion de dimensiones del dataset
dim(hotel_bookings)

sum(is.na(hotel_bookings))

faltante <- colSums(is.na(hotel_bookings))
faltante[faltante>0] 


unique(hotel_bookings$agent)
unique(hotel_bookings$company)
unique(hotel_bookings$country)

#Revisar los valores de cada columna
table(hotel_bookings$country, useNA = "ifany")
table(hotel_bookings$agent, useNA = "ifany")
table(hotel_bookings$company, useNA = "ifany")


#Los que tienen NULL en las filas de cada columna
sum(hotel_bookings$country == "NULL", na.rm = TRUE)
sum(hotel_bookings$agent == "NULL", na.rm = TRUE)
sum(hotel_bookings$company == "NULL", na.rm = TRUE)


#sapply recorre cada una de las columnas

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

#Crear una tabla de porcentaje de datos faltantes
tabla_faltantes <- data.frame(
  Variable = names(porcentaje_faltantes),
  Porcentaje = round(porcentaje_faltantes, 3)
)

tabla_faltantes[
  tabla_faltantes$Porcentaje > 0,
]


# ==========================================================
# 2. UNICIDAD
# ==========================================================

# Cantidad de filas completamente duplicadas
sum(duplicated(hotel_bookings))

head(hotel_bookings[duplicated(hotel_bookings), ])


# ==========================================================
# 3. Validez
# ==========================================================

#Comprobar si hay valores negativos
sum(hotel_bookings$adults < 0, na.rm = TRUE)
sum(hotel_bookings$children < 0, na.rm = TRUE)
sum(hotel_bookings$babies < 0, na.rm = TRUE)
sum(hotel_bookings$lead_time < 0, na.rm = TRUE)
sum(hotel_bookings$stays_in_weekend_nights < 0, na.rm = TRUE)
sum(hotel_bookings$stays_in_week_nights < 0, na.rm = TRUE)
sum(hotel_bookings$required_car_parking_spaces < 0, na.rm = TRUE)
sum(hotel_bookings$total_of_special_requests < 0, na.rm = TRUE)

#Valores extremos
summary(hotel_bookings$adr)

#Si los valores binarios están correctos
sum(
  !hotel_bookings$is_canceled %in% c(0, 1),
  na.rm = TRUE
)

#Si los meses están correctos
table(hotel_bookings$arrival_date_month)

#Dias dentro del rango
sum(
  hotel_bookings$arrival_date_day_of_month < 1 |
    hotel_bookings$arrival_date_day_of_month > 31,
  na.rm = TRUE
)

#Reservas con ninguna persona ocupandolas
total_huespedes <-
  hotel_bookings$adults +
  hotel_bookings$children +
  hotel_bookings$babies


sum(total_huespedes == 0, na.rm = TRUE)

#
total_noches <-
  hotel_bookings$stays_in_weekend_nights +
  hotel_bookings$stays_in_week_nights
sum(total_noches == 0, na.rm = TRUE)



# ==========================================================
# 4. Consistencia
# ==========================================================

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

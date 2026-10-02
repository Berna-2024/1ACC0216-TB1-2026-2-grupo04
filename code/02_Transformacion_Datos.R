
hotel_preparado <- hotel_bookings

head(hotel_preparado$hotel)

#Conversacion a Factor: 

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
# Convertir valores NULL a NA
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
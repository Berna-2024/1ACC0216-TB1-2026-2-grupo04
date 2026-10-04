# Analisís Bivariado
# PREGUNTA 5: MARKET SEGMENT vs CANCELACION

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


#Grafico
tasa_seg <- tabla_seg_cancel[tabla_seg_cancel$Reservas > 30, ]

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



# PREGUNTA 5: CUSTOMER TYPE vs CANCELACION

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


#Grafico
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



# multivariado: segmento x tipo de cliente y segmento x deposito

n_seg_cliente <- table(
  hotel_preparado$market_segment,
  hotel_preparado$customer_type
)

tasa_seg_cliente <- tapply(
  hotel_preparado$is_canceled == "Sí",
  list(hotel_preparado$market_segment, hotel_preparado$customer_type),
  mean
) * 100

tasa_seg_cliente[n_seg_cliente < 100] <- NA
round(tasa_seg_cliente, 1)


#tipo de deposito
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

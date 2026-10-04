#Visualizacion Temporal
# ADR MEDIANO POR MES DE LLEGADA Y TIPO DE HOTEL


#adr mayor a 0 y menor a 1000
datos_mes <- hotel_preparado[
  hotel_preparado$adr > 0 & hotel_preparado$adr < 1000,
]

nrow(datos_mes)


#mes de llegada (1 a 12)
datos_mes$mes_llegada <- as.numeric(
  format(datos_mes$arrival_date, "%m")
)

table(datos_mes$mes_llegada)


#mediana de ADR por mes y hotel
adr_mes <- tapply(
  datos_mes$adr,
  list(datos_mes$mes_llegada, datos_mes$hotel),
  median
)

meses_es <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
              "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

rownames(adr_mes) <- meses_es

round(adr_mes, 1)


#grafico
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

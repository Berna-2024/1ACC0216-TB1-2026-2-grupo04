#Analisis Bivariado
# PREGUNTA 6: LEAD TIME vs ADR

sum(hotel_preparado$adr < 0)
sum(hotel_preparado$adr == 0)
sum(hotel_preparado$adr >= 1000)

#para esta pregunta se excluyen adr <= 0 y el valor extremo de 5400
datos_adr <- hotel_preparado[
  hotel_preparado$adr > 0 & hotel_preparado$adr < 1000,
]

nrow(datos_adr)


#correlacion (Spearman por la asimetria vista en los valores atipicos)
cor(datos_adr$lead_time, datos_adr$adr, method = "pearson")
cor(datos_adr$lead_time, datos_adr$adr, method = "spearman")


#grafico de dispersion
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


#ADR segun rango de anticipacion
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





# multivariado: lead_time vs adr segun tipo de hotel

adr_city <- datos_adr[datos_adr$hotel == "City Hotel", ]
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
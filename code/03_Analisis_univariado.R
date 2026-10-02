#Analisis Univariado

frecuencia_segmento <- table(
  hotel_preparado$market_segment
)

frecuencia_segmento


porcentaje_segmento <- prop.table(
  frecuencia_segmento
) * 100

round(porcentaje_segmento, 2)


valores_miles <- frecuencia_segmento / 1000
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


# ----------------------------------------------------------
# CUSTOMER TYPE
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


#Grafico
valores_cliente_miles <- frecuencia_cliente / 1000

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

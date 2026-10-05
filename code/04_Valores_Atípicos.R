# ==========================================================
# 4. VALORES ATÍPICOS
# ==========================================================

# DE LEAD_TIME

summary(hotel_preparado$lead_time)


# -------------
# Calcular Q1 y Q3
# -------------

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


# ----------
# Calcular rango intercuartílico (IQR)
# ----------

IQR_lead <- Q3_lead - Q1_lead

IQR_lead


# -----------
# Calcular límites
# ------------

limite_inferior_lead <- Q1_lead - 1.5 * IQR_lead
limite_superior_lead <- Q3_lead + 1.5 * IQR_lead

limite_inferior_lead
limite_superior_lead


# -------------
# Identificar valores atípicos
# ---------
atipicos_lead <- hotel_preparado$lead_time[
  !is.na(hotel_preparado$lead_time) &
    (
      hotel_preparado$lead_time < limite_inferior_lead |
        hotel_preparado$lead_time > limite_superior_lead
    )
]


length(atipicos_lead)


# -----------
# Porcentaje de valores atípicos
# --------

porcentaje_atipicos_lead <- (
  length(atipicos_lead) /
    nrow(hotel_preparado)
) * 100

round(porcentaje_atipicos_lead, 2)


# -------
# Cantidad de reservas dentro de los límites
# ------------------------

cantidad_dentro_lead <- sum(
  hotel_preparado$lead_time >= limite_inferior_lead &
    hotel_preparado$lead_time <= limite_superior_lead,
  na.rm = TRUE
)

cantidad_dentro_lead


# --------
# Cantidad de reservas fuera de los límites
# -----------------------------

cantidad_fuera_lead <- sum(
  hotel_preparado$lead_time < limite_inferior_lead |
    hotel_preparado$lead_time > limite_superior_lead,
  na.rm = TRUE
)

cantidad_fuera_lead


# --------------
# Porcentaje dentro de los límites
# -------------

porcentaje_dentro_lead <- (
  cantidad_dentro_lead /
    nrow(hotel_preparado)
) * 100

round(porcentaje_dentro_lead, 2)


#Grafico

boxplot(
  hotel_preparado$lead_time,
  main = "Valores atípicos en la anticipación de reserva",
  ylab = "Días de anticipación"

  )






# =====================
# 4.2 VALORES ATÍPICOS DE ADR
# =====================


summary(hotel_preparado$adr)


# Calcular Q1 y Q3
# ------------------

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


# ---------------------
# Calcular rango intercuartílico
# ---------------------

IQR_adr <- Q3_adr - Q1_adr

IQR_adr


# ----------------------------------------------------------
# Calcular límites
# ----------------------------------------------------------

limite_inferior_adr <- Q1_adr - 1.5 * IQR_adr
limite_superior_adr <- Q3_adr + 1.5 * IQR_adr

limite_inferior_adr
limite_superior_adr


# -------------
# Identificar valores atípicos
# ------------------

atipicos_adr <- hotel_preparado$adr[
  !is.na(hotel_preparado$adr) &
    (
      hotel_preparado$adr < limite_inferior_adr |
        hotel_preparado$adr > limite_superior_adr
    )
]

length(atipicos_adr)


# ------------------
# Porcentaje de valores atípicos
# ---------------------

porcentaje_atipicos_adr <- (
  length(atipicos_adr) /
    nrow(hotel_preparado)
) * 100

round(porcentaje_atipicos_adr, 2)


# ---------------------
# Cantidad dentro de los límites
# --------------------------

cantidad_dentro_adr <- sum(
  hotel_preparado$adr >= limite_inferior_adr &
    hotel_preparado$adr <= limite_superior_adr,
  na.rm = TRUE
)

cantidad_dentro_adr


# ----------------------
# Cantidad fuera de los límites
# --------------------------

cantidad_fuera_adr <- sum(
  hotel_preparado$adr < limite_inferior_adr |
    hotel_preparado$adr > limite_superior_adr,
  na.rm = TRUE
)

cantidad_fuera_adr


# -------------------
# Porcentaje dentro de los límites
# --------------------------

porcentaje_dentro_adr <- (
  cantidad_dentro_adr /
    nrow(hotel_preparado)
) * 100

round(porcentaje_dentro_adr, 2)


# --------------------
# Revisar valores atípicos extremos
# ------------------------


head(
  sort(atipicos_adr),
  10
)


tail(
  sort(atipicos_adr),
  10
)

#Grafico

boxplot(
  hotel_preparado$adr,
  main = "Valores atípicos en la tarifa diaria promedio",
  ylab = "ADR",
  ylim = c(0, 500)
)


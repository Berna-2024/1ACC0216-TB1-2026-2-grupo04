# Hotel Booking Demand: Análisis exploratorio (TB1 – Grupo 03)

Curso: Fundamentos de Data Science (1ACC0216) – UPC – Ciclo 2026-2

## Objetivo del trabajo
Realizar un análisis exploratorio del dataset Hotel Booking Demand en R/RStudio:
evaluar su calidad, describir distribuciones y valores atípicos, explorar relaciones
entre variables y comunicar hallazgos con visualizaciones, conclusiones y
recomendaciones. Preguntas analíticas del grupo:
1. ¿Cómo se distribuyen las reservas y cancelaciones según el segmento de mercado
   y el tipo de cliente?
2. ¿Qué relación se observa entre la anticipación de la reserva (lead_time) y la
   tarifa diaria promedio (adr)?

## Integrantes
- Andy Cristian Condori Valeriano
- Bernabe Gonzales Galan
- Leonardo Fabian Huerta Ramirez
- Matias Alessandro Palacios Paulino

## Descripción del dataset
119,390 reservas y 32 variables de un City Hotel y un Resort Hotel (llegadas entre
julio de 2015 y agosto de 2017). Incluye cancelaciones, fechas, anticipación,
estancia, huéspedes, segmento de mercado, depósito y tarifa diaria promedio (ADR).
Fuente: Antonio, N., de Almeida, A., & Nunes, L. (2019). Hotel booking demand
datasets. *Data in Brief*, 22, 41-49. https://doi.org/10.1016/j.dib.2018.11.126

## Estructura del repositorio
- `data/`: dataset original (sin modificar) y dataset preparado (generado por el script).
- `code/`: script único `upc-grupo03-tb1-codigo.R` y dashboard `.Rmd`.
- `output/`: gráficos (PNG) y dashboard en HTML.

## Cómo ejecutar
1. Clonar el repositorio y abrir `code/upc-grupo03-tb1-codigo.R` en RStudio.
2. Ejecutar el script completo (genera `data/hotel_bookings_preparado.csv` y los gráficos).
3. Para el dashboard: instalar `flexdashboard`, `tidyverse` y `plotly`, y tejer el `.Rmd`.

## Conclusiones
- Las reservas se concentran en Online TA (47.30%) y en clientes Transient (75.06%),
  que también aportan el mayor número de cancelaciones (tasa global: 37.04%).
- Groups tiene la tasa de cancelación más alta (61.06%); las reservas Non Refund
  de ese segmento se cancelan en 99.36% de los casos. Es una asociación a revisar.
- No hay relación lineal relevante entre anticipación y ADR (Spearman = -0.012);
  el ADR varía más con la temporada y el tipo de hotel, sobre todo en Resort Hotel.

## Licencia
Código bajo licencia MIT (ver `LICENSE`). El dataset pertenece a sus autores originales.

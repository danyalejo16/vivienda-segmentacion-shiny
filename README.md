# Dashboard Vivienda – Beneficiarios (R Shiny)

Aplicación web en **R Shiny** para **explorar, filtrar y analizar** una base de datos de beneficiarios de vivienda. Permite segmentación por **sexo**, **provincia/cantón**, **segmento**, condición **CONADIS** y **rango de ingresos**. Incluye gráficos interactivos, estadísticas interpretadas, semáforo financiero (déficit y ratio gasto/ingreso), rankings y descarga de datos filtrados.

## Funcionalidades
- Filtros: sexo, provincia, cantón (dependiente), segmento, CONADIS + tipo, rango de ingresos.
- Visualizaciones (Plotly):
  - Distribución por sexo
  - Histograma de ingresos
  - Ingreso vs gasto (gama azul eléctrico)
  - Distribución por provincia (barras con colores distintos)
- Estadísticas con interpretación automática:
  - Media, mediana, percentiles, IQR, outliers
  - Correlación ingreso–gasto
  - Ratio gasto/ingreso
  - % de déficit (gasto > ingreso)
- Semáforo financiero (verde/amarillo/rojo) basado en % déficit y ratio.
- Rankings por provincia y por segmento (tabla con “chips” de color por provincia).
- Tabla interactiva (DT) + descarga del dataset filtrado a CSV.

## Requisitos
- R (recomendado: versión reciente)
- Paquetes:
  - `shiny`, `ggplot2`, `plotly`, `dplyr`, `DT`, `janitor`

Instalación rápida:
```r
install.packages(c("shiny","ggplot2","plotly","dplyr","DT","janitor"))

#Estructura sugerida del proyecto

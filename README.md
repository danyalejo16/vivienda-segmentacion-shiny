````md
# Dashboard Vivienda – Beneficiarios (R Shiny)

Aplicación web en **R Shiny** para **explorar, filtrar y analizar** una base de datos de beneficiarios de vivienda. Permite segmentación por **sexo**, **provincia/cantón**, **segmento**, condición **CONADIS** y **rango de ingresos**. Incluye gráficos interactivos, estadísticas interpretadas, semáforo financiero (déficit y ratio gasto/ingreso), rankings y descarga de datos filtrados.

---

## Funcionalidades

- **Filtros**: sexo, provincia, cantón (dependiente), segmento, CONADIS + tipo, rango de ingresos.
- **Visualizaciones (Plotly)**:
  - Distribución por sexo
  - Histograma de ingresos
  - Ingreso vs gasto (azul eléctrico)
  - Distribución por provincia (barras con colores distintos)
- **Estadísticas con interpretación automática**:
  - Media, mediana, percentiles, IQR, outliers
  - Correlación ingreso–gasto
  - Ratio gasto/ingreso
  - % de déficit (gasto > ingreso)
- **Semáforo financiero** (verde/amarillo/rojo) basado en % déficit y ratio.
- **Rankings** por provincia y por segmento (tabla con “chips” de color por provincia).
- **Tabla interactiva (DT)** + **descarga** del dataset filtrado a CSV.

---

## Requisitos

- **R** (recomendado: versión reciente)
- Paquetes: `shiny`, `ggplot2`, `plotly`, `dplyr`, `DT`, `janitor`

### Instalación de dependencias (una sola vez)

```r
install.packages(c("shiny","ggplot2","plotly","dplyr","DT","janitor"))
````

---

## Estructura sugerida del proyecto

> Recomendado: renombrar el CSV sin espacios y ubicarlo en `data/`.

```text
/
├─ app.R
├─ data/
│  └─ bdd_completa_beneficiarios2.csv
├─ README.md
└─ .gitignore
```

---

## Ejecución local

1. Clona el repositorio.
2. Verifica que el CSV esté en la ruta esperada: `data/bdd_completa_beneficiarios2.csv`.
3. Ejecuta la app:

```r
shiny::runApp()
```

---

## Publicación en shinyapps.io (resumen)

1. Instala y carga `rsconnect`:

```r
install.packages("rsconnect")
library(rsconnect)
```

2. Conecta tu cuenta en **shinyapps.io → Tokens** (copia y pega el comando `rsconnect::setAccountInfo(...)` en RStudio cuando te lo pida).

3. Publica desde RStudio con **Publish**, o por consola:

```r
rsconnect::deployApp()
```

---

## Datos

El archivo CSV contiene información de beneficiarios. Si los datos son sensibles, se recomienda:

* anonimizar campos personales antes de publicarlos, o
* no subir el CSV al repositorio (usar archivo local/privado).

---

## Licencia

Define tu licencia (por ejemplo **MIT**) o elimina esta sección si no aplica.

```

::contentReference[oaicite:0]{index=0}
```

# =========================
# app.R  (Shiny + DT + análisis + semáforo + rankings + colores pro + chips por provincia)
# =========================

# Evita el error: "trying to use CRAN without setting a mirror"
options(repos = c(CRAN = "https://cloud.r-project.org"))

# (Opcional) Instalar paquetes faltantes automáticamente
# Nota: en producción es mejor instalar paquetes una sola vez (fuera de la app)
pkgs <- c("shiny", "ggplot2", "plotly", "dplyr", "DT", "janitor")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(janitor)

# =========================
# Paleta profesional + Theme
# =========================
COL_BLUE    <- "#00A3FF"  # azul eléctrico (principal)
COL_PURPLE  <- "#6C5CE7"  # violeta moderno (secundario)
COL_ORANGE  <- "#FF6B00"  # naranja intenso (acento)
COL_PINK    <- "#FF2D55"  # magenta (acento)
COL_TEAL    <- "#00C2A8"  # turquesa (acento)
COL_OUTLINE <- "#0B0F1A"  # contorno oscuro
GRID_COL    <- "#E9EEF6"  # grid suave

PAL_CAT <- c(
  "#00A3FF", "#6C5CE7", "#FF6B00", "#00C2A8", "#FF2D55",
  "#2D9CDB", "#9B51E0", "#F2994A", "#27AE60", "#EB5757"
)

pal_for_levels <- function(levels_vec) {
  lv <- sort(unique(na.omit(levels_vec)))
  setNames(rep(PAL_CAT, length.out = length(lv)), lv)
}

theme_pro <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14, color = COL_OUTLINE),
      axis.title = element_text(face = "bold", color = COL_OUTLINE),
      axis.text  = element_text(color = COL_OUTLINE),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = GRID_COL, linewidth = 0.6),
      legend.title = element_blank(),
      legend.position = "bottom"
    )
}

# =========================
# Helpers
# =========================
fmt_money <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
fmt_num   <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = ",")
fmt_pct   <- function(x, d = 1) paste0(round(100 * x, d), "%")

desc_stats <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(list(n = 0))
  q <- quantile(x, probs = c(.1, .25, .5, .75, .9), na.rm = TRUE, names = FALSE)
  iqr <- q[4] - q[2]
  list(
    n      = length(x),
    mean   = mean(x),
    median = q[3],
    sd     = sd(x),
    min    = min(x),
    max    = max(x),
    p10    = q[1],
    q1     = q[2],
    q3     = q[4],
    p90    = q[5],
    iqr    = iqr,
    cv     = ifelse(mean(x) == 0, NA, sd(x) / mean(x))
  )
}

interpreta_cor <- function(r) {
  if (is.na(r)) return("No se puede estimar (pocos datos con ingreso y gasto).")
  a <- abs(r)
  nivel <- dplyr::case_when(
    a < 0.20 ~ "muy débil",
    a < 0.40 ~ "débil",
    a < 0.60 ~ "moderada",
    a < 0.80 ~ "fuerte",
    TRUE     ~ "muy fuerte"
  )
  signo <- ifelse(r >= 0, "positiva", "negativa")
  paste0("Relación ", signo, " ", nivel, " (r=", fmt_num(r, 3), ").")
}

interpreta_asimetria <- function(meanv, medv) {
  if (is.na(meanv) || is.na(medv)) return("No se puede interpretar la forma de la distribución.")
  if (meanv > medv) return("La media es mayor que la mediana: asimetría a la derecha (unos pocos valores altos empujan la media).")
  if (meanv < medv) return("La media es menor que la mediana: asimetría a la izquierda (valores bajos empujan la media).")
  "Media y mediana similares: distribución relativamente balanceada."
}

# Normalizar "si/sí" (Conadis)
is_yes <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x %in% c("si", "sí", "sì")
}

# =========================
# Semáforo: umbrales (AJUSTABLES)
# =========================
TH_PDEF_GREEN  <- 0.15   # Verde ≤ 15%
TH_PDEF_YELLOW <- 0.30   # Amarillo 15–30% | Rojo >30%
TH_RATIO_GREEN <- 0.90   # Verde ≤ 0.90
TH_RATIO_RED   <- 1.00   # Amarillo 0.90–1.00 | Rojo > 1.00

grade_pdef <- function(p) {
  if (is.na(p)) return("gris")
  if (p <= TH_PDEF_GREEN) return("verde")
  if (p <= TH_PDEF_YELLOW) return("amarillo")
  "rojo"
}
grade_ratio <- function(r) {
  if (is.na(r)) return("gris")
  if (r <= TH_RATIO_GREEN) return("verde")
  if (r <= TH_RATIO_RED)   return("amarillo")
  "rojo"
}
worst_grade <- function(g1, g2) {
  ord <- c("verde" = 1, "amarillo" = 2, "rojo" = 3, "gris" = 0)
  if (ord[g1] >= ord[g2]) g1 else g2
}
badge_html <- function(label, grade) {
  colors <- list(
    "verde"    = "#1f7a1f",
    "amarillo" = "#b58900",
    "rojo"     = "#b22222",
    "gris"     = "#6c757d"
  )
  tags$span(
    style = paste0(
      "display:inline-block;padding:6px 10px;border-radius:14px;",
      "color:#fff;font-weight:600;background:", colors[[grade]], ";"
    ),
    label
  )
}

# =========================
# Cargar y preparar datos
# =========================
data <- read.csv(
  "bdd completa Beneficiaros2.csv",
  sep = ";",
  dec = ".",
  quote = "\"",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "latin1"
)

data <- janitor::clean_names(data)

data <- data %>%
  mutate(
    ingreso_mensual = suppressWarnings(as.numeric(ingreso_mensual)),
    gasto_mensual   = suppressWarnings(as.numeric(gasto_mensual))
  ) %>%
  filter(!is.na(ingreso_mensual))  # necesario para slider

# Colores fijos por provincia (y segmento opcional) para que coincidan gráficos y tabla
PROV_COLORS <- pal_for_levels(data$provincia_domicilio)
SEG_COLORS  <- pal_for_levels(data$segmento)

# =========================
# UI
# =========================
ui <- fluidPage(
  titlePanel("Acceso a financiamiento de vivienda por nivel de ingresos"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("sexo", "Sexo:",
                  choices = c("Todos", sort(unique(na.omit(data$sexo)))),
                  selected = "Todos"),
      selectInput("provincia", "Provincia:",
                  choices = c("Todas", sort(unique(na.omit(data$provincia_domicilio)))),
                  selected = "Todas"),
      uiOutput("canton_ui"),
      selectInput("segmento", "Segmento:",
                  choices = c("Todos", sort(unique(na.omit(data$segmento)))),
                  selected = "Todos"),
      checkboxInput("solo_discapacitados", "Solo personas con discapacidad", value = FALSE),
      uiOutput("tipo_discapacidad_ui"),
      
      sliderInput("rango_ingresos", "Rango de ingresos ($)",
                  min = 0,
                  max = max(data$ingreso_mensual, na.rm = TRUE),
                  value = c(0, max(data$ingreso_mensual, na.rm = TRUE))),
      
      downloadButton("descargar", "Descargar tabla filtrada")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen",
                 plotlyOutput("grafico_sexo"),
                 plotlyOutput("grafico_ingresos")),
        tabPanel("Ingreso vs Gasto",
                 plotlyOutput("grafico_ing_vs_gasto")),
        tabPanel("Por Provincia",
                 plotlyOutput("grafico_provincia")),
        tabPanel("Estadísticas",
                 uiOutput("semaforo_ui"),
                 br(),
                 uiOutput("resumen_estadistico"),
                 br(),
                 tags$h4("Métricas interpretadas"),
                 DTOutput("tabla_estadisticas"),
                 br(),
                 tags$h4("Top provincias con mayor % déficit (gasto > ingreso)"),
                 plotlyOutput("plot_deficit_prov"),
                 DTOutput("tabla_deficit_prov"),
                 br(),
                 tags$h4("Ranking por segmento"),
                 DTOutput("tabla_deficit_seg")
        ),
        tabPanel("Tabla",
                 DTOutput("tabla"))
      )
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output, session) {
  
  datos_filtrados <- reactive({
    df <- data
    
    if (input$sexo != "Todos") df <- df %>% filter(sexo == input$sexo)
    
    if (input$provincia != "Todas") df <- df %>% filter(provincia_domicilio == input$provincia)
    
    if (!is.null(input$canton) && input$canton != "Todos") {
      df <- df %>% filter(canton_domicilio == input$canton)
    }
    
    if (input$segmento != "Todos") df <- df %>% filter(segmento == input$segmento)
    
    if (isTRUE(input$solo_discapacitados)) {
      df <- df %>% filter(is_yes(conadis))
      if (!is.null(input$tipo_discapacidad) && input$tipo_discapacidad != "Todos") {
        df <- df %>% filter(conadis_tipo_discapacidad == input$tipo_discapacidad)
      }
    }
    
    df <- df %>% filter(
      ingreso_mensual >= input$rango_ingresos[1],
      ingreso_mensual <= input$rango_ingresos[2]
    )
    
    if (nrow(df) == 0) return(NULL)
    df
  })
  
  output$canton_ui <- renderUI({
    req(input$provincia)
    
    cantones <- if (input$provincia == "Todas") {
      sort(unique(na.omit(data$canton_domicilio)))
    } else {
      data %>%
        filter(provincia_domicilio == input$provincia) %>%
        pull(canton_domicilio) %>%
        unique() %>%
        na.omit() %>%
        sort()
    }
    
    selectInput("canton", "Cantón:", choices = c("Todos", cantones), selected = "Todos")
  })
  
  output$tipo_discapacidad_ui <- renderUI({
    if (!isTRUE(input$solo_discapacitados)) return(NULL)
    
    tipos <- data %>%
      filter(is_yes(conadis)) %>%
      pull(conadis_tipo_discapacidad) %>%
      unique() %>%
      na.omit() %>%
      sort()
    
    selectInput("tipo_discapacidad", "Tipo de Discapacidad:",
                choices = c("Todos", tipos), selected = "Todos")
  })
  
  # ======================
  # Gráficos (colores pro)
  # ======================
  
  output$grafico_sexo <- renderPlotly({
    datos <- datos_filtrados()
    if (is.null(datos)) return(NULL)
    
    pal_sexo <- pal_for_levels(datos$sexo)
    
    p <- datos %>% count(sexo) %>%
      ggplot(aes(x = sexo, y = n, fill = sexo,
                 text = paste("Sexo:", sexo, "<br>Cantidad:", n))) +
      geom_col(color = COL_OUTLINE, alpha = 0.95) +
      scale_fill_manual(values = pal_sexo) +
      theme_pro() +
      labs(title = "Distribución por sexo", x = "Sexo", y = "Cantidad")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$grafico_ingresos <- renderPlotly({
    datos <- datos_filtrados()
    if (is.null(datos)) return(NULL)
    
    p <- ggplot(datos, aes(x = ingreso_mensual)) +
      geom_histogram(fill = COL_ORANGE, color = COL_OUTLINE, bins = 50, alpha = 0.95) +
      geom_vline(aes(xintercept = median(ingreso_mensual, na.rm = TRUE)),
                 color = COL_PINK, linetype = "dashed", linewidth = 1) +
      theme_pro() +
      labs(title = "Distribución de Ingresos", x = "Ingreso mensual", y = "Frecuencia")
    
    ggplotly(p)
  })
  
  output$grafico_ing_vs_gasto <- renderPlotly({
    datos <- datos_filtrados()
    if (is.null(datos)) return(NULL)
    
    datos2 <- datos %>% filter(!is.na(gasto_mensual))
    if (nrow(datos2) == 0) return(NULL)
    
    p <- ggplot(datos2, aes(
      x = ingreso_mensual, y = gasto_mensual,
      text = paste("Ingreso:", ingreso_mensual, "<br>Gasto:", gasto_mensual)
    )) +
      geom_point(alpha = 0.55, color = COL_BLUE, size = 2) +
      theme_pro() +
      labs(title = "Ingreso vs Gasto mensual", x = "Ingreso", y = "Gasto")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Cada provincia con color distinto
  output$grafico_provincia <- renderPlotly({
    datos <- datos_filtrados()
    if (is.null(datos)) return(NULL)
    
    p <- datos %>%
      count(provincia_domicilio) %>%
      arrange(desc(n)) %>%
      ggplot(aes(
        x = reorder(provincia_domicilio, -n),
        y = n,
        fill = provincia_domicilio,
        text = paste("Provincia:", provincia_domicilio, "<br>Cantidad:", n)
      )) +
      geom_col(color = COL_OUTLINE, alpha = 0.95, show.legend = FALSE) +
      scale_fill_manual(values = PROV_COLORS) +
      theme_pro() +
      labs(title = "Distribución por provincia", x = "Provincia", y = "Cantidad") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  # ======================
  # Métricas interpretadas
  # ======================
  resumen_metricas <- reactive({
    datos <- datos_filtrados()
    if (is.null(datos)) return(NULL)
    
    inc <- datos$ingreso_mensual
    gas <- datos$gasto_mensual
    
    s_inc <- desc_stats(inc)
    s_gas <- desc_stats(gas)
    
    both <- datos %>% filter(!is.na(ingreso_mensual), !is.na(gasto_mensual))
    both_ratio <- both %>% filter(ingreso_mensual > 0)
    
    prop_deficit <- if (nrow(both) == 0) NA else mean(both$gasto_mensual > both$ingreso_mensual)
    med_deficit  <- if (nrow(both) == 0) NA else median(both$gasto_mensual - both$ingreso_mensual, na.rm = TRUE)
    med_ratio    <- if (nrow(both_ratio) == 0) NA else median(both_ratio$gasto_mensual / both_ratio$ingreso_mensual, na.rm = TRUE)
    cor_rg       <- if (nrow(both) < 3) NA else suppressWarnings(cor(both$ingreso_mensual, both$gasto_mensual, use = "complete.obs"))
    
    out_inc <- 0
    if (is.finite(s_inc$iqr)) {
      li <- s_inc$q1 - 1.5 * s_inc$iqr
      ls <- s_inc$q3 + 1.5 * s_inc$iqr
      out_inc <- sum(inc < li | inc > ls, na.rm = TRUE)
    }
    
    out_gas <- 0
    if (is.finite(s_gas$iqr)) {
      li <- s_gas$q1 - 1.5 * s_gas$iqr
      ls <- s_gas$q3 + 1.5 * s_gas$iqr
      out_gas <- sum(gas < li | gas > ls, na.rm = TRUE)
    }
    
    top_prov <- datos %>% count(provincia_domicilio, sort = TRUE) %>% slice_head(n = 5)
    
    data.frame(
      Metrica = c(
        "Registros (filtrados)",
        "Ingreso: media", "Ingreso: mediana", "Ingreso: Q1–Q3 (IQR)", "Ingreso: P10–P90", "Ingreso: outliers (IQR)",
        "Gasto: media", "Gasto: mediana", "Gasto: Q1–Q3 (IQR)", "Gasto: P10–P90", "Gasto: outliers (IQR)",
        "Correlación Ingreso–Gasto (r)",
        "% con gasto > ingreso (déficit)",
        "Déficit mediano (gasto - ingreso)",
        "Ratio mediano gasto/ingreso",
        "Top 5 provincias (conteo)"
      ),
      Valor = c(
        nrow(datos),
        fmt_money(s_inc$mean), fmt_money(s_inc$median),
        paste0(fmt_money(s_inc$q1), " – ", fmt_money(s_inc$q3), " (IQR=", fmt_money(s_inc$iqr), ")"),
        paste0(fmt_money(s_inc$p10), " – ", fmt_money(s_inc$p90)),
        out_inc,
        fmt_money(s_gas$mean), fmt_money(s_gas$median),
        paste0(fmt_money(s_gas$q1), " – ", fmt_money(s_gas$q3), " (IQR=", fmt_money(s_gas$iqr), ")"),
        paste0(fmt_money(s_gas$p10), " – ", fmt_money(s_gas$p90)),
        out_gas,
        ifelse(is.na(cor_rg), "NA", fmt_num(cor_rg, 3)),
        ifelse(is.na(prop_deficit), "NA", fmt_pct(prop_deficit, 1)),
        ifelse(is.na(med_deficit), "NA", fmt_money(med_deficit)),
        ifelse(is.na(med_ratio), "NA", fmt_num(med_ratio, 3)),
        paste0(paste(top_prov$provincia_domicilio, top_prov$n, sep = ": ", collapse = " | "))
      ),
      stringsAsFactors = FALSE
    )
  })
  
  output$tabla_estadisticas <- renderDT({
    df <- resumen_metricas()
    if (is.null(df)) return(NULL)
    datatable(df, rownames = FALSE,
              options = list(pageLength = 15, dom = "tip", scrollX = TRUE))
  })
  
  # ======================
  # Semáforo + interpretación
  # ======================
  output$semaforo_ui <- renderUI({
    datos <- datos_filtrados()
    if (is.null(datos)) return(NULL)
    
    both <- datos %>% filter(!is.na(ingreso_mensual), !is.na(gasto_mensual))
    both_ratio <- both %>% filter(ingreso_mensual > 0)
    
    if (nrow(both) == 0) {
      return(tags$div(
        badge_html("Semáforo: NO ESTIMABLE", "gris"),
        tags$p(style = "margin-top:8px;",
               "No hay suficientes registros con ingreso y gasto para calcular déficit y ratio.")
      ))
    }
    
    p_def <- mean(both$gasto_mensual > both$ingreso_mensual)
    r_med <- if (nrow(both_ratio) == 0) NA else median(both_ratio$gasto_mensual / both_ratio$ingreso_mensual, na.rm = TRUE)
    
    g1 <- grade_pdef(p_def)
    g2 <- grade_ratio(r_med)
    g_final <- worst_grade(g1, g2)
    
    tags$div(
      tags$h4("Semáforo financiero del corte filtrado"),
      badge_html(paste0("Semáforo: ", toupper(g_final)), g_final),
      tags$div(style = "margin-top:10px;",
               badge_html(paste0("% Déficit: ", fmt_pct(p_def, 1)), g1),
               tags$span("  "),
               badge_html(paste0("Ratio mediano gasto/ingreso: ", ifelse(is.na(r_med), "NA", fmt_num(r_med, 3))), g2)
      ),
      tags$p(style = "margin-top:10px;color:#555;",
             "Reglas: %déficit (verde≤15%, amarillo 15–30%, rojo>30%) y ratio (verde≤0.90, amarillo 0.90–1.00, rojo>1.00). ",
             "El semáforo final toma el peor de ambos.")
    )
  })
  
  output$resumen_estadistico <- renderUI({
    datos <- datos_filtrados()
    if (is.null(datos)) return(tags$p("No hay datos filtrados para mostrar."))
    
    inc <- datos$ingreso_mensual
    gas <- datos$gasto_mensual
    
    s_inc <- desc_stats(inc)
    s_gas <- desc_stats(gas)
    
    both <- datos %>% filter(!is.na(ingreso_mensual), !is.na(gasto_mensual))
    both_ratio <- both %>% filter(ingreso_mensual > 0)
    
    cor_rg <- if (nrow(both) < 3) NA else suppressWarnings(cor(both$ingreso_mensual, both$gasto_mensual, use = "complete.obs"))
    prop_deficit <- if (nrow(both) == 0) NA else mean(both$gasto_mensual > both$ingreso_mensual)
    med_ratio    <- if (nrow(both_ratio) == 0) NA else median(both_ratio$gasto_mensual / both_ratio$ingreso_mensual, na.rm = TRUE)
    
    ratio_txt <- if (is.na(med_ratio)) {
      "No se puede calcular el ratio gasto/ingreso (faltan datos o ingreso=0)."
    } else if (med_ratio < 0.70) {
      paste0("El gasto típico es bajo respecto al ingreso (ratio mediano ", fmt_num(med_ratio, 3), ").")
    } else if (med_ratio <= 1.00) {
      paste0("El gasto típico está cercano al ingreso (ratio mediano ", fmt_num(med_ratio, 3), ").")
    } else {
      paste0("El gasto típico supera al ingreso (ratio mediano ", fmt_num(med_ratio, 3), "). Posible presión financiera en el grupo filtrado.")
    }
    
    tags$div(
      tags$h4("Lectura rápida del corte filtrado"),
      tags$ul(
        tags$li(tags$b("Tamaño de muestra: "), nrow(datos), " registros."),
        tags$li(tags$b("Ingresos: "), interpreta_asimetria(s_inc$mean, s_inc$median)),
        tags$li(tags$b("Gastos: "), interpreta_asimetria(s_gas$mean, s_gas$median)),
        tags$li(tags$b("Ingreso vs Gasto: "), interpreta_cor(cor_rg)),
        tags$li(tags$b("Déficit (gasto>ingreso): "),
                ifelse(is.na(prop_deficit),
                       "No estimable con el filtro actual.",
                       paste0(fmt_pct(prop_deficit, 1), " de los casos con ingreso y gasto."))),
        tags$li(tags$b("Balance gasto/ingreso: "), ratio_txt)
      ),
      tags$h4("Notas"),
      tags$ul(
        tags$li("La ", tags$b("mediana"), " es un “caso típico” y es menos sensible a valores extremos."),
        tags$li(tags$b("IQR (Q1–Q3)"), " muestra dispersión de la mitad central."),
        tags$li("La correlación no implica causalidad; solo asociación.")
      )
    )
  })
  
  # ======================
  # Rankings: déficit por provincia y segmento
  # ======================
  deficit_por_grupo <- function(df, group_var) {
    both <- df %>% filter(!is.na(ingreso_mensual), !is.na(gasto_mensual))
    if (nrow(both) == 0) return(NULL)
    
    both_ratio <- both %>% filter(ingreso_mensual > 0)
    
    tab_def <- both %>%
      mutate(
        deficit_flag = gasto_mensual > ingreso_mensual,
        deficit_val  = gasto_mensual - ingreso_mensual
      ) %>%
      group_by(.data[[group_var]]) %>%
      summarise(
        n_con_ing_gas   = n(),
        n_deficit       = sum(deficit_flag, na.rm = TRUE),
        pct_deficit     = n_deficit / n_con_ing_gas,
        deficit_mediano = median(deficit_val, na.rm = TRUE),
        .groups = "drop"
      )
    
    tab_ratio <- both_ratio %>%
      mutate(ratio = gasto_mensual / ingreso_mensual) %>%
      group_by(.data[[group_var]]) %>%
      summarise(
        ratio_mediano = median(ratio, na.rm = TRUE),
        .groups = "drop"
      )
    
    tab_def %>%
      left_join(tab_ratio, by = setNames(group_var, group_var)) %>%
      arrange(desc(pct_deficit), desc(n_con_ing_gas))
  }
  
  # Tabla de provincias con CHIP de color (mismo color que en el gráfico)
  output$tabla_deficit_prov <- renderDT({
    datos <- datos_filtrados()
    if (is.null(datos)) return(NULL)
    
    tab <- deficit_por_grupo(datos, "provincia_domicilio")
    if (is.null(tab)) return(NULL)
    
    tab2 <- tab %>%
      mutate(
        prov_color = PROV_COLORS[provincia_domicilio],
        Provincia = paste0(
          '<span style="display:inline-block;width:12px;height:12px;border-radius:3px;',
          'margin-right:8px;background:', ifelse(is.na(prov_color), COL_PURPLE, prov_color), ';"></span>',
          provincia_domicilio
        ),
        `N (ingreso y gasto)` = n_con_ing_gas,
        `N déficit` = n_deficit,
        `% déficit` = fmt_pct(pct_deficit, 1),
        `Déficit mediano` = fmt_money(deficit_mediano),
        `Ratio mediano (gasto/ingreso)` = ifelse(is.na(ratio_mediano), "NA", fmt_num(ratio_mediano, 3))
      ) %>%
      select(
        Provincia,
        `N (ingreso y gasto)`,
        `N déficit`,
        `% déficit`,
        `Déficit mediano`,
        `Ratio mediano (gasto/ingreso)`
      )
    
    datatable(
      tab2,
      rownames = FALSE,
      escape = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$tabla_deficit_seg <- renderDT({
    datos <- datos_filtrados()
    if (is.null(datos)) return(NULL)
    
    tab <- deficit_por_grupo(datos, "segmento")
    if (is.null(tab)) return(NULL)
    
    tab2 <- tab %>%
      mutate(
        pct_deficit = fmt_pct(pct_deficit, 1),
        deficit_mediano = fmt_money(deficit_mediano),
        ratio_mediano = ifelse(is.na(ratio_mediano), "NA", fmt_num(ratio_mediano, 3))
      ) %>%
      rename(
        Segmento = segmento,
        `N (ingreso y gasto)` = n_con_ing_gas,
        `N déficit` = n_deficit,
        `% déficit` = pct_deficit,
        `Déficit mediano` = deficit_mediano,
        `Ratio mediano (gasto/ingreso)` = ratio_mediano
      )
    
    datatable(tab2, rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Top provincias por % déficit (cada barra color distinto, consistente con PROV_COLORS)
  output$plot_deficit_prov <- renderPlotly({
    datos <- datos_filtrados()
    if (is.null(datos)) return(NULL)
    
    tab <- deficit_por_grupo(datos, "provincia_domicilio")
    if (is.null(tab)) return(NULL)
    
    top <- tab %>% slice_head(n = 10)
    
    p <- ggplot(top, aes(
      x = reorder(provincia_domicilio, pct_deficit),
      y = pct_deficit,
      fill = provincia_domicilio,
      text = paste0(
        "Provincia: ", provincia_domicilio,
        "<br>% déficit: ", fmt_pct(pct_deficit, 1),
        "<br>N (ingreso y gasto): ", n_con_ing_gas,
        "<br>Ratio mediano: ", ifelse(is.na(ratio_mediano), "NA", fmt_num(ratio_mediano, 3)),
        "<br>Déficit mediano: $", fmt_money(deficit_mediano)
      )
    )) +
      geom_col(color = COL_OUTLINE, alpha = 0.95, show.legend = FALSE) +
      scale_fill_manual(values = PROV_COLORS) +
      coord_flip() +
      theme_pro() +
      labs(title = "Top 10 provincias por % déficit", x = "Provincia", y = "% déficit")
    
    ggplotly(p, tooltip = "text")
  })
  
  # ======================
  # Tabla principal + descarga
  # ======================
  output$tabla <- renderDT({
    datos <- datos_filtrados()
    if (is.null(datos)) return(NULL)
    datatable(datos, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$descargar <- downloadHandler(
    filename = function() paste0("datos_filtrados_", Sys.Date(), ".csv"),
    content = function(file) {
      datos <- datos_filtrados()
      if (!is.null(datos)) write.csv(datos, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)

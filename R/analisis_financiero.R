# FUNCIONES DE ANALISIS FINANCIERO

library(dplyr)
library(scales)

# --------------------------------------------------
# ANALISIS MOROSIDAD
# --------------------------------------------------

analisis_morosidad <- function(df) {
  
  ultimo <- dplyr::last(df$valor)
  maximo <- max(df$valor, na.rm = TRUE)
  mes_max <- as.character(df$mes[which.max(df$valor)])
  
  estado <- dplyr::case_when(
    ultimo < 0.05 ~ "nivel saludable",
    ultimo < 0.08 ~ "nivel moderado de riesgo",
    ultimo < 0.10 ~ "nivel de vigilancia",
    TRUE ~ "nivel crítico"
  )
  
  texto <- paste0(
    "La morosidad ampliada registra un valor actual de ",
    percent(ultimo, accuracy = 0.1),
    ", lo cual corresponde a un ",
    estado,
    ". El valor máximo observado durante el periodo fue ",
    percent(maximo, accuracy = 0.1),
    " en el mes de ",
    mes_max,
    "."
  )
  
  return(texto)
}

# --------------------------------------------------
# ANALISIS PRODUCTIVIDAD
# --------------------------------------------------

analisis_productividad <- function(df) {
  
  ultimo <- dplyr::last(df$valor)
  
  evaluacion <- dplyr::case_when(
    ultimo > 1.10 ~ "una estructura altamente eficiente",
    ultimo > 1.00 ~ "una estructura financiera adecuada",
    TRUE ~ "una estructura financiera débil"
  )
  
  texto <- paste0(
    "La relación de productividad se ubica en ",
    percent(ultimo, accuracy = 0.1),
    ", indicando ",
    evaluacion,
    " entre activos productivos y pasivos con costo."
  )
  
  return(texto)
}

# --------------------------------------------------
# ANALISIS EFICIENCIA OPERATIVA
# --------------------------------------------------

analisis_eficiencia <- function(df) {
  
  ultimo <- dplyr::last(df$eficiencia)
  
  evaluacion <- dplyr::case_when(
    ultimo < 0.60 ~ "alta eficiencia operativa",
    ultimo < 0.80 ~ "eficiencia operativa aceptable",
    TRUE ~ "presión significativa de gastos operativos"
  )
  
  texto <- paste0(
    "El indicador de eficiencia operativa se sitúa en ",
    percent(ultimo, accuracy = 0.1),
    ", lo que refleja ",
    evaluacion,
    " respecto al margen financiero generado."
  )
  
  return(texto)
}

# --------------------------------------------------
# ANALISIS ABSORCION DEL MARGEN
# --------------------------------------------------

analisis_absorcion <- function(df) {
  
  ultimo <- dplyr::last(df$valor)
  
  evaluacion <- dplyr::case_when(
    ultimo < 0.85 ~ "un nivel saludable de absorción del margen",
    ultimo < 0.95 ~ "un nivel elevado de absorción del margen",
    TRUE ~ "una presión crítica sobre el margen financiero"
  )
  
  texto <- paste0(
    "El grado de absorción del margen financiero se ubicó en ",
    percent(ultimo, accuracy = 0.1),
    ", reflejando ",
    evaluacion,
    "."
  )
  
  return(texto)
}

# --------------------------------------------------
# ANALISIS RATIO CARTERA IMPRODUCTIVA
# --------------------------------------------------

analisis_ratio_cartera <- function(df) {
  
  ultimo <- dplyr::last(df$ratio_improductiva)
  
  evaluacion <- dplyr::case_when(
    ultimo < 0.05 ~ "una calidad de cartera saludable",
    ultimo < 0.08 ~ "una cartera en observación",
    TRUE ~ "un deterioro relevante de la cartera"
  )
  
  texto <- paste0(
    "El ratio de cartera improductiva cerró en ",
    percent(ultimo, accuracy = 0.1),
    ", lo que evidencia ",
    evaluacion,
    "."
  )
  
  return(texto)
}

# --------------------------------------------------
# ANALISIS CRECIMIENTO CARTERA
# --------------------------------------------------

analisis_crecimiento_cartera <- function(df) {
  
  cartera <- df %>%
    filter(indicador == "CARTERA.BRUTA") %>%
    arrange(mes)
  
  inicial <- dplyr::first(cartera$valor)
  final <- dplyr::last(cartera$valor)
  crecimiento <- (final / inicial) - 1
  
  evaluacion <- dplyr::case_when(
    crecimiento > 0.20 ~ "un crecimiento robusto del negocio crediticio",
    crecimiento > 0.10 ~ "un crecimiento moderado del negocio crediticio",
    TRUE ~ "un crecimiento limitado del negocio crediticio"
  )
  
  texto <- paste0(
    "La cartera bruta pasó de ",
    dollar(inicial, accuracy = 0.01),
    " a ",
    dollar(final, accuracy = 0.01),
    ", equivalente a ",
    percent(crecimiento, accuracy = 0.1),
    ", mostrando ",
    evaluacion,
    "."
  )
  
  return(texto)
}

# --------------------------------------------------
# FUNCION SEMAFORO
# --------------------------------------------------

semaforo_indicador <- function(valor, tipo) {
  
  resultado <- dplyr::case_when(
    tipo == "morosidad" & valor < 0.05 ~ "RIESGO BAJO",
    tipo == "morosidad" & valor < 0.08 ~ "RIESGO MEDIO",
    tipo == "morosidad" ~ "RIESGO ALTO",
    
    tipo == "eficiencia" & valor < 0.70 ~ "NIVEL FAVORABLE",
    tipo == "eficiencia" & valor < 0.85 ~ "NIVEL INTERMEDIO",
    tipo == "eficiencia" ~ "NIVEL CRITICO",
    
    tipo == "absorcion" & valor < 0.85 ~ "NIVEL FAVORABLE",
    tipo == "absorcion" & valor < 0.95 ~ "NIVEL INTERMEDIO",
    tipo == "absorcion" ~ "NIVEL CRITICO",
    
    tipo == "ratio_cartera" & valor < 0.05 ~ "RIESGO BAJO",
    tipo == "ratio_cartera" & valor < 0.08 ~ "RIESGO MEDIO",
    tipo == "ratio_cartera" ~ "RIESGO ALTO",
    
    tipo == "productividad" & valor > 1.10 ~ "DESEMPEÑO ALTO",
    tipo == "productividad" & valor > 1.00 ~ "DESEMPEÑO ACEPTABLE",
    tipo == "productividad" ~ "DESEMPEÑO BAJO",
    
    TRUE ~ "SIN CLASIFICACION"
  )
  
  return(resultado)
}

# --------------------------------------------------
# RESUMEN EJECUTIVO AUTOMATICO
# --------------------------------------------------

resumen_ejecutivo_auto <- function(
    morosidad_data,
    productividad_data,
    eficiencia_data,
    absorcion_data,
    ratio_cartera,
    growth_data
) {
  
  morosidad_actual <- dplyr::last(morosidad_data$valor)
  productividad_actual <- dplyr::last(productividad_data$valor)
  eficiencia_actual <- dplyr::last(eficiencia_data$eficiencia)
  absorcion_actual <- dplyr::last(absorcion_data$valor)
  ratio_actual <- dplyr::last(ratio_cartera$ratio_improductiva)
  
  texto <- paste(
    paste0(
      "- ", semaforo_indicador(morosidad_actual, "morosidad"),
      ": La morosidad ampliada cerró en ",
      percent(morosidad_actual, accuracy = 0.1),
      "."
    ),
    paste0(
      "- ", semaforo_indicador(productividad_actual, "productividad"),
      ": La relación de productividad se ubicó en ",
      percent(productividad_actual, accuracy = 0.1),
      "."
    ),
    paste0(
      "- ", semaforo_indicador(eficiencia_actual, "eficiencia"),
      ": La eficiencia operativa alcanzó ",
      percent(eficiencia_actual, accuracy = 0.1),
      "."
    ),
    paste0(
      "- ", semaforo_indicador(absorcion_actual, "absorcion"),
      ": El grado de absorción del margen fue de ",
      percent(absorcion_actual, accuracy = 0.1),
      "."
    ),
    paste0(
      "- ", semaforo_indicador(ratio_actual, "ratio_cartera"),
      ": El ratio de cartera improductiva se situó en ",
      percent(ratio_actual, accuracy = 0.1),
      "."
    ),
    paste0(
      "- Crecimiento: ",
      analisis_crecimiento_cartera(growth_data)
    ),
    sep = "\n"
  )
}

# --------------------------------------------------
# CONCLUSIONES AUTOMATICAS
# --------------------------------------------------

conclusion_general_auto <- function(
    morosidad_data,
    productividad_data,
    eficiencia_data,
    absorcion_data,
    ratio_cartera,
    growth_data
) {
  
  morosidad_actual <- dplyr::last(morosidad_data$valor)
  productividad_actual <- dplyr::last(productividad_data$valor)
  eficiencia_actual <- dplyr::last(eficiencia_data$eficiencia)
  absorcion_actual <- dplyr::last(absorcion_data$valor)
  ratio_actual <- dplyr::last(ratio_cartera$ratio_improductiva)
  
  cartera <- growth_data %>%
    dplyr::filter(indicador == "CARTERA.BRUTA") %>%
    dplyr::arrange(mes)
  
  crecimiento_cartera <- (dplyr::last(cartera$valor) / dplyr::first(cartera$valor)) - 1
  
  texto <- dplyr::case_when(
    morosidad_actual >= 0.08 & eficiencia_actual >= 0.85 ~
      "La cooperativa presenta un escenario de crecimiento con presión simultánea en calidad de cartera y eficiencia operativa, lo que compromete la sostenibilidad financiera de mediano plazo.",
    
    morosidad_actual >= 0.08 ~
      "La cooperativa presenta crecimiento institucional, aunque con un deterioro relevante en la calidad de cartera que requiere acciones inmediatas de control y recuperación.",
    
    eficiencia_actual >= 0.85 | absorcion_actual >= 0.95 ~
      "La cooperativa mantiene actividad financiera dinámica, pero con una estructura operativa que absorbe gran parte del margen generado, limitando su capacidad de rentabilidad.",
    
    crecimiento_cartera > 0.20 & productividad_actual > 1.10 ~
      "La cooperativa presenta crecimiento sostenido y una adecuada productividad financiera; sin embargo, debe mantenerse vigilancia continua sobre el riesgo crediticio y el gasto operativo.",
    
    TRUE ~
      "La cooperativa muestra un desempeño financiero estable, con oportunidades de mejora en eficiencia, calidad de cartera y consolidación del crecimiento."
  )
  
  return(texto)
}

# --------------------------------------------------
# FORTALEZAS AUTOMATICAS
# --------------------------------------------------

fortalezas_auto <- function(
    productividad_data,
    growth_data,
    margen_data
) {
  
  productividad_actual <- dplyr::last(productividad_data$valor)
  
  cartera <- growth_data %>%
    dplyr::filter(indicador == "CARTERA.BRUTA") %>%
    dplyr::arrange(mes)
  
  crecimiento_cartera <- (dplyr::last(cartera$valor) / dplyr::first(cartera$valor)) - 1
  
  margen <- margen_data %>%
    dplyr::arrange(mes)
  
  crecimiento_margen <- (dplyr::last(margen$valor) / dplyr::first(margen$valor)) - 1
  
  fortalezas <- c()
  
  if (crecimiento_cartera > 0.20) {
    fortalezas <- c(fortalezas, paste0(
      "- La cartera bruta evidencia crecimiento robusto de ",
      scales::percent(crecimiento_cartera, accuracy = 0.1),
      ", reflejando expansión del negocio crediticio."
    ))
  }
  
  if (productividad_actual > 1.10) {
    fortalezas <- c(fortalezas, paste0(
      "- La relación de productividad cerró en ",
      scales::percent(productividad_actual, accuracy = 0.1),
      ", mostrando una adecuada cobertura de activos productivos frente a pasivos con costo."
    ))
  }
  
  if (crecimiento_margen > 0.20) {
    fortalezas <- c(fortalezas, paste0(
      "- El margen financiero neto mantiene una trayectoria creciente, con una variación acumulada de ",
      scales::percent(crecimiento_margen, accuracy = 0.1),
      "."
    ))
  }
  
  if (length(fortalezas) == 0) {
    fortalezas <- c("- No se identifican fortalezas sobresalientes; se recomienda profundizar el análisis financiero.")
  }
  
  paste(fortalezas, collapse = "\n")
}

# --------------------------------------------------
# RIESGOS AUTOMATICOS
# --------------------------------------------------

riesgos_auto <- function(
    morosidad_data,
    eficiencia_data,
    absorcion_data,
    ratio_cartera
) {
  
  morosidad_actual <- dplyr::last(morosidad_data$valor)
  eficiencia_actual <- dplyr::last(eficiencia_data$eficiencia)
  absorcion_actual <- dplyr::last(absorcion_data$valor)
  ratio_actual <- dplyr::last(ratio_cartera$ratio_improductiva)
  
  riesgos <- c()
  
  if (morosidad_actual >= 0.08) {
    riesgos <- c(riesgos, paste0(
      "- La morosidad ampliada cerró en ",
      scales::percent(morosidad_actual, accuracy = 0.1),
      ", por encima del umbral prudencial de vigilancia."
    ))
  }
  
  if (ratio_actual >= 0.08) {
    riesgos <- c(riesgos, paste0(
      "- El ratio de cartera improductiva alcanzó ",
      scales::percent(ratio_actual, accuracy = 0.1),
      ", evidenciando deterioro del portafolio."
    ))
  }
  
  if (eficiencia_actual >= 0.85) {
    riesgos <- c(riesgos, paste0(
      "- La eficiencia operativa se ubicó en ",
      scales::percent(eficiencia_actual, accuracy = 0.1),
      ", reflejando presión significativa de gastos operativos."
    ))
  }
  
  if (absorcion_actual >= 0.95) {
    riesgos <- c(riesgos, paste0(
      "- El grado de absorción del margen fue de ",
      scales::percent(absorcion_actual, accuracy = 0.1),
      ", lo que limita la generación de excedentes."
    ))
  }
  
  if (length(riesgos) == 0) {
    riesgos <- c("- No se identifican alertas críticas en los indicadores evaluados.")
  }
  
  paste(riesgos, collapse = "\n")
}

# --------------------------------------------------
# RECOMENDACIONES AUTOMATICAS
# --------------------------------------------------

recomendaciones_auto <- function(
    morosidad_data,
    eficiencia_data,
    absorcion_data,
    ratio_cartera
) {
  
  morosidad_actual <- dplyr::last(morosidad_data$valor)
  eficiencia_actual <- dplyr::last(eficiencia_data$eficiencia)
  absorcion_actual <- dplyr::last(absorcion_data$valor)
  ratio_actual <- dplyr::last(ratio_cartera$ratio_improductiva)
  
  recomendaciones <- c()
  
  if (morosidad_actual >= 0.08) {
    recomendaciones <- c(recomendaciones,
                         "- Fortalecer la originación crediticia, segmentación de riesgo y gestión de cobranza temprana."
    )
  }
  
  if (ratio_actual >= 0.08) {
    recomendaciones <- c(recomendaciones,
                         "- Implementar un plan intensivo de recuperación y normalización de cartera improductiva."
    )
  }
  
  if (eficiencia_actual >= 0.85 | absorcion_actual >= 0.95) {
    recomendaciones <- c(recomendaciones,
                         "- Revisar la estructura de gastos operativos y establecer metas de eficiencia por unidad de negocio."
    )
  }
  
  if (length(recomendaciones) == 0) {
    recomendaciones <- c(
      "- Mantener seguimiento periódico de indicadores para preservar el equilibrio entre crecimiento, riesgo y eficiencia."
    )
  }
  
  paste(recomendaciones, collapse = "\n")
}
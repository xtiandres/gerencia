# COAC INFORME GERENCIA 2025

# DIRECTORIO RAIZ DEL PROYECTO
if (!exists("project_root")) {
  project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
}

# LIBRERIAS
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggrepel)

# MODULOS
sys.source(
  file.path(project_root, "R", "funciones.R"),
  envir = environment()
)

sys.source(
  file.path(project_root, "R", "analisis_financiero.R"),
  envir = environment()
)

# FORMATOS DE EJES
axis_money <- scale_y_continuous(
  labels = label_number(big.mark = ",", decimal.mark = ".", prefix = "$")
)

axis_percent <- scale_y_continuous(
  labels = percent_format(accuracy = 0.1)
)

# TEMA INSTITUCIONAL
tema_institucional <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle = element_text(size = 12.5, color = "gray40", margin = margin(b = 10)),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1, color = "gray20"),
    axis.text.y = element_text(size = 10, color = "gray20"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 30, 10, 10)
  )

# PALETAS INSTITUCIONALES
colores_crecimiento <- c(
  "CARTERA.BRUTA" = "#1565C0",
  "ACTIVOS.PRODUCTIVOS" = "#2E7D32"
)

colores_estructura <- c(
  "ACTIVOS.PRODUCTIVOS" = "#1565C0",
  "PASIVOS.CON.COSTO" = "#6A1B9A"
)

colores_liquidez <- c(
  "LPL" = "#1565C0",
  "LSL" = "#00897B"
)

colores_productividad2 <- c(
  "RELACION.DE.PRODUCTIVIDAD" = "#1565C0",
  "UTILIZACION.PASIVOS.CON.COSTOS" = "#00897B"
)

colores_patrimonio <- c(
  "PATRIMONIO.TECNICO.PRIMARIO" = "#1565C0",
  "PATRIMONIO.TECNICO.SECUNDARIO" = "#90CAF9",
  "PATRIMONIO.TECNICO.CONSTITUIDO" = "#0D47A1"
)

# FUNCION PARA ETIQUETAS DE REFERENCIA A LA DERECHA
agregar_etiqueta_umbral <- function(y, label, color_hex) {
  annotate(
    "text",
    x = Inf,
    y = y,
    label = label,
    color = color_hex,
    hjust = 1.1,
    vjust = -0.4,
    size = 4
  )
}

# RUTAS
output_dir <- file.path(project_root, "output")
input_file <- file.path(project_root, "data", "INDICES2025.xlsx")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# DATABASE
data25 <- read_excel(input_file, sheet = 15)

# LIMPIEZA
data_clean <- data25 %>%
  clean_names()

data_tidy <- data_clean %>%
  pivot_longer(
    cols = -indicador,
    names_to = "mes",
    values_to = "valor"
  ) %>%
  mutate(
    indicador = make.names(indicador),
    mes = factor(
      mes,
      levels = c(
        "enero", "febrero", "marzo", "abril", "mayo", "junio",
        "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
      )
    ),
    valor = as.numeric(gsub("%", "", valor))
  )

# --------------------------------------------------
# GRAFICO - CRECIMIENTO DEL NEGOCIO - INFORME
# --------------------------------------------------

growth_data <- data_tidy %>%
  filter(indicador %in% c("CARTERA.BRUTA", "ACTIVOS.PRODUCTIVOS"))

ultimo_cartera <- growth_data %>%
  filter(indicador == "CARTERA.BRUTA", mes == "diciembre")

ultimo_activos_productivos <- growth_data %>%
  filter(indicador == "ACTIVOS.PRODUCTIVOS", mes == "diciembre")

grafico_crecimiento <- ggplot(
  growth_data,
  aes(x = mes, y = valor, color = indicador, group = indicador)
) +
  geom_line(linewidth = 1.35) +
  geom_point(size = 3.2) +
  geom_text(
    data = ultimo_cartera,
    aes(label = scales::dollar(valor, accuracy = 0.01)),
    color = "#1565C0",
    hjust = -0.1,
    vjust = -0.7,
    size = 3.8
  ) +
  geom_text(
    data = ultimo_activos_productivos,
    aes(label = scales::dollar(valor, accuracy = 0.01)),
    color = "#2E7D32",
    hjust = -0.1,
    vjust = 1.5,
    size = 3.8
  ) +
  axis_money +
  scale_color_manual(
    values = colores_crecimiento,
    labels = c(
      "CARTERA.BRUTA" = "Cartera Bruta",
      "ACTIVOS.PRODUCTIVOS" = "Activos Productivos"
    )
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.12))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Crecimiento del Negocio",
    subtitle = "Expansión sostenida del negocio crediticio con fortalecimiento de activos productivos",
    x = "Mes",
    y = "Monto (USD)",
    color = "Indicador"
  ) +
  tema_institucional

# --------------------------------------------------
# GRAFICO - ESTRUCTURA FINANCIERA
# --------------------------------------------------

estructura_data <- data_tidy %>%
  filter(indicador %in% c("ACTIVOS.PRODUCTIVOS", "PASIVOS.CON.COSTO"))

ultimo_activos_estructura <- estructura_data %>%
  filter(indicador == "ACTIVOS.PRODUCTIVOS", mes == "diciembre")

ultimo_pasivos_costo <- estructura_data %>%
  filter(indicador == "PASIVOS.CON.COSTO", mes == "diciembre")

grafico_estructura <- ggplot(
  estructura_data,
  aes(x = mes, y = valor, color = indicador, group = indicador)
) +
  geom_line(linewidth = 1.35) +
  geom_point(size = 3.2) +
  geom_text(
    data = ultimo_activos_estructura,
    aes(label = scales::dollar(valor, accuracy = 0.01)),
    color = "#1565C0",
    hjust = -0.1,
    vjust = -0.7,
    size = 3.8
  ) +
  geom_text(
    data = ultimo_pasivos_costo,
    aes(label = scales::dollar(valor, accuracy = 0.01)),
    color = "#6A1B9A",
    hjust = -0.1,
    vjust = 1.5,
    size = 3.8
  ) +
  axis_money +
  scale_color_manual(
    values = colores_estructura,
    labels = c(
      "ACTIVOS.PRODUCTIVOS" = "Activos Productivos",
      "PASIVOS.CON.COSTO" = "Pasivos con Costo"
    )
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.12))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Estructura Financiera",
    subtitle = "Uso equilibrado de recursos captados con adecuada generación de activos productivos",
    x = "Mes",
    y = "Monto (USD)",
    color = "Indicador"
  ) +
  tema_institucional

# --------------------------------------------------
# GRAFICO - PRODUCTIVIDAD DEL BALANCE
# --------------------------------------------------

productividad_data <- data_tidy %>%
  filter(indicador == "RELACION.DE.PRODUCTIVIDAD")

texto_productividad <- analisis_productividad(productividad_data)

ultimo_productividad <- productividad_data %>%
  filter(mes == "diciembre")

grafico_productividad <- ggplot(
  productividad_data,
  aes(x = mes, y = valor, group = 1)
) +
  geom_line(color = "#1565C0", linewidth = 1.35) +
  geom_point(color = "#1565C0", size = 3.2) +
  geom_text(
    data = ultimo_productividad,
    aes(label = scales::percent(valor, accuracy = 0.1)),
    color = "#1565C0",
    hjust = -0.1,
    vjust = -0.7,
    size = 3.8
  ) +
  axis_percent +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.12))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Relación de Productividad",
    subtitle = "Generación adecuada de activos productivos en relación con los pasivos con costo",
    x = "Mes",
    y = "Porcentaje"
  ) +
  tema_institucional

# --------------------------------------------------
# GRAFICO - CALIDAD DE CARTERA MOROSIDAD - INFORME
# --------------------------------------------------

morosidad_data <- data_tidy %>%
  filter(indicador == "MOROSIDAD.AMPLIADA")

texto_morosidad <- analisis_morosidad(morosidad_data)

ultimo_morosidad <- morosidad_data %>%
  filter(mes == "diciembre")

grafico_morosidad <- ggplot(
  morosidad_data,
  aes(x = mes, y = valor, group = 1)
) +
  geom_line(color = "#C62828", linewidth = 1.35) +
  geom_point(color = "#C62828", size = 3.2) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
  geom_hline(yintercept = 0.08, linetype = "dashed", color = "orange", linewidth = 0.8) +
  geom_hline(yintercept = 0.10, linetype = "dashed", color = "red", linewidth = 0.8) +
  agregar_etiqueta_umbral(0.05, "5%", "darkgreen") +
  agregar_etiqueta_umbral(0.08, "8%", "orange") +
  agregar_etiqueta_umbral(0.10, "10%", "red") +
  geom_text(
    data = ultimo_morosidad,
    aes(label = scales::percent(valor, accuracy = 0.1)),
    color = "#C62828",
    hjust = -0.1,
    vjust = -0.7,
    size = 3.8
  ) +
  axis_percent +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.15))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Calidad de Cartera - Morosidad",
    subtitle = "Control del riesgo crediticio con niveles de morosidad dentro de parámetros gestionables",
    x = "Mes",
    y = "Porcentaje"
  ) +
  tema_institucional

# --------------------------------------------------
# GRAFICO - MARGEN FINANCIERO NETO
# --------------------------------------------------

margen_data <- data_tidy %>%
  filter(indicador == "MARGEN.FINANCIERO.NETO")

ultimo_margen <- margen_data %>%
  filter(mes == "diciembre")

grafico_margen <- ggplot(
  margen_data,
  aes(x = mes, y = valor, group = 1)
) +
  geom_line(color = "#2E7D32", linewidth = 1.35) +
  geom_point(color = "#2E7D32", size = 3.2) +
  geom_text(
    data = ultimo_margen,
    aes(label = scales::dollar(valor, accuracy = 0.01)),
    color = "#2E7D32",
    hjust = -0.1,
    vjust = -0.7,
    size = 3.8
  ) +
  axis_money +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.12))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Margen Financiero Neto",
    subtitle = "Fortalecimiento progresivo de la capacidad institucional de generación de ingresos",
    x = "Mes",
    y = "Monto (USD)"
  ) +
  tema_institucional

# --------------------------------------------------
# GRAFICO - RENTABILIDAD Y EFICIENCIA - INFORME
# --------------------------------------------------

absorcion_data <- data_tidy %>%
  filter(indicador == "GRADO.DE.ABSORCION.DEL.MARGEN.FINANCIERO")

texto_absorcion <- analisis_absorcion(absorcion_data)

ultimo_absorcion <- absorcion_data %>%
  filter(mes == "diciembre")

grafico_absorcion <- ggplot(
  absorcion_data,
  aes(x = mes, y = valor, group = 1)
) +
  geom_line(color = "#6A1B9A", linewidth = 1.35) +
  geom_point(color = "#6A1B9A", size = 3.2) +
  geom_hline(yintercept = 0.85, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red", linewidth = 0.8) +
  agregar_etiqueta_umbral(0.85, "85%", "darkgreen") +
  agregar_etiqueta_umbral(0.95, "95%", "red") +
  geom_text(
    data = ultimo_absorcion,
    aes(label = scales::percent(valor, accuracy = 0.1)),
    color = "#6A1B9A",
    hjust = -0.1,
    vjust = -0.7,
    size = 3.8
  ) +
  axis_percent +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.15))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Rentabilidad y Eficiencia",
    subtitle = "Estructura operativa en proceso de optimización con adecuada cobertura del margen financiero",
    x = "Mes",
    y = "Porcentaje"
  ) +
  tema_institucional

# --------------------------------------------------
# GRAFICO - RATIO CARTERA IMPRODUCTIVA
# --------------------------------------------------

ratio_cartera <- data_tidy %>%
  filter(indicador %in% c("CARTERA.IMPRODUCTIVA", "CARTERA.BRUTA")) %>%
  pivot_wider(names_from = indicador, values_from = valor) %>%
  mutate(
    ratio_improductiva = CARTERA.IMPRODUCTIVA / CARTERA.BRUTA
  )

texto_ratio_cartera <- analisis_ratio_cartera(ratio_cartera)

ultimo_ratio <- ratio_cartera %>%
  filter(mes == "diciembre")

grafico_ratio_cartera <- ggplot(
  ratio_cartera,
  aes(x = mes, y = ratio_improductiva, group = 1)
) +
  geom_line(color = "#C62828", linewidth = 1.35) +
  geom_point(color = "#C62828", size = 3.2) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
  geom_hline(yintercept = 0.08, linetype = "dashed", color = "red", linewidth = 0.8) +
  agregar_etiqueta_umbral(0.05, "5%", "darkgreen") +
  agregar_etiqueta_umbral(0.08, "8%", "red") +
  geom_text(
    data = ultimo_ratio,
    aes(label = scales::percent(ratio_improductiva, accuracy = 0.1)),
    color = "#C62828",
    hjust = -0.1,
    vjust = -0.7,
    size = 3.8
  ) +
  axis_percent +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.15))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Ratio de Cartera Improductiva",
    subtitle = "Seguimiento prudente del deterioro relativo de la cartera frente al crecimiento del portafolio",
    x = "Mes",
    y = "Porcentaje"
  ) +
  tema_institucional

# --------------------------------------------------
# GRAFICO - EFICIENCIA OPERATIVA
# --------------------------------------------------

eficiencia_data <- data_tidy %>%
  filter(indicador %in% c("GASTOS.DE.OPERACION", "MARGEN.FINANCIERO.NETO")) %>%
  pivot_wider(names_from = indicador, values_from = valor) %>%
  mutate(
    eficiencia = GASTOS.DE.OPERACION / MARGEN.FINANCIERO.NETO
  )

texto_eficiencia <- analisis_eficiencia(eficiencia_data)

ultimo_eficiencia <- eficiencia_data %>%
  filter(mes == "diciembre")

grafico_eficiencia <- ggplot(
  eficiencia_data,
  aes(x = mes, y = eficiencia, group = 1)
) +
  geom_line(color = "#6A1B9A", linewidth = 1.35) +
  geom_point(color = "#6A1B9A", size = 3.2) +
  geom_hline(yintercept = 0.70, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
  geom_hline(yintercept = 0.85, linetype = "dashed", color = "orange", linewidth = 0.8) +
  geom_hline(yintercept = 1.00, linetype = "dashed", color = "red", linewidth = 0.8) +
  agregar_etiqueta_umbral(0.70, "70%", "darkgreen") +
  agregar_etiqueta_umbral(0.85, "85%", "orange") +
  agregar_etiqueta_umbral(1.00, "100%", "red") +
  geom_text(
    data = ultimo_eficiencia,
    aes(label = scales::percent(eficiencia, accuracy = 0.1)),
    color = "#6A1B9A",
    hjust = -0.1,
    vjust = -0.7,
    size = 3.8
  ) +
  axis_percent +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.15))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Eficiencia Operativa",
    subtitle = "Seguimiento de costos operativos con oportunidades de mejora en productividad institucional",
    x = "Mes",
    y = "Porcentaje"
  ) +
  tema_institucional

# --------------------------------------------------
# GRAFICO - RIESGO VS CRECIMIENTO
# --------------------------------------------------

riesgo_crecimiento_data <- data_tidy %>%
  filter(indicador %in% c("CARTERA.BRUTA", "MOROSIDAD.AMPLIADA")) %>%
  pivot_wider(names_from = indicador, values_from = valor)

grafico_riesgo_crecimiento <- ggplot(
  riesgo_crecimiento_data,
  aes(x = CARTERA.BRUTA, y = MOROSIDAD.AMPLIADA, label = mes)
) +
  geom_point(size = 3.2, color = "#C62828") +
  geom_text(vjust = -0.8, size = 3.5) +
  scale_x_continuous(
    labels = label_number(big.mark = ",", decimal.mark = ".", prefix = "$")
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1)
  ) +
  labs(
    title = "Relación entre Crecimiento y Riesgo",
    subtitle = "Expansión del negocio acompañada de monitoreo permanente del riesgo crediticio",
    x = "Cartera Bruta (USD)",
    y = "Morosidad Ampliada"
  ) +
  tema_institucional

# --------------------------------------------------
# GRAFICO - LIQUIDEZ (LPrimeraL vs LSegundaL) - INFORME
# --------------------------------------------------

liquidez_data <- data_tidy %>%
  filter(indicador %in% c("LPL", "LSL"))

ultimo_lpl <- liquidez_data %>%
  filter(indicador == "LPL", mes == "diciembre")

ultimo_lsl <- liquidez_data %>%
  filter(indicador == "LSL", mes == "diciembre")

grafico_liquidez <- ggplot(
  liquidez_data,
  aes(x = mes, y = valor, color = indicador, group = indicador)
) +
  geom_line(linewidth = 1.35) +
  geom_point(size = 3.2) +
  geom_hline(yintercept = 0.12, linetype = "dashed", color = "#2E7D32", linewidth = 0.8) +
  geom_hline(yintercept = 0.18, linetype = "dashed", color = "#C62828", linewidth = 0.8) +
  agregar_etiqueta_umbral(0.12, "12%", "#2E7D32") +
  agregar_etiqueta_umbral(0.18, "18%", "#C62828") +
  geom_text(
    data = ultimo_lpl,
    aes(label = scales::percent(valor, accuracy = 0.1)),
    color = "#1565C0",
    hjust = -0.1,
    vjust = -0.7,
    size = 3.8
  ) +
  geom_text(
    data = ultimo_lsl,
    aes(label = scales::percent(valor, accuracy = 0.1)),
    color = "#00897B",
    hjust = -0.1,
    vjust = 1.5,
    size = 3.8
  ) +
  axis_percent +
  scale_color_manual(
    values = colores_liquidez,
    labels = c(
      "LPL" = "Liquidez de Primera Línea",
      "LSL" = "Liquidez de Segunda Línea"
    )
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.15))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Indicadores de Liquidez (LPL vs LSL)",
    subtitle = "Adecuada capacidad de cobertura de obligaciones con gestión activa de liquidez",
    x = "Mes",
    y = "Porcentaje",
    color = "Indicador"
  ) +
  tema_institucional

# --------------------------------------------------
# GRAFICO - PRODUCTIVIDAD - INFORME
# --------------------------------------------------

productividad2_data <- data_tidy %>%
  filter(indicador %in% c("RELACION.DE.PRODUCTIVIDAD", "UTILIZACION.PASIVOS.CON.COSTOS"))

ultimo_relacion_productividad <- productividad2_data %>%
  filter(indicador == "RELACION.DE.PRODUCTIVIDAD", mes == "diciembre")

ultimo_utilizacion_pasivos <- productividad2_data %>%
  filter(indicador == "UTILIZACION.PASIVOS.CON.COSTOS", mes == "diciembre")

grafico_productividad2 <- ggplot(
  productividad2_data,
  aes(x = mes, y = valor, color = indicador, group = indicador)
) +
  geom_line(linewidth = 1.35) +
  geom_point(size = 3.2) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "orange", linewidth = 0.8) +
  geom_hline(yintercept = 0.90, linetype = "dashed", color = "red", linewidth = 0.8) +
  agregar_etiqueta_umbral(0.80, "80%", "orange") +
  agregar_etiqueta_umbral(0.90, "90%", "red") +
  geom_text(
    data = ultimo_relacion_productividad,
    aes(label = scales::percent(valor, accuracy = 0.1)),
    color = "#1565C0",
    hjust = -0.1,
    vjust = -0.7,
    size = 3.8
  ) +
  geom_text(
    data = ultimo_utilizacion_pasivos,
    aes(label = scales::percent(valor, accuracy = 0.1)),
    color = "#00897B",
    hjust = -0.1,
    vjust = 1.5,
    size = 3.8
  ) +
  axis_percent +
  scale_color_manual(
    values = colores_productividad2,
    labels = c(
      "RELACION.DE.PRODUCTIVIDAD" = "Relación de Productividad",
      "UTILIZACION.PASIVOS.CON.COSTOS" = "Utilización de Pasivos con Costo"
    )
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.15))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Productividad",
    subtitle = "Uso eficiente de los recursos captados con adecuada generación de activos productivos",
    x = "Mes",
    y = "Porcentaje",
    color = "Indicador"
  ) +
  tema_institucional

# --------------------------------------------------
# GRAFICO - PATRIMONIO TECNICO - INFORME
# --------------------------------------------------

patrimonio_tecnico_data <- data_tidy %>%
  filter(indicador %in% c(
    "PATRIMONIO.TECNICO.PRIMARIO",
    "PATRIMONIO.TECNICO.SECUNDARIO",
    "PATRIMONIO.TECNICO.CONSTITUIDO"
  ))

ultimo_primario <- patrimonio_tecnico_data %>%
  filter(indicador == "PATRIMONIO.TECNICO.PRIMARIO", mes == "diciembre")

ultimo_secundario <- patrimonio_tecnico_data %>%
  filter(indicador == "PATRIMONIO.TECNICO.SECUNDARIO", mes == "diciembre")

ultimo_constituido <- patrimonio_tecnico_data %>%
  filter(indicador == "PATRIMONIO.TECNICO.CONSTITUIDO", mes == "diciembre")

grafico_patrimonio_tecnico <- ggplot(
  patrimonio_tecnico_data,
  aes(x = mes, y = valor, color = indicador, group = indicador)
) +
  geom_line(linewidth = 1.35) +
  geom_point(size = 3.2) +
  geom_text(
    data = ultimo_primario,
    aes(label = scales::dollar(valor, accuracy = 0.01)),
    color = "#1565C0",
    hjust = -0.1,
    vjust = -0.7,
    size = 3.8
  ) +
  geom_text(
    data = ultimo_secundario,
    aes(label = scales::dollar(valor, accuracy = 0.01)),
    color = "#90CAF9",
    hjust = -0.1,
    vjust = 1.6,
    size = 3.8
  ) +
  geom_text(
    data = ultimo_constituido,
    aes(label = scales::dollar(valor, accuracy = 0.01)),
    color = "#0D47A1",
    hjust = -0.1,
    vjust = -0.7,
    size = 3.8
  ) +
  axis_money +
  scale_color_manual(
    values = colores_patrimonio,
    labels = c(
      "PATRIMONIO.TECNICO.PRIMARIO" = "Patrimonio Técnico Primario",
      "PATRIMONIO.TECNICO.SECUNDARIO" = "Patrimonio Técnico Secundario",
      "PATRIMONIO.TECNICO.CONSTITUIDO" = "Patrimonio Técnico Constituido"
    )
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.12))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Patrimonio Técnico",
    subtitle = "Fortalecimiento progresivo del patrimonio técnico como base de estabilidad institucional",
    x = "Mes",
    y = "Monto (USD)",
    color = "Indicador"
  ) +
  tema_institucional

# --------------------------------------------------
# GRAFICO - SOLVENCIA
# --------------------------------------------------

solvencia_data <- data_tidy %>%
  filter(indicador == "SOLVENCIA")

ultimo_solvencia <- solvencia_data %>%
  filter(mes == "diciembre")

grafico_solvencia <- ggplot(
  solvencia_data,
  aes(x = mes, y = valor, group = 1)
) +
  geom_line(color = "#C62828", linewidth = 1.35) +
  geom_point(color = "#C62828", size = 3.2) +
  geom_hline(yintercept = 0.09, linetype = "dashed", color = "#2E7D32", linewidth = 0.8) +
  agregar_etiqueta_umbral(0.09, "9%", "#2E7D32") +
  geom_text(
    data = ultimo_solvencia,
    aes(label = scales::percent(valor, accuracy = 0.1)),
    color = "#C62828",
    hjust = -0.1,
    vjust = -0.7,
    size = 3.8
  ) +
  axis_percent +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.15))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Solvencia",
    subtitle = "Niveles de solvencia sólidos por encima de los requerimientos regulatorios",
    x = "Mes",
    y = "Porcentaje"
  ) +
  tema_institucional

# --------------------------------------------------
# GRAFICO 15 RESULTADO DEL EJERCICIO
# VERSION CONSULTORIA: barras agrupadas + linea de excedente
# --------------------------------------------------

resultado_data <- data_tidy %>%
  filter(indicador %in% c(
    "TOTAL.INGRESOS",
    "TOTAL.GASTOS",
    "EXCEDENTE"
  ))

resultado_barras <- resultado_data %>%
  filter(indicador %in% c("TOTAL.INGRESOS", "TOTAL.GASTOS"))

resultado_excedente <- resultado_data %>%
  filter(indicador == "EXCEDENTE")

ultimo_ingresos <- resultado_barras %>%
  filter(indicador == "TOTAL.INGRESOS", mes == "diciembre")

ultimo_gastos <- resultado_barras %>%
  filter(indicador == "TOTAL.GASTOS", mes == "diciembre")

ultimo_excedente <- resultado_excedente %>%
  filter(mes == "diciembre")

colores_resultado_barras <- c(
  "TOTAL.INGRESOS" = "#1565C0",
  "TOTAL.GASTOS" = "#C62828"
)

grafico_resultado <- ggplot() +
  geom_col(
    data = resultado_barras,
    aes(x = mes, y = valor, fill = indicador),
    position = position_dodge(width = 0.72),
    width = 0.62,
    alpha = 0.92
  ) +
  geom_line(
    data = resultado_excedente,
    aes(x = mes, y = valor, color = "EXCEDENTE", group = 1),
    linewidth = 1.25
  ) +
  geom_point(
    data = resultado_excedente,
    aes(x = mes, y = valor, color = "EXCEDENTE"),
    size = 2.8
  ) +
  geom_text(
    data = ultimo_ingresos,
    aes(x = mes, y = valor, label = scales::dollar(valor, accuracy = 0.01)),
    color = "#1565C0",
    hjust = -0.1,
    vjust = -0.7,
    size = 3.8
  ) +
  geom_text(
    data = ultimo_gastos,
    aes(x = mes, y = valor, label = scales::dollar(valor, accuracy = 0.01)),
    color = "#C62828",
    hjust = -0.1,
    vjust = 1.6,
    size = 3.8
  ) +
  geom_text(
    data = ultimo_excedente,
    aes(x = mes, y = valor, label = scales::dollar(valor, accuracy = 0.01)),
    color = "#2E7D32",
    hjust = -0.1,
    vjust = -0.8,
    size = 3.8
  ) +
  axis_money +
  scale_fill_manual(
    values = colores_resultado_barras,
    labels = c(
      "TOTAL.INGRESOS" = "Total Ingresos",
      "TOTAL.GASTOS" = "Total Gastos"
    )
  ) +
  scale_color_manual(
    values = c("EXCEDENTE" = "#2E7D32"),
    labels = c("EXCEDENTE" = "Excedente")
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.12))) +
  coord_cartesian(clip = "off") +
  guides(
    fill = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  labs(
    title = "Resultado del Ejercicio",
    subtitle = "Crecimiento sostenido de la operación con excedente positivo al cierre del periodo",
    x = "Mes",
    y = "Monto (USD)",
    fill = "Indicador",
    color = "Resultado"
  ) +
  tema_institucional

# --------------------------------------------------
# GRAFICO 16 ESTRUCTURA PATRIMONIAL
# VERSION CONSULTORIA: lineas + cierre destacado
# TOTAL ACTIVOS vs TOTAL PASIVOS
# --------------------------------------------------

estructura_patrimonial_data <- data_tidy %>%
  filter(indicador %in% c(
    "TOTAL.ACTIVOS",
    "TOTAL.PASIVOS"
  ))

colores_estructura_patrimonial <- c(
  "TOTAL.ACTIVOS" = "#1565C0",
  "TOTAL.PASIVOS" = "#6A1B9A"
)

ultimo_activos <- estructura_patrimonial_data %>%
  filter(indicador == "TOTAL.ACTIVOS", mes == "diciembre")

ultimo_pasivos <- estructura_patrimonial_data %>%
  filter(indicador == "TOTAL.PASIVOS", mes == "diciembre")

grafico_estructura_patrimonial <- ggplot(
  estructura_patrimonial_data,
  aes(x = mes, y = valor, color = indicador, group = indicador)
) +
  geom_line(linewidth = 1.35) +
  geom_point(size = 3.2) +
  geom_text(
    data = ultimo_activos,
    aes(x = mes, y = valor, label = scales::dollar(valor, accuracy = 0.01)),
    color = "#1565C0",
    hjust = -0.1,
    vjust = -0.7,
    size = 3.8
  ) +
  geom_text(
    data = ultimo_pasivos,
    aes(x = mes, y = valor, label = scales::dollar(valor, accuracy = 0.01)),
    color = "#6A1B9A",
    hjust = -0.1,
    vjust = 1.5,
    size = 3.8
  ) +
  axis_money +
  scale_color_manual(
    values = colores_estructura_patrimonial,
    labels = c(
      "TOTAL.ACTIVOS" = "Total Activos",
      "TOTAL.PASIVOS" = "Total Pasivos"
    )
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.12))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Estructura Patrimonial",
    subtitle = "Crecimiento de la base patrimonial con adecuada relación entre activos y pasivos",
    x = "Mes",
    y = "Monto (USD)",
    color = "Indicador"
  ) +
  tema_institucional

# --------------------------------------------------
# TEXTOS AUTOMATICOS PARA EL INFORME
# --------------------------------------------------

texto_resumen_ejecutivo <- resumen_ejecutivo_auto(
  morosidad_data = morosidad_data,
  productividad_data = productividad_data,
  eficiencia_data = eficiencia_data,
  absorcion_data = absorcion_data,
  ratio_cartera = ratio_cartera,
  growth_data = growth_data
)

texto_conclusion_general <- conclusion_general_auto(
  morosidad_data = morosidad_data,
  productividad_data = productividad_data,
  eficiencia_data = eficiencia_data,
  absorcion_data = absorcion_data,
  ratio_cartera = ratio_cartera,
  growth_data = growth_data
)

texto_fortalezas <- fortalezas_auto(
  productividad_data = productividad_data,
  growth_data = growth_data,
  margen_data = margen_data
)

texto_riesgos <- riesgos_auto(
  morosidad_data = morosidad_data,
  eficiencia_data = eficiencia_data,
  absorcion_data = absorcion_data,
  ratio_cartera = ratio_cartera
)

texto_recomendaciones <- recomendaciones_auto(
  morosidad_data = morosidad_data,
  eficiencia_data = eficiencia_data,
  absorcion_data = absorcion_data,
  ratio_cartera = ratio_cartera
)

# --------------------------------------------------
# EXPORTAR TODAS LAS GRAFICAS
# --------------------------------------------------

graficos <- list(
  crecimiento = grafico_crecimiento,
  estructura = grafico_estructura,
  productividad = grafico_productividad,
  morosidad = grafico_morosidad,
  margen = grafico_margen,
  absorcion = grafico_absorcion,
  ratio = grafico_ratio_cartera,
  eficiencia = grafico_eficiencia,
  riesgo_crecimiento = grafico_riesgo_crecimiento,
  liquidez = grafico_liquidez,
  productividad2 = grafico_productividad2,
  patrimonio_tecnico = grafico_patrimonio_tecnico,
  solvencia = grafico_solvencia,
  resultado = grafico_resultado,
  estructura_patrimonial = grafico_estructura_patrimonial
)

for (nombre in names(graficos)) {
  ggsave(
    filename = file.path(output_dir, paste0(nombre, ".png")),
    plot = graficos[[nombre]],
    width = 10,
    height = 6,
    dpi = 300
  )

  ggsave(
    filename = file.path(output_dir, paste0(nombre, ".pdf")),
    plot = graficos[[nombre]],
    width = 10,
    height = 6
  )
}

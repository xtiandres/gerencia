library(dplyr)
library(scales)
library(ggrepel)

annotate_extremos <- function(df, value_col = "valor") {
  max_point <- df %>%
    filter(.data[[value_col]] == max(.data[[value_col]], na.rm = TRUE)) %>%
    slice(1)
  
  min_point <- df %>%
    filter(.data[[value_col]] == min(.data[[value_col]], na.rm = TRUE)) %>%
    slice(1)
  
  list(
    max = max_point,
    min = min_point
  )
}

add_extremos <- function(grafico, extremos, tipo = "numero", value_col = "valor") {
  if (nrow(extremos$max) == 0 || nrow(extremos$min) == 0) {
    return(grafico)
  }
  
  if (tipo == "porcentaje") {
    extremos$max$etiqueta <- paste0("Max: ", scales::percent(extremos$max[[value_col]], accuracy = 0.1))
    extremos$min$etiqueta <- paste0("Min: ", scales::percent(extremos$min[[value_col]], accuracy = 0.1))
  } else {
    extremos$max$etiqueta <- paste0("Max: ", scales::comma(extremos$max[[value_col]], accuracy = 0.01))
    extremos$min$etiqueta <- paste0("Min: ", scales::comma(extremos$min[[value_col]], accuracy = 0.01))
  }
  
  grafico +
    ggrepel::geom_label_repel(
      data = extremos$max,
      aes(label = etiqueta),
      color = "darkgreen",
      size = 3.3,
      min.segment.length = 0,
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    ggrepel::geom_label_repel(
      data = extremos$min,
      aes(label = etiqueta),
      color = "red3",
      size = 3.3,
      min.segment.length = 0,
      na.rm = TRUE,
      show.legend = FALSE
    )
}
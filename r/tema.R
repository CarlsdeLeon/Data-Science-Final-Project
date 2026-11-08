# Tema y colores comunes para todos los gráficos en R
library(ggplot2)

# Paleta de 12 colores (los gráficos del proyecto usan como máximo 12 categorías)
paleta <- c("#2E6F40", "#3A7CA5", "#F2C14E", "#B2182B", "#6A4C93", "#E07A5F",
            "#3D405B", "#81B29A", "#F4A261", "#264653", "#8AB17D", "#BC4B51")

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(color = "grey40"),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold"),
      legend.position  = "bottom"
    )
)

options(ggplot2.discrete.colour = paleta, ggplot2.discrete.fill = paleta)

# Fases 3 a 5: ¿incluir las 3,201 filas imputadas (frutales 1969/70 a 1975/76)?
#   FALSE = solo datos observados (recomendado para estadísticas y tendencias)
#   TRUE  = dataset completado con las imputaciones
usar_imputadas <- FALSE

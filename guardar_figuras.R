# =============================================================================
#  Guarda las gráficas clave del proyecto como imágenes .png dentro de img/
#  para poder mostrarlas en el README de GitHub.
#
#  IMPORTANTE: primero ejecuta tu análisis completo (proyecto_severidad.R)
#  con el botón "Source". Eso crea en memoria los objetos que este script
#  necesita (arbol_estados, d5_sev_final1, agg_final).
#  Después abre este archivo y ejecútalo también con "Source".
#
#  Al terminar tendrás la carpeta img/ con:
#    - clustering.png        (dendrograma de agrupación de estados)
#    - adn_danos_grupos.png  (densidad de daños por grupo)
#    - ajuste_grupo.png      (QQ-plot de validación del ajuste)
# =============================================================================

library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(actuar)

# Ubica la carpeta del proyecto (donde vive este script) y crea img/
localizar <- function() {
  for (i in sys.nframe():1) {
    of <- sys.frame(i)$ofile
    if (!is.null(of)) return(dirname(normalizePath(of)))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    return(dirname(normalizePath(rstudioapi::getSourceEditorContext()$path)))
  }
  getwd()
}
carpeta_R <- localizar()
raiz      <- dirname(carpeta_R)
dir_img   <- file.path(raiz, "img")
dir.create(dir_img, showWarnings = FALSE)

# --- 1) Dendrograma de clustering -------------------------------------------
png(file.path(dir_img, "clustering.png"), width = 1000, height = 700, res = 120)
plot(arbol_estados,
     main = "Agrupamiento de Estados por Similitud en Daños",
     xlab = "Estados de la República",
     ylab = "Distancia (Kolmogorov-Smirnov)",
     sub = "", cex = 0.8)
rect.hclust(arbol_estados, k = 3, border = "blue")
dev.off()

# --- 2) "ADN de daños" por grupo (densidad) ---------------------------------
p_adn <- ggplot(d5_sev_final1, aes(x = log_danos, fill = Grupo_Severidad)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~Grupo_Severidad) +
  labs(title = "ADN de Daños por Grupo",
       x = "Logaritmo de Daños", y = "Densidad")
ggsave(file.path(dir_img, "adn_danos_grupos.png"),
       p_adn, width = 9, height = 6, dpi = 120)

# --- 3) Validación del ajuste (QQ-plot, Grupo 1 con Pareto) ------------------
x_g1 <- agg_final %>%
  filter(Grupo_Severidad == 1) %>%
  pull(`Total de Daños MDP (Ajustados a 2024)`)
x_g1 <- x_g1[x_g1 > 0]
fit_g1 <- fitdist(x_g1, "pareto", method = "mle")

png(file.path(dir_img, "ajuste_grupo.png"), width = 1000, height = 700, res = 120)
qqcomp(fit_g1, main = "QQ-Plot — Grupo 1 (Pareto)")
dev.off()

message("¡Listo! Imágenes guardadas en: ", dir_img)

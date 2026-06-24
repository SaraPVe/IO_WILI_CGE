############################################################
# 0. Librerías
############################################################

# Instala si hace falta:
# install.packages("readxl")
# install.packages("openxlsx")
# install.packages("writexl")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("plotly")

library(readxl)
library(openxlsx)
library(writexl)
library(dplyr)
library(ggplot2)
library(plotly)

############################################################
# 1. Archivos de entrada / salida
############################################################

archivo_wiliam  <- "W.xlsx"          # Matriz WILIAM
archivo_unizar  <- "IO_wiliam.xlsx"  # Matriz UNIZAR (cámbialo si no es esta)
nombre_excel_salida <- "analisis_UNIZAR_vs_WILIAM.xlsx"

############################################################
# 2. Cargar datos + etiquetas + matrices numéricas
############################################################

# 2.1. Cargar WILIAM
datos_wiliam_completo <- read_excel(archivo_wiliam, sheet = 1, col_names = FALSE)

# Etiquetas de filas (primera columna, sin la celda [1,1])
etiquetas_filas    <- as.vector(unlist(datos_wiliam_completo[-1, 1]))
# Etiquetas de columnas (primera fila, sin la celda [1,1])
etiquetas_columnas <- as.vector(unlist(datos_wiliam_completo[1, -1]))

# Matriz numérica WILIAM (sin primera fila ni primera columna)
matriz_wiliam <- datos_wiliam_completo[-1, -1]
colnames(matriz_wiliam) <- NULL

matriz_wiliam_num <- as.matrix(matriz_wiliam)
storage.mode(matriz_wiliam_num) <- "double"

# 2.2. Cargar UNIZAR (misma estructura: 1ª fila/columna como etiquetas)
datos_unizar_completo <- read_excel(archivo_unizar, sheet = 1, col_names = FALSE)

matriz_unizar <- datos_unizar_completo[-1, -1]
colnames(matriz_unizar) <- NULL

matriz_unizar_num <- as.matrix(matriz_unizar)
storage.mode(matriz_unizar_num) <- "double"

# 2.3. Comprobaciones básicas
if (!identical(dim(matriz_wiliam_num), dim(matriz_unizar_num))) {
  stop(paste(
    "ERROR: Las matrices numéricas no tienen las mismas dimensiones.\n",
    "WILIAM: Filas =", nrow(matriz_wiliam_num), "Cols =", ncol(matriz_wiliam_num), "\n",
    "UNIZAR: Filas =", nrow(matriz_unizar_num), "Cols =", ncol(matriz_unizar_num)
  ))
}

if (length(etiquetas_filas) != nrow(matriz_wiliam_num)) {
  stop("El número de etiquetas de filas no coincide con las filas de la matriz.")
}
if (length(etiquetas_columnas) != ncol(matriz_wiliam_num)) {
  stop("El número de etiquetas de columnas no coincide con las columnas de la matriz.")
}

############################################################
# 3. Ceros: conteos con las matrices originales
############################################################

is_zero_W <- (matriz_wiliam_num == 0)
is_zero_U <- (matriz_unizar_num == 0)

zeros_both_0     <-  is_zero_W &  is_zero_U       # W=0, U=0
zeros_W_0_U_neq0 <-  is_zero_W & !is_zero_U       # W=0, U!=0
zeros_U_0_W_neq0 <-  is_zero_U & !is_zero_W       # U=0, W!=0

n_W_0_U_neq0 <- sum(zeros_W_0_U_neq0, na.rm = TRUE)
n_U_0_W_neq0 <- sum(zeros_U_0_W_neq0, na.rm = TRUE)
n_both_0     <- sum(zeros_both_0,     na.rm = TRUE)

cat("Celdas con WILIAM = 0 y UNIZAR ≠ 0:", n_W_0_U_neq0, "\n")
cat("Celdas con UNIZAR = 0 y WILIAM ≠ 0:", n_U_0_W_neq0, "\n")
cat("Celdas con WILIAM = 0 y UNIZAR = 0:", n_both_0, "\n")

############################################################
# 4. Tasa de variación UNIZAR respecto a WILIAM
############################################################
# Fórmula base: (UNIZAR - WILIAM) / WILIAM

matriz_tasa <- (matriz_unizar_num - matriz_wiliam_num) / matriz_wiliam_num

# 4.1. Si W=0 y U=0 → forzamos tasa = 0 (en vez de 0/0 = NaN)
matriz_tasa[zeros_both_0] <- 0

# 4.2. Para el resto de celdas, si la tasa no es finita (Inf, -Inf, NaN) → NA
matriz_tasa[!zeros_both_0 & !is.finite(matriz_tasa)] <- NA

# Matriz en formato ancho (para Excel) CON identificadores
df_tasa_resultado <- as.data.frame(matriz_tasa)
colnames(df_tasa_resultado) <- etiquetas_columnas
rownames(df_tasa_resultado) <- etiquetas_filas

############################################################
# 5. Tabla larga con etiquetas, flags y país
############################################################

n_filas <- nrow(matriz_wiliam_num)
n_cols  <- ncol(matriz_wiliam_num)

df_long <- data.frame(
  i = rep(seq_len(n_filas), times = n_cols),
  j = rep(seq_len(n_cols),  each = n_filas)
)

df_long <- df_long %>%
  mutate(
    fila     = etiquetas_filas[i],
    columna  = etiquetas_columnas[j],
    WILIAM   = as.vector(matriz_wiliam_num),
    UNIZAR   = as.vector(matriz_unizar_num),
    tasa     = as.vector(matriz_tasa),
    abs_tasa = abs(tasa),
    W0_Uneq0 = as.vector(zeros_W_0_U_neq0),
    U0_Wneq0 = as.vector(zeros_U_0_W_neq0),
    both0    = as.vector(zeros_both_0),
    # País extraído de etiquetas tipo "PAIS1_SECTOR1" o "PAIS1-SECTOR1"
    pais_fila    = sub("[_-].*$", "", fila),
    pais_columna = sub("[_-].*$", "", columna)
  )

# Usamos todas las celdas con tasa finita (incluye both0 con tasa=0)
df_long_valid <- df_long %>%
  filter(is.finite(tasa))

############################################################
# 6. Tablas de ceros (para mirar en Excel si quieres)
############################################################

df_ceros_W0_Uneq0 <- df_long %>% filter(W0_Uneq0)
df_ceros_U0_Wneq0 <- df_long %>% filter(U0_Wneq0)
df_ceros_both0    <- df_long %>% filter(both0)   # estas ahora tienen tasa = 0

############################################################
# 7. Resúmenes numéricos (todas las celdas con tasa definida)
############################################################

# 7.1. Resumen global
resumen_global <- df_long_valid %>%
  summarise(
    media_tasa       = mean(tasa, na.rm = TRUE),
    media_abs_tasa   = mean(abs_tasa, na.rm = TRUE),
    mediana_abs_tasa = median(abs_tasa, na.rm = TRUE),
    max_abs_tasa     = max(abs_tasa, na.rm = TRUE)
  )

# 7.2. Resumen por fila
resumen_filas <- df_long_valid %>%
  group_by(fila) %>%
  summarise(
    media_abs_tasa = mean(abs_tasa, na.rm = TRUE),
    max_abs_tasa   = max(abs_tasa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(media_abs_tasa))

# 7.3. Resumen por columna
resumen_columnas <- df_long_valid %>%
  group_by(columna) %>%
  summarise(
    media_abs_tasa = mean(abs_tasa, na.rm = TRUE),
    max_abs_tasa   = max(abs_tasa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(media_abs_tasa))

# Top 40 columnas por desviación media
top_columnas <- resumen_columnas %>% slice(1:40)

# 7.4. Top celdas con mayor desviación absoluta
top_desvios <- df_long_valid %>%
  arrange(desc(abs_tasa)) %>%
  slice(1:100)  # cambia 100 si quieres otra cantidad

############################################################
# 7.5. Tasa de variación TOTAL por país (filas y columnas)
############################################################

# Por país en filas
res_pais_filas <- df_long %>%
  group_by(pais_fila) %>%
  summarise(
    suma_W = sum(WILIAM, na.rm = TRUE),
    suma_U = sum(UNIZAR, na.rm = TRUE),
    tv_total     = ifelse(suma_W == 0, NA_real_, (suma_U - suma_W) / suma_W),
    tv_total_pct = 100 * tv_total,
    media_tasa       = mean(tasa, na.rm = TRUE),
    media_abs_tasa   = mean(abs_tasa, na.rm = TRUE),
    n_celdas         = sum(!is.na(tasa)),
    .groups = "drop"
  ) %>%
  arrange(desc(tv_total))

# Por país en columnas
res_pais_columnas <- df_long %>%
  group_by(pais_columna) %>%
  summarise(
    suma_W = sum(WILIAM, na.rm = TRUE),
    suma_U = sum(UNIZAR, na.rm = TRUE),
    tv_total     = ifelse(suma_W == 0, NA_real_, (suma_U - suma_W) / suma_W),
    tv_total_pct = 100 * tv_total,
    media_tasa       = mean(tasa, na.rm = TRUE),
    media_abs_tasa   = mean(abs_tasa, na.rm = TRUE),
    n_celdas         = sum(!is.na(tasa)),
    .groups = "drop"
  ) %>%
  arrange(desc(tv_total))

############################################################
# 8. Gráficos (visual) usando todas las celdas con tasa definida
############################################################

# 8.1. Top 40 filas más discrepantes
top_filas <- resumen_filas %>% slice(1:40)

ggplot(top_filas, aes(x = reorder(fila, media_abs_tasa), y = media_abs_tasa)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 40 filas por desviación media |tasa|",
    x = "Fila (sector/país)",
    y = "Media |tasa|"
  )

# 8.2. Heatmap mejorado: top 40 filas x top 40 columnas
filas_sel <- top_filas$fila
cols_sel  <- top_columnas$columna

heat_df <- df_long_valid %>%
  filter(
    fila %in% filas_sel,
    columna %in% cols_sel
  )

# Recortamos la escala al percentil 1%–99% de la tasa en este sub-bloque
q_low  <- quantile(heat_df$tasa, 0.01, na.rm = TRUE)
q_high <- quantile(heat_df$tasa, 0.99, na.rm = TRUE)

heat_df <- heat_df %>%
  mutate(
    tasa_cap = pmax(pmin(tasa, q_high), q_low)  # tasa "capada" a ese rango
  )

ggplot(
  heat_df,
  aes(
    x    = columna,
    y    = fila,
    fill = tasa_cap
  )
) +
  geom_tile() +
  scale_fill_gradient2(
    midpoint = 0,
    low  = "blue",
    mid  = "white",
    high = "red"
  ) +
  labs(
    title = "Heatmap de tasa (top 40 filas x top 40 columnas)",
    x = "Columna",
    y = "Fila",
    fill = "tasa (capada\n1%-99%)"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 8)
  )

# 8.3. Dispersión UNIZAR vs WILIAM resaltando las celdas más alejadas
set.seed(123)
n_total    <- nrow(df_long_valid)
n_mostrar  <- min(50000, n_total)

muestra_df <- df_long_valid %>%
  sample_n(n_mostrar)

top_extremos <- top_desvios %>% slice(1:500)

ggplot() +
  geom_point(
    data = muestra_df,
    aes(x = WILIAM, y = UNIZAR),
    alpha = 0.1
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(
    data = top_extremos,
    aes(x = WILIAM, y = UNIZAR),
    color = "red",
    alpha = 0.7
  ) +
  labs(
    title = "Dispersión UNIZAR vs WILIAM\n(con puntos más alejados resaltados)",
    x = "WILIAM",
    y = "UNIZAR"
  )

############################################################
# 8.5. Dispersión log-log UNIZAR vs WILIAM
############################################################

df_scatter <- df_long_valid %>%
  filter(WILIAM > 0, UNIZAR > 0)

set.seed(123)
n_total   <- nrow(df_scatter)
n_mostrar <- min(50000, n_total)

muestra_df <- df_scatter %>% sample_n(n_mostrar)

top_extremos <- top_desvios %>%
  filter(WILIAM > 0, UNIZAR > 0)

ggplot() +
  geom_point(
    data = muestra_df,
    aes(x = WILIAM, y = UNIZAR),
    alpha = 0.15
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(
    data = top_extremos,
    aes(x = WILIAM, y = UNIZAR),
    alpha = 0.9
  ) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Dispersión log-log UNIZAR vs WILIAM\n(top desvíos resaltados)",
    x = "WILIAM (log10)",
    y = "UNIZAR (log10)"
  )

############################################################
# 8.5 bis. Dispersión log-log UNIZAR vs WILIAM (interactiva)
############################################################

df_scatter <- df_long_valid %>%
  filter(WILIAM > 0, UNIZAR > 0)

set.seed(123)
n_total   <- nrow(df_scatter)
n_mostrar <- min(20000, n_total)

muestra_df <- df_scatter %>% sample_n(n_mostrar)

top_extremos <- top_desvios %>%
  filter(WILIAM > 0, UNIZAR > 0)

min_xy <- min(muestra_df$WILIAM, muestra_df$UNIZAR, na.rm = TRUE)
max_xy <- max(muestra_df$WILIAM, muestra_df$UNIZAR, na.rm = TRUE)
diag_df <- data.frame(x = c(min_xy, max_xy), y = c(min_xy, max_xy))

p_loglog <- plot_ly() %>%
  add_markers(
    data = muestra_df,
    x = ~WILIAM, y = ~UNIZAR,
    text = ~paste0(
      "Fila: ", fila,
      "<br>Columna: ", columna,
      "<br>WILIAM: ", signif(WILIAM, 4),
      "<br>UNIZAR: ", signif(UNIZAR, 4),
      "<br>tasa: ", signif(tasa, 4),
      "<br>|tasa|: ", signif(abs_tasa, 4)
    ),
    hoverinfo = "text",
    opacity = 0.15,
    name = "Muestra"
  ) %>%
  add_markers(
    data = top_extremos,
    x = ~WILIAM, y = ~UNIZAR,
    text = ~paste0(
      "Fila: ", fila,
      "<br>Columna: ", columna,
      "<br>WILIAM: ", signif(WILIAM, 4),
      "<br>UNIZAR: ", signif(UNIZAR, 4),
      "<br>tasa: ", signif(tasa, 4),
      "<br>|tasa|: ", signif(abs_tasa, 4)
    ),
    hoverinfo = "text",
    opacity = 0.9,
    name = "Top desvíos"
  ) %>%
  add_lines(
    data = diag_df,
    x = ~x, y = ~y,
    inherit = FALSE,
    name = "y = x"
  ) %>%
  layout(
    title = "Dispersión log-log UNIZAR vs WILIAM",
    xaxis = list(title = "WILIAM (log10)", type = "log"),
    yaxis = list(title = "UNIZAR (log10)", type = "log")
  )

p_loglog

############################################################
# 8.6. Dispersión magnitud vs desviación relativa
############################################################

scatter_mag <- df_long_valid %>%
  mutate(
    magnitud = (WILIAM + UNIZAR) / 2
  )

set.seed(123)
n_total   <- nrow(scatter_mag)
n_mostrar <- min(50000, n_total)

muestra_mag <- scatter_mag %>% sample_n(n_mostrar)

top_extremos_mag <- top_desvios %>%
  mutate(magnitud = (WILIAM + UNIZAR) / 2)

ggplot() +
  geom_point(
    data = muestra_mag,
    aes(x = magnitud, y = abs_tasa),
    alpha = 0.1
  ) +
  geom_point(
    data = top_extremos_mag,
    aes(x = magnitud, y = abs_tasa),
    alpha = 0.9
  ) +
  scale_x_log10() +
  labs(
    title = "Magnitud del flujo vs |tasa de variación|",
    x = "Magnitud del flujo (media W+U, log10)",
    y = "|tasa|"
  )
############################################################
# 8.7. Dispersión por país (agregado, sin sectores)
############################################################

# Preparar datos de países para filas y columnas
res_pais_filas_plot <- res_pais_filas %>%
  transmute(
    pais   = pais_fila,
    suma_W,
    suma_U,
    tv_total,
    origen = "Filas"
  )

res_pais_columnas_plot <- res_pais_columnas %>%
  transmute(
    pais   = pais_columna,
    suma_W,
    suma_U,
    tv_total,
    origen = "Columnas"
  )

res_pais_both <- bind_rows(res_pais_filas_plot, res_pais_columnas_plot)

# Gráfico: suma WILIAM vs suma UNIZAR por país, coloreado por tasa total
ggplot(res_pais_both, aes(x = suma_W, y = suma_U, color = tv_total)) +
  geom_point(size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  facet_wrap(~ origen) +
  labs(
    title = "UNIZAR vs WILIAM agregados por país",
    x = "Suma WILIAM por país (log10)",
    y = "Suma UNIZAR por país (log10)",
    color = "Tasa total"
  )
############################################################
# 8.7. Dispersión por país (agregado, interactiva)
############################################################

# Preparar datos de países para filas y columnas
res_pais_filas_plot <- res_pais_filas %>%
  transmute(
    pais   = pais_fila,
    suma_W,
    suma_U,
    tv_total,
    tv_total_pct,
    origen = "Filas"
  )

res_pais_columnas_plot <- res_pais_columnas %>%
  transmute(
    pais   = pais_columna,
    suma_W,
    suma_U,
    tv_total,
    tv_total_pct,
    origen = "Columnas"
  )

res_pais_both <- bind_rows(res_pais_filas_plot, res_pais_columnas_plot)

# Datos para la diagonal y = x (en escala agregada)
min_xy_pais <- min(res_pais_both$suma_W, res_pais_both$suma_U, na.rm = TRUE)
max_xy_pais <- max(res_pais_both$suma_W, res_pais_both$suma_U, na.rm = TRUE)
diag_pais <- data.frame(x = c(min_xy_pais, max_xy_pais),
                        y = c(min_xy_pais, max_xy_pais))

p_paises <- plot_ly() %>%
  add_markers(
    data = res_pais_both,
    x = ~suma_W,
    y = ~suma_U,
    color = ~tv_total,        # color por tasa total
    colors = "RdBu",
    text = ~paste0(
      "País: ", pais,
      "<br>Origen: ", origen,
      "<br>Suma WILIAM: ", signif(suma_W, 4),
      "<br>Suma UNIZAR: ", signif(suma_U, 4),
      "<br>Tasa total: ", signif(tv_total, 4),
      "<br>Tasa total (%): ", signif(tv_total_pct, 4)
    ),
    hoverinfo = "text",
    marker = list(size = 9),
    name = "País"
  ) %>%
  add_lines(
    data = diag_pais,
    x = ~x, y = ~y,
    inherit = FALSE,
    line = list(dash = "dash"),
    name = "y = x"
  ) %>%
  layout(
    title = "UNIZAR vs WILIAM agregados por país",
    xaxis = list(title = "Suma WILIAM por país (log10)", type = "log"),
    yaxis = list(title = "Suma UNIZAR por país (log10)", type = "log"),
    coloraxis = list(colorbar = list(title = "Tasa total"))
  )

p_paises

############################################################
# 9. Guardar resultados numéricos en Excel
############################################################

wb <- createWorkbook()

# 9.1. Matriz completa de tasas (formato ancho, con IDs)
addWorksheet(wb, "Tasa_matriz")
writeData(wb, "Tasa_matriz", df_tasa_resultado, rowNames = TRUE)

# 9.2. Top celdas con mayor desviación
addWorksheet(wb, "Top_desvios")
writeData(wb, "Top_desvios", top_desvios)

# 9.3. Resumen global
addWorksheet(wb, "Resumen_global")
writeData(wb, "Resumen_global", resumen_global)

# 9.4. Resumen por filas
addWorksheet(wb, "Resumen_filas")
writeData(wb, "Resumen_filas", resumen_filas)

# 9.5. Resumen por columnas
addWorksheet(wb, "Resumen_columnas")
writeData(wb, "Resumen_columnas", resumen_columnas)

# 9.6. Ceros desiguales y ambos cero (diagnóstico)
addWorksheet(wb, "W0_Uneq0")
writeData(wb, "W0_Uneq0", df_ceros_W0_Uneq0)

addWorksheet(wb, "U0_Wneq0")
writeData(wb, "U0_Wneq0", df_ceros_U0_Wneq0)

addWorksheet(wb, "W0_U0_both")
writeData(wb, "W0_U0_both", df_ceros_both0)

# 9.7. Resultados por país (filas y columnas)
addWorksheet(wb, "Pais_filas")
writeData(wb, "Pais_filas", res_pais_filas)

addWorksheet(wb, "Pais_columnas")
writeData(wb, "Pais_columnas", res_pais_columnas)

saveWorkbook(wb, nombre_excel_salida, overwrite = TRUE)

############################################################
# Fin del script
############################################################

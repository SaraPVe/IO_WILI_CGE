#############################################
# 0. Librerías
#############################################

# install.packages("openxlsx")
# install.packages("tidyverse")

library(openxlsx)
library(dplyr)
library(tidyr)

#############################################
# 1. UNIZAR: A (coeficientes) + X  → IO_unizar
#############################################

# 1.1 Matriz de coeficientes A (antes la llamabas unizar_m)
#     Hoja 1 de data_Unizar.xlsx, sin nombres de columna
A_raw <- read.xlsx("data_Unizar.xlsx", sheet = 1, colNames = FALSE)
cat("Dimensiones de A_raw:", dim(A_raw), "\n")

# 1.2 Leer X_CGE como vector columna (sin cabecera)
X_CGE <- read.csv("X_CGE.csv", header = FALSE)
X_vec <- as.numeric(X_CGE[[1]])

cat("Longitud de X_vec:       ", length(X_vec), "\n")
cat("Número de columnas A_raw: ", ncol(A_raw),  "\n")

if (length(X_vec) != ncol(A_raw)) {
  stop("⚠️ length(X_vec) y ncol(A_raw) NO coinciden. Revisa el orden de X.")
}

# 1.3 Asegurarnos de que A_raw es numérica
A_mat <- as.matrix(A_raw)
storage.mode(A_mat) <- "numeric"

# 1.4 Construir la matriz IO en valores monetarios
#     IO_unizar_ij = a_ij * x_j  →  IO_unizar = A * diag(X)
IO_unizar <- sweep(A_mat, 2, X_vec, `*`)
cat("\nResumen de IO_unizar (valores monetarios):\n")
print(summary(as.vector(IO_unizar)))

# A partir de aquí usamos IO_unizar como tu antigua unizar_m
unizar_m <- IO_unizar

#############################################
# 2. Países UNIZAR y estructura país–sector
#############################################

# 2.1 Leer la lista de países desde el MISMO Excel (menos ficheros sueltos)
#     Ajusta la hoja si tus países están en otra (por ejemplo, sheet = 3)

countries_unizar <- read.xlsx("data_Unizar.xlsx", sheet = 3, colNames = FALSE)
# Lo convertimos en un vector plano de caracteres
countries_unizar <- as.character(unlist(countries_unizar))

cat("\nPaíses UNIZAR detectados (", length(countries_unizar), "):\n", 
    paste(countries_unizar, collapse = ", "), "\n", sep = "")

# 2.2 Número de sectores por país (ajusta si no son 48)
n_sect_uz <- 48L

# 2.3 Construir las columnas in_cou e in_sec_uz
#     in_cou: cada país repetido n_sect_uz veces
#     in_sec_uz: 1..n_sect_uz para cada país

names_unizar <- data.frame(
  in_cou    = rep(countries_unizar, each = n_sect_uz),
  in_sec_uz = rep(seq_len(n_sect_uz),  times = length(countries_unizar))
)

# Comprobación de dimensiones: país×sector debe coincidir con nº columnas
stopifnot(nrow(names_unizar) == ncol(unizar_m))

#############################################
# 3. Añadir identificadores y pivotar UNIZAR
#############################################

# 3.1 Asignar nombres de columna PAIS_SECTOR
colnames(unizar_m) <- paste0(names_unizar$in_cou, "_", names_unizar$in_sec_uz)

# 3.2 Añadir columnas identificadoras al principio
unizar_done <- cbind(names_unizar, unizar_m)

# 3.3 Pivotar UNIZAR a formato largo (manteniendo tu lógica)

unizar_pivot <- unizar_done %>%
  pivot_longer(
    cols      = -c(in_cou, in_sec_uz),   # solo las columnas de valores
    names_to  = "tmp",                  # nombre temporal
    values_to = "unizar_values"
  ) %>%
  separate(
    col    = tmp,
    into   = c("out_cou", "out_sec_uz"),
    sep    = "_",
    remove = TRUE
  ) %>%
  mutate(
    in_sec_uz  = as.character(in_sec_uz),
    out_sec_uz = as.character(out_sec_uz)
  )

#############################################
# 4. CARGAR Y PIVOTAR WILIAM
#############################################

# Aquí asumimos que ya tienes una matriz W_norm en memoria
# (por ejemplo, leída de un Excel grande). Solo toco la parte
# de pivotado y nombres de columnas clave.

# Ejemplo de lectura (ajusta ruta/hoja):
W_norm <- read.xlsx("W_normalizada.xlsx", sheet = 1)

wiliam_pivot <- W_norm %>%
  # La primera columna contiene país-segmento tipo "AUSTRIA-1", etc.
  rename(in_wil = 1) %>%
  separate(in_wil, into = c("in_cou", "in_sec_wi"), sep = "-") %>%
  # Pasar a formato largo, excluyendo las columnas de identificación
  pivot_longer(
    cols      = -c(in_cou, in_sec_wi),
    names_to  = "tmp",
    values_to = "wiliam_values"
  ) %>%
  # Separar país y sector de salida
  separate(
    tmp,
    into   = c("out_cou", "out_sec_wi"),
    sep    = "-",
    remove = TRUE
  ) %>%
  mutate(
    in_sec_wi  = as.character(in_sec_wi),
    out_sec_wi = as.character(out_sec_wi)
  ) #%>%
  # Renombrar para que las claves se llamen igual que en unizar_pivot
  #rename(
    #in_sec_uz  = in_sec_wi,
    #out_sec_uz = out_sec_wi
  #)

#############################################
# 5. Comprobaciones rápidas de consistencia países/sectores
#############################################

# Conjuntos de países de entrada
cat("\nComparando in_cou (entrada):\n")
cat("UNIZAR:", sort(unique(unizar_pivot$in_cou)), "\n")
cat("WILIAM:", sort(unique(wiliam_pivot$in_cou)), "\n")

# Conjuntos de países de salida
cat("\nComparando out_cou (salida):\n")
cat("UNIZAR:", sort(unique(unizar_pivot$out_cou)), "\n")
cat("WILIAM:", sort(unique(wiliam_pivot$out_cou)), "\n")

# (Opcional) Checks más estrictos:
stopifnot(
 identical(sort(unique(unizar_pivot$in_cou)),  sort(unique(wiliam_pivot$in_cou))),
 identical(sort(unique(unizar_pivot$out_cou)), sort(unique(wiliam_pivot$out_cou)))
)

cat("\n✅ Script unificado ejecutado (UNIZAR + WILIAM, menos Excels intermedios).\n")
#**MWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWM
# UNIRLAS TABLAS POR FIN ----------------------

# limpiar todo menos mis dos matrices pivotadas
rm(list = setdiff(ls(), c("wiliam_pivot", "unizar_pivot")))

# la tabla nexo
sector_join <- read.xlsx("./info/Correspondance_final.xlsx", sheet = "Rafa_intermediate_wili") |> 
  mutate_all(~as.character(.)) #importante que sea caracter

# check nombres de paises y sectores
a = distinct(wiliam_pivot |> select(in_cou))
b = distinct(unizar_pivot |> select(in_cou))
a == b

a = distinct(wiliam_pivot |> select(out_cou))
b = distinct(unizar_pivot |> select(out_cou))
a == b

a = distinct(wiliam_pivot |> select(in_sec_wi) |> arrange(in_sec_wi))
b = distinct(sector_join |> select(code_wi)|> arrange(code_wi))
a == b


# union de la tabla nexo con la de wiliam. Uno primero los setores IN y despues OUT
# importante le cambio el nombre  para que esté acorde
wiliam_join <- wiliam_pivot |>
  left_join(
    sector_join |> rename(in_sec_uz = code_za),
    by = c("in_sec_wi" = "code_wi")
  ) |>
  left_join(
    sector_join |> rename(out_sec_uz = code_za),
    by = c("out_sec_wi" = "code_wi")
  )
# ahora que tiene bien el nexo, procedo a attach la matriz
# con el full_join me aseguro de que "duplican" los valores de zar para que no queden huecos libres 
# en la matriz final ( que debe tener el tamaño de wili)
# para que se una tiene que cumplir que sean iguales: pais de entrada, salida, y los sectores de entrada y salida de unizar

  wiliam_unizar <- wiliam_join |> 
    full_join(unizar_pivot,
    by = c("in_cou", "out_cou", "in_sec_uz", "out_sec_uz")) 
  
  
  # save(wiliam_unizar,file = "./Data_CT/wiliam_unizar.RData")
  #load("./Data_CT/wiliam_unizar.RData")


# ahora calculo el producto (wiliam_values * unizar_values)
# limpio la matriz y la preparo para su pivote
# y la pivoto a lo ancho
wiliam_wide_c <- wiliam_unizar |> 
  mutate(values = wiliam_values * unizar_values) |> 
  unite(c("in_cou", "in_sec_wi"),  col = "insecou") |> 
  unite(c("out_cou", "out_sec_wi"), col = "outsecou") |> 
  select(-in_sec_uz, -out_sec_uz, -unizar_values, -wiliam_values) |> 
  pivot_wider(names_from = "outsecou", values_from = "values")


save(wiliam_wide_c, file = "./Data_CT/wiliam_wide_c.RData")
write.xlsx(wiliam_wide_c, "IO_wiliam.xlsx")

# === Helpers (mismos que usas en tus scripts) ===
clean_names <- function(x){
  x <- trimws(x)
  x <- gsub("\\.-\\.","-",x)  # Corrige la expresión regular
  x <- gsub("\\s*-\\s*","-",x)  # Elimina los espacios antes/después de los guiones
  x <- gsub("[–—−]", "-", x)  # Reemplaza otros tipos de guiones por "-"
  toupper(x)  # Convierte todo a mayúsculas
}

country_of  <- function(x) sub("-\\d+$", "", x)
extract_id  <- function(x) as.integer(sub(".*-(\\d+)$", "\\1", x))
canon_country <- function(x){ x <- clean_names(x); gsub("[^A-Z0-9]", "", x) }

# === Carga matrices ===
W0 <- as.matrix(read.xlsx("W.xlsx", sheet = 1, colNames = TRUE, rowNames = TRUE))
W1 <- as.matrix(read.xlsx("W_WILI_CGE.xlsx", sheet = 1, colNames = TRUE, rowNames = TRUE))

# === Cargar M_unizar desde la tercera hoja ===
M_unizar <- read.xlsx("data_unizar.xlsx", sheet = 3)  # Leer solo la tercera hoja
countries_unizar <- M_unizar[[1]]  # Los países están en la primera columna
sectors_unizar <- 1:48  # Los sectores son del 1 al 48
M_unizar <- as.matrix(M_unizar[, -1])  # Eliminar la primera columna (de países) y mantener los datos de la matriz

# Limpiar los nombres de países
countries_unizar <- clean_names(countries_unizar)

# === Índices ===
rn <- clean_names(rownames(W0)); cn <- clean_names(colnames(W0))
stopifnot(identical(rn, clean_names(rownames(W1))), identical(cn, clean_names(colnames(W1))))
ct_r <- canon_country(country_of(rn)); ct_c <- canon_country(country_of(cn))
ids_r <- extract_id(rn); ids_c <- extract_id(cn)
countries <- unique(ct_r)
idxWr_by <- setNames(lapply(countries, function(cc) which(ct_r == cc)), countries)
idxWc_by <- setNames(lapply(countries, function(cc) which(ct_c == cc)), countries)

# === Conjuntos y parámetros ===
B_base <- c(1:8, 18:46, 52:62)  # Sectores normales
G5  <- 9:17                      # Grupo 5 (especial)
G39 <- 47:49                      # Grupo 39 (especial)
G37 <- 50:51                      # Grupo 37 (especial)
groups <- list(G5=G5, G39=G39, G37=G37)
eps <- 1e-8                       # Umbral para la normalización
tol <- 1e-12                       # Tolerancia para las comparaciones

# === Función para desagregar y aplicar las reglas de normalización ===
normalize_W_res <- function(W0, W_norm, M_unizar) {
  n_ctry <- length(countries)
  RES_global <- matrix(0, nrow=62 * n_ctry, ncol=62 * n_ctry)  # Iniciar la matriz final
  
  for (k in 1:n_ctry) {
    for (l in 1:n_ctry) {
      for (r in B_base) {  # Sectores normales, se dejan igual
        for (c in B_base) {
          RES_global[r, c] <- M_unizar[r, c] * W_norm[r, c]  # Multiplicamos por W_norm
        }
      }
    }
  }
  
  # === Desagregación para sectores especiales ===
  for (k in 1:n_ctry) {
    for (l in 1:n_ctry) {
      for (r in groups$G5) {  # Si la fila pertenece al grupo G5
        for (c in B_base) {  # Si la columna es normal
          if (r == 5) {
            RES_global[r, c] <- M_unizar[r, c] * W_norm[9:17, 9:17]  # Multiplicar por la fracción de W_norm
          }
        }
      }
      
      # Casos similares para los demás grupos G39 y G37
      for (r in groups$G39) {  # Si la fila pertenece al grupo G39
        for (c in B_base) {
          if (r == 39) {
            RES_global[r, c] <- M_unizar[r, c] * W_norm[47:49, 47:49]
          }
        }
      }
      
      for (r in groups$G37) {  # Si la fila pertenece al grupo G37
        for (c in B_base) {
          if (r == 37) {
            RES_global[r, c] <- M_unizar[r, c] * W_norm[50:51, 50:51]
          }
        }
      }
    }
  }
  
  # === Intersecciones entre sectores especiales (5 con 8, etc.) ===
  for (k in 1:n_ctry) {
    for (l in 1:n_ctry) {
      for (r in groups$G5) {  # Si la fila pertenece al grupo G5
        for (c in groups$G5) {  # Si la columna pertenece al grupo G5
          if (r != c) {
            RES_global[r, c] <- (W_norm[r, c] + W_norm[c, r]) / length(groups$G5) * M_unizar[r, c]
          }
        }
      }
      # Se puede repetir el mismo proceso para los otros grupos (G39 y G37)
    }
  }
  
  # === Devuelve la matriz final ===
  return(RES_global)
}

# === Ejecuta la normalización ===
W_norm_res <- normalize_W_res(W0, W1, M_unizar)

# === Comprobación de ceros y fracciones ===
cat("Ceros en la matriz original (W): ", sum(W0 == 0), "\n")
cat("Ceros en la matriz normalizada (W_norm): ", sum(W_norm_res == 0), "\n")

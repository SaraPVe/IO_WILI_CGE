# === Helpers para la normalización de la matriz W ===

# Función para limpiar nombres
clean_names <- function(x){
  x <- trimws(x)
  x <- gsub("\\.-\\.", "-", x)  # Corrige las expresiones regulares
  x <- gsub("\\s*-\\s*", "-", x)  # Elimina los espacios antes/después de los guiones
  x <- gsub("[–—−]", "-", x)  # Reemplaza otros tipos de guiones por "-"
  toupper(x)  # Convierte todo a mayúsculas
}

# Funciones para extraer la información de países y sectores
country_of  <- function(x) sub("-\\d+$", "", x)
extract_id  <- function(x) as.integer(sub(".*-(\\d+)$", "\\1", x))
canon_country <- function(x){ x <- clean_names(x); gsub("[^A-Z0-9]", "", x) }

# Grupos definidos (según canvas)
G5  <- 9:17
G39 <- 47:49
G37 <- 50:51
G6_21 <- c(6, 21)  # Subgrupo especial
B_base <- c(1:8, 18:46, 52:62)  # Sectores normales (de acuerdo a tu definición)
groups <- list(G5 = G5, G39 = G39, G37 = G37, G6_21 = G6_21)

# Carga matrices (W0 original y W1 normalizada)
W0 <- as.matrix(read.xlsx("W.xlsx", sheet = 1, colNames = TRUE, rowNames = TRUE))
W1 <- as.matrix(read.xlsx("W_WILI_CGE.xlsx", sheet = 1, colNames = TRUE, rowNames = TRUE))

# Índices de países y sectores
rn <- clean_names(rownames(W0)); cn <- clean_names(colnames(W0))
ct_r <- canon_country(country_of(rn)); ct_c <- canon_country(country_of(cn))
ids_r <- extract_id(rn); ids_c <- extract_id(cn)
countries <- unique(ct_r)
idxWr_by <- setNames(lapply(countries, function(cc) which(ct_r == cc)), countries)
idxWc_by <- setNames(lapply(countries, function(cc) which(ct_c == cc)), countries)

# Tolerancias para los errores
eps <- 1e-8
tol <- 1e-12

# Comprobación por celdas de la matriz
check_normalized_matrix <- function(W_normalized, W_original, groups, eps = 1e-8) {
  
  n_ctry <- length(unique(extract_id(rownames(W_normalized))))
  result <- data.frame(country_r = character(), country_c = character(), row_id = integer(), col_id = integer(), result = character())
  
  for (k in 1:n_ctry) {
    for (l in 1:n_ctry) {
      # Extrae los índices de fila y columna por país
      iR <- idxWr_by[[k]]
      iC <- idxWc_by[[l]]
      Wkl_normalized <- W_normalized[iR, iC, drop = FALSE]
      Wkl_original <- W_original[iR, iC, drop = FALSE]
      
      # Recorre todas las celdas de la submatriz correspondiente
      for (r in 1:length(iR)) {
        for (c in 1:length(iC)) {
          r_id <- extract_id(rownames(W_normalized)[iR[r]])
          c_id <- extract_id(colnames(W_normalized)[iC[c]])
          
          # Verifica si las filas y las columnas son normales o pertenecen a un subgrupo especial
          g_row <- names(Filter(function(g) r_id %in% g, groups))
          g_col <- names(Filter(function(g) c_id %in% g, groups))
          
          # Caso 1: Si la fila y la columna pertenecen a un subgrupo especial, se usa la matriz cuadrada
          if (length(g_row) && length(g_col) && g_row == g_col) {
            Sg <- sum(Wkl_original[which(ids_r %in% groups[[g_row]]), 
                                   which(ids_c %in% groups[[g_col]])])
            if (abs(Sg) > eps) {
              expected_value <- Wkl_original[r, c] / Sg
              if (abs(expected_value - 1) > eps) {
                result <- rbind(result, data.frame(country_r = rownames(W_normalized)[iR[r]], 
                                                   country_c = colnames(W_normalized)[iC[c]], 
                                                   row_id = r_id, col_id = c_id, 
                                                   result = sprintf("Matriz cuadrada no normalizada correctamente: Expected %.12f, Got %.12f", 1, expected_value)))
              }
            }
          }
          
          # Caso 2: Si la fila es normal y la columna es especial, se debe normalizar verticalmente
          if (!length(g_row) && length(g_col)) {
            g <- groups[[g_col]]
            RR <- which(ids_r %in% g)
            Sc <- sum(Wkl_original[RR, c], na.rm = TRUE)
            if (abs(Sc) > eps) {
              expected_value <- Wkl_original[r, c] / Sc
              if (abs(expected_value - 1) > eps) {
                result <- rbind(result, data.frame(country_r = rownames(W_normalized)[iR[r]], 
                                                   country_c = colnames(W_normalized)[iC[c]], 
                                                   row_id = r_id, col_id = c_id, 
                                                   result = sprintf("Normalización vertical incorrecta: Expected %.12f, Got %.12f", 1, expected_value)))
              }
            }
          }
          
          # Caso 3: Si la columna es normal y la fila es especial, se debe normalizar horizontalmente
          if (length(g_row) && !length(g_col)) {
            g <- groups[[g_row]]
            CC <- which(ids_c %in% g)
            Sr <- sum(Wkl_original[r, CC], na.rm = TRUE)
            if (abs(Sr) > eps) {
              expected_value <- Wkl_original[r, c] / Sr
              if (abs(expected_value - 1) > eps) {
                result <- rbind(result, data.frame(country_r = rownames(W_normalized)[iR[r]], 
                                                   country_c = colnames(W_normalized)[iC[c]], 
                                                   row_id = r_id, col_id = c_id, 
                                                   result = sprintf("Normalización horizontal incorrecta: Expected %.12f, Got %.12f", 1, expected_value)))
              }
            }
          }
        }
      }
    }
  }
  
  if (nrow(result) == 0) {
    cat("Todos los cheques pasados correctamente. La normalización está correcta.\n")
  } else {
    cat("Errores encontrados en la normalización. Revisa los resultados.\n")
  }
  
  return(result)
}

# Ejecutamos la comprobación con la matriz normalizada
result_check <- check_normalized_matrix(W1, W0, groups)

# Mostrar los resultados
if (nrow(result_check) > 0) {
  print(result_check)
} else {
  cat("Todo parece correcto en la normalización.\n")
}

# Función para convertir NA en cero
convert_NA_to_zero <- function(matrix) {
  matrix[is.na(matrix)] <- 0
  return(matrix)
}

# Convertir los NA en cero en las matrices original y normalizada
W0_clean <- convert_NA_to_zero(W0)
W1_clean <- convert_NA_to_zero(W1)

# Comparar los ceros en la matriz original y en la normalizada
zeros_original <- sum(W0_clean == 0, na.rm = TRUE)  # Número de ceros en la matriz original
zeros_normalized <- sum(W1_clean == 0, na.rm = TRUE)  # Número de ceros en la matriz normalizada

cat("Número de ceros en la matriz original (W0):", zeros_original, "\n")
cat("Número de ceros en la matriz normalizada (W1):", zeros_normalized, "\n")

# Comparar las ubicaciones de los ceros en ambas matrices
matching_zeros <- sum(W0_clean == 0 & W1_clean == 0, na.rm = TRUE)  # Ceros comunes en ambas matrices
cat("Ceros que coinciden en ambas matrices:", matching_zeros, "\n")

# Opcional: Mostrar las posiciones donde las matrices tienen ceros diferentes
different_zeros <- which(W0_clean == 0 & W1_clean != 0 | W0_clean != 0 & W1_clean == 0, arr.ind = TRUE)
if (length(different_zeros) > 0) {
  cat("Posiciones con ceros diferentes entre las matrices:\n")
  print(different_zeros)
} else {
  cat("No hay diferencias en los ceros entre las matrices.\n")
}

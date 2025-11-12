# ============================================================
# NORMALIZACIÓN DE LA MATRIZ W (WILI_CGE) POR PAÍS Y REGLAS
#  - Lee W.xlsx (bloque global 62*n × 62*n con nombres tipo PAIS-<id>)
#  - Aplica las 5 reglas de normalización para TODOS los pares (k,l)
#    de países: consigo mismos y entre países distintos
#  - Grupos especiales: {6,21}, 9–17, 47–49, 50–51, 58–62
#  - Sectores normales (fuera de grupos): celda != 0 → 1 ; 0 → 0
#  - Especial×Especial (mismo o distinto): frecuencia relativa en la submatriz
#  - Normal×Especial: vector horizontal (fila normal / columnas del grupo)
#  - Especial×Normal: vector vertical   (filas del grupo / columna normal)
#  - Manejo seguro de denominadores (si suma==0, se deja 0)
# ============================================================

suppressPackageStartupMessages({library(openxlsx)})

# ---------------- Helpers de nombres e ids ----------------
clean_names <- function(x) {
  x <- trimws(x)
  x <- gsub("\\.-\\.", "-", x)  # Corrige la expresión regular
  x <- gsub("\\s*-\\s*", "-", x)  # Elimina los espacios antes/después de los guiones
  x <- gsub("[–—−]", "-", x)  # Reemplaza otros tipos de guiones por "-"
  toupper(x)  # Convierte todo a mayúsculas
}

country_of  <- function(x) sub("-\\d+$", "", x)
extract_id  <- function(x) as.integer(sub(".*-(\\d+)$", "\\1", x))
canon_country <- function(x){ x <- clean_names(x); gsub("[^A-Z0-9]", "", x) }

# ---------------- Lectura robusta de W.xlsx ----------------
read_W_matrix <- function(path = "W.xlsx", sheet = 1){
  df <- openxlsx::read.xlsx(path, sheet = sheet, colNames = TRUE)
  # Si la 1ª columna son nombres de fila (tipo "PAIS-<id>")
  first_col <- df[[1]]
  if (is.character(first_col)) {
    rn <- clean_names(first_col)
    mat <- as.matrix(df[, -1, drop = FALSE])
    storage.mode(mat) <- "double"
    colnames(mat) <- clean_names(colnames(df)[-1])
    rownames(mat) <- rn
  } else {
    # Intento directo (cabeceras ya traen colnames; sin rownames explícitos → error)
    mat <- as.matrix(df)
    storage.mode(mat) <- "double"
    if (is.null(rownames(mat))) stop("El Excel debe tener la 1ª columna con los nombres de fila tipo 'PAIS-<id>'.")
    rownames(mat) <- clean_names(rownames(mat))
    colnames(mat) <- clean_names(colnames(mat))
  }
  mat
}

# ---------------- Definición de grupos especiales ----------------
# 5 subgrupos (disjuntos):
#  - G6_21  : {6,21}
#  - G9_17  : 9:17
#  - G47_49 : 47:49
#  - G50_51 : 50:51
#  - G58_62 : 58:62
SPECIAL_GROUPS <- list(
  G6_21  = c(6, 21),
  G9_17  = 9:17,
  G47_49 = 47:49,
  G50_51 = 50:51,
  G58_62 = 58:62
)
ALL_SPECIAL_IDS <- sort(unique(unlist(SPECIAL_GROUPS)))

# ---------------- Normalización por bloques país–país ----------------
normalize_W_global <- function(W){
  # Limpia nombres e identifica países e ids
  rn <- clean_names(rownames(W)); cn <- clean_names(colnames(W))
  if (anyNA(rn) || anyNA(cn)) stop("W debe tener nombres de fila/columna tipo 'PAIS-<id>'.")
  id_r <- extract_id(rn); id_c <- extract_id(cn)
  if (anyNA(id_r) || anyNA(id_c)) stop("No se pudieron extraer ids <1..62> de 'PAIS-<id>'.")
  ct_r <- canon_country(country_of(rn)); ct_c <- canon_country(country_of(cn))
  
  # Conjuntos de países (orden según aparecen en filas)
  countries <- unique(ct_r)
  if (!setequal(unique(ct_r), unique(ct_c))) {
    stop("Los países en filas y columnas de W no coinciden tras canonizar nombres.")
  }
  
  # Índices por país
  idxWr_by <- setNames(lapply(countries, function(cc) which(ct_r == cc)), countries)
  idxWc_by <- setNames(lapply(countries, function(cc) which(ct_c == cc)), countries)
  
  # Chequeo rápido: cada bloque debe contener ids 1..62 (en cualquier orden)
  for (cc in countries){
    iR <- idxWr_by[[cc]]; iC <- idxWc_by[[cc]]
    if (!setequal(sort(id_r[iR]), 1:62)) stop("Bloque filas país ", cc, " no contiene todos los ids 1..62.")
    if (!setequal(sort(id_c[iC]), 1:62)) stop("Bloque columnas país ", cc, " no contiene todos los ids 1..62.")
  }
  
  # Resultado
  W_norm <- W
  
  # Bucle por todos los pares (k,l)
  for (k in seq_along(countries)){
    iR <- idxWr_by[[k]]; idsR <- id_r[iR]
    for (l in seq_along(countries)){
      iC <- idxWc_by[[l]]; idsC <- id_c[iC]
      
      Wkl <- W[iR, iC, drop = FALSE]
      Wkl_new <- Wkl  # trabajamos sobre copia del bloque
      
      # ---- 1) Especial × Especial (misma o distinta familia): normaliza en la submatriz ----
      for (gR_name in names(SPECIAL_GROUPS)){
        RR <- which(idsR %in% SPECIAL_GROUPS[[gR_name]])
        if (!length(RR)) next
        for (gC_name in names(SPECIAL_GROUPS)){
          CC <- which(idsC %in% SPECIAL_GROUPS[[gC_name]])
          if (!length(CC)) next
          sub <- Wkl[RR, CC, drop = FALSE]
          s <- sum(sub)
          if (is.finite(s) && s > 0) {
            Wkl_new[RR, CC] <- sub / s
          } else {
            Wkl_new[RR, CC] <- 0
          }
        }
      }
      
      # ---- 2) Normal × Especial: vector horizontal (fila normal sobre columnas del grupo) ----
      RR_norm <- which(!idsR %in% ALL_SPECIAL_IDS)
      if (length(RR_norm)){
        for (gC_name in names(SPECIAL_GROUPS)){
          CC <- which(idsC %in% SPECIAL_GROUPS[[gC_name]])
          if (!length(CC)) next
          for (rr in RR_norm){
            s <- sum(Wkl[rr, CC])
            if (is.finite(s) && s > 0){
              Wkl_new[rr, CC] <- Wkl[rr, CC] / s
            } else {
              Wkl_new[rr, CC] <- 0
            }
          }
        }
      }
      
      # ---- 3) Especial × Normal: vector vertical (filas del grupo sobre columna normal) ----
      CC_norm <- which(!idsC %in% ALL_SPECIAL_IDS)
      if (length(CC_norm)){
        for (gR_name in names(SPECIAL_GROUPS)){
          RR <- which(idsR %in% SPECIAL_GROUPS[[gR_name]])
          if (!length(RR)) next
          for (cc in CC_norm){
            s <- sum(Wkl[RR, cc])
            if (is.finite(s) && s > 0){
              Wkl_new[RR, cc] <- Wkl[RR, cc] / s
            } else {
              Wkl_new[RR, cc] <- 0
            }
          }
        }
      }
      
      # ---- 4) Normal × Normal: celda !=0 → 1 ; 0 → 0 ----
      RR_base <- which(!idsR %in% ALL_SPECIAL_IDS)
      CC_base <- which(!idsC %in% ALL_SPECIAL_IDS)
      if (length(RR_base) && length(CC_base)){
        base_block <- Wkl[RR_base, CC_base, drop = FALSE]
        Wkl_new[RR_base, CC_base] <- ifelse(base_block != 0, 1, 0)
      }
      
      # Vuelca el bloque normalizado
      W_norm[iR, iC] <- Wkl_new
    }
  }
  
  W_norm
}

# ---------------- Script principal ----------------
# 1) Lee W.xlsx
W <- read_W_matrix("W.xlsx")

# 2) Normaliza por reglas para TODOS los pares de países
W_norm <- normalize_W_global(W)

# 3) Exporta resultado
openxlsx::write.xlsx(as.data.frame(W_norm), file = "W_normalizada.xlsx",
                     rowNames = TRUE, overwrite = TRUE)


# ============================================================
# 01_transform_W.R
# Transforma W.xlsx aplicando las normas WILI_CGE (simétricas y sin solapes)
# (A) r,c ∈ G (mismo grupo): W[r,c] <- W[r,c] / sum(W[G,G])            [2D]
# (B) r ∉ G, c ∈ G:         W[r,c] <- W[r,c] / sum_rows_in_G_col_orig  [col]
# (C) r ∈ G, c ∉ G:         W[r,c] <- W[r,c] / sum_cols_in_G_row_orig  [fila]
# (D) c ∈ Base:             W[r,c] <- 1 si W[r,c]!=0; 0 si W[r,c]==0   [celda]
# ============================================================

suppressPackageStartupMessages({ library(openxlsx) })

# ---------- Helpers ----------
clean_names <- function(x){
  x <- trimws(x)
  x <- gsub("\\.-\\.", "-", x)
  x <- gsub("\\s*-\\s*", "-", x)
  x <- gsub("[–—−]", "-", x)
  toupper(x)
}
country_of  <- function(x) sub("-\\d+$","", x)
extract_id  <- function(x) as.integer(sub(".*-(\\d+)$","\\1", x))
canon_country <- function(x){ x <- clean_names(x); gsub("[^A-Z0-9]","", x) }

# ---------- Parámetros ----------
in_path  <- "W.xlsx"
sheet    <- 1
out_path <- "W_WILI_CGE.xlsx"

# ---------- Lectura ----------
W_df <- read.xlsx(in_path, sheet = sheet, colNames = TRUE, rowNames = TRUE)
W    <- as.matrix(W_df)
stopifnot(is.matrix(W), nrow(W)==ncol(W))

# ---------- Indexación por país/sector ----------
rn <- clean_names(rownames(W)); cn <- clean_names(colnames(W))
stopifnot(!anyDuplicated(rn), !anyDuplicated(cn), identical(sort(rn), sort(cn)))

ct_r <- canon_country(country_of(rn)); ct_c <- canon_country(country_of(cn))
ids_r <- extract_id(rn); ids_c <- extract_id(cn)
stopifnot(!anyNA(ids_r), !anyNA(ids_c))
countries <- unique(ct_r)
stopifnot(identical(sort(unique(ct_r)), sort(unique(ct_c))))

idxWr_by <- setNames(lapply(countries, function(cc) which(ct_r==cc)), countries)
idxWc_by <- setNames(lapply(countries, function(cc) which(ct_c==cc)), countries)

# Sanidad: cada bloque debe ser ids 1..62
for (cc in countries){
  idsR <- ids_r[idxWr_by[[cc]]]; idsC <- ids_c[idxWc_by[[cc]]]
  stopifnot(setequal(idsR, 1:62), setequal(idsC, 1:62))
}

# ---------- Conjuntos ----------
B_base <- c(1:8, 18:46, 52:62)
G5     <- 9:17
G39    <- 47:49
G37    <- 50:51
groups <- list(G5=G5, G39=G39, G37=G37)

# ---------- Transformación ----------
W_out <- W

for (k in seq_along(countries)) {
  iR <- idxWr_by[[k]]; idsR <- ids_r[iR]
  for (l in seq_along(countries)) {
    iC <- idxWc_by[[l]]; idsC <- ids_c[iC]
    
    # Bloque (k,l)
    Wkl_out <- W_out[iR, iC, drop=FALSE]  # sobre este escribimos
    Wkl0    <- W    [iR, iC, drop=FALSE]  # snapshot ORIGINAL (denominadores)
    
    ## (A) Normalización 2D en cada G×G
    for (g in groups) {
      RR <- which(idsR %in% g)  # filas del grupo
      CC <- which(idsC %in% g)  # columnas del grupo
      if (length(RR) && length(CC)) {
        Sg <- sum(Wkl0[RR, CC, drop=FALSE])
        if (is.finite(Sg) && Sg > 0) {
          Wkl_out[RR, CC] <- Wkl0[RR, CC] / Sg
        }
      }
    }
    
    ## (B) Columnas de grupo con filas fuera: r ∉ G, c ∈ G  (normalización vertical)
    for (g in groups) {
      RR <- which(idsR %in% g)         # filas del grupo
      CC <- which(idsC %in% g)         # columnas del grupo
      if (!length(CC) || !length(RR)) next
      R_out <- setdiff(seq_along(idsR), RR)  # filas fuera del grupo
      if (!length(R_out)) next
      for (pc in CC) {
        S_c <- sum(Wkl0[RR, pc])  # suma de la columna en filas del grupo (ORIGINAL)
        if (is.finite(S_c) && S_c > 0) {
          # SOLO filas fuera del grupo
          Wkl_out[R_out, pc] <- Wkl0[R_out, pc] / S_c
        }
      }
    }
    
    ## (C) Filas de grupo con columnas fuera: r ∈ G, c ∉ G  (normalización horizontal)
    for (g in groups) {
      RR <- which(idsR %in% g)
      CC <- which(idsC %in% g)
      if (!length(CC) || !length(RR)) next
      C_out <- setdiff(seq_along(idsC), CC)  # columnas fuera del grupo
      if (!length(C_out)) next
      for (pr in RR) {
        S_r <- sum(Wkl0[pr, CC])  # suma de la fila en columnas del grupo (ORIGINAL)
        if (is.finite(S_r) && S_r > 0) {
          # SOLO columnas fuera del grupo
          Wkl_out[pr, C_out] <- Wkl0[pr, C_out] / S_r
        }
      }
    }
    
    ## (D) Columnas base: celda ≠ 0 -> 1 ; 0 -> 0  (usa el valor ORIGINAL para decidir)
    base_cols <- which(idsC %in% B_base)
    if (length(base_cols)) {
      for (pc in base_cols) {
        nz <- Wkl0[, pc] != 0
        Wkl_out[nz, pc] <- 1
        Wkl_out[!nz, pc] <- 0
      }
    }
    
    # Escribe el bloque de vuelta
    W_out[iR, iC] <- Wkl_out
  }
}

# ---------- Guardar ----------
write.xlsx(as.data.frame(W_out), out_path, rowNames = TRUE, overwrite = TRUE)
cat("OK: escrito '", out_path, "'.\n", sep="")


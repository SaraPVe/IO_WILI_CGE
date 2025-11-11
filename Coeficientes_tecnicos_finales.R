# ============================================================
# Coeficientes técnicos finales (62x62) para TODOS los pares país→país
# Reglas:
# 1) Base (no especiales): RES[r',c'] += A[r,c] * W[r',c']   con remapeo 7→5 y 34→43
# 2) Especiales p ∈ {5,8,37,39,45}:
#    3.1 Columna p & fila NO especial:    RES[r', kids_p]      += A[r,p] * W[r', kids_p]
#    3.2 Diagonal (p,p):                  RES[kids_p, kids_p]  += A[p,p] * W[kids_p, kids_p]
#    3.3 Fila p & columna NO especial:    RES[kids_p, c']      += A[p,c] * W[kids_p, c']
# 3) Cruces especiales distintos (q ≠ p):
#    RES[kids_q, kids_p] += A[q,p] * ( W[kids_q,kids_p] + t(W[kids_p,kids_q]) ) / (|kids_q|+|kids_p|)
# Nota clave: Siempre "mover" (remap/expandir) → luego multiplicar → y al final insertar en orden físico.
# ============================================================

suppressPackageStartupMessages({
  library(openxlsx)
})

# ---------- Helpers ----------
clean_names <- function(x){
  x <- trimws(x)
  x <- gsub("\\.-\\.", "-", x)
  x <- gsub("\\s*-\\s*", "-", x)
  x <- gsub("[–—−]", "-", x)
  toupper(x)
}
canon_country <- function(x){
  x <- clean_names(x)
  gsub("[^A-Z0-9]", "", x)  # CZECH_REPUBLIC == CZECHREPUBLIC
}
country_of  <- function(x) sub("-\\d+$", "", x)
extract_id  <- function(x) as.integer(sub(".*-(\\d+)$", "\\1", x))

# Remapeos simples (7 -> 5 ; 34 -> 43)
map_simple_child <- function(idx){
  idx <- ifelse(idx == 7, 5, idx)
  idx <- ifelse(idx == 34, 43, idx)
  idx
}

# Conjuntos especiales (expansiones)
expand_sets <- list(
  `5`  = 9:17,
  `8`  = c(6,21),
  `37` = 50:51,
  `39` = 47:49,
  `45` = 58:62
)
expand_keys <- as.integer(names(expand_sets))

# ---------- Lectura Excel ----------
unizar_path <- "data_UNIZAR.xlsx"

# Sheet 3: países en 1ª columna, sin encabezados (orden de bloques en Sheet1)
s3 <- read.xlsx(unizar_path, sheet = 3, colNames = FALSE)
if (is.null(s3) || ncol(s3) < 1) stop("Sheet3 vacío o sin columna 1.")
country_order <- clean_names(trimws(as.character(s3[[1]])))
country_order <- country_order[nzchar(country_order)]

# Sheet 1: matriz padre A (48*n_ctry × 48*n_ctry), sin nombres
M <- read.xlsx(unizar_path, sheet = 1, colNames = FALSE, rowNames = FALSE)
M <- as.matrix(M)
if (nrow(M) != ncol(M) || (nrow(M) %% 48) != 0) {
  stop(sprintf("Sheet1 debe ser cuadrada y múltiplo de 48. Recibido %dx%d.", nrow(M), ncol(M)))
}
n_sec  <- 48
n_ctry <- nrow(M) / n_sec
if (length(country_order) < n_ctry) {
  stop(sprintf("Sheet3 aporta %d países; se esperaban %d.", length(country_order), n_ctry))
}
country_order <- country_order[seq_len(n_ctry)]
sector_ids <- 1:48

# ---------- W (62 por país, TODOS los pares) ----------
if (!exists("W")) stop("No se encuentra la matriz W en memoria (debe existir antes de ejecutar).")
stopifnot(is.matrix(W), nrow(W) == ncol(W))

rownames(W) <- clean_names(rownames(W))
colnames(W) <- clean_names(colnames(W))

codes62 <- rownames(W)
if (is.null(codes62) || anyNA(codes62)) stop("W debe tener rownames con patrón 'PAIS-<id>'.")
ctry62_raw <- country_of(codes62)
ids62      <- extract_id(codes62)
if (anyNA(ids62)) stop("No se pudieron extraer ids de sector 1..62 de los nombres de W.")

# Alinear países de W al orden del Excel (con canonización para CZECH_REPUBLIC)
ctry62_canon        <- canon_country(ctry62_raw)
country_order_canon <- canon_country(country_order)

if (!setequal(unique(ctry62_canon), unique(country_order_canon))) {
  faltan <- setdiff(unique(country_order_canon), unique(ctry62_canon))
  sobran <- setdiff(unique(ctry62_canon), unique(country_order_canon))
  faltan_orig <- country_order[match(faltan, country_order_canon)]
  sobran_orig <- unique(ctry62_raw)[match(sobran, unique(ctry62_canon))]
  stop("Los países en W no coinciden con los del Excel (tras canonizar). ",
       if (length(faltan)) paste0("Faltan en W: ", paste(faltan_orig, collapse=", "), ". ") else "",
       if (length(sobran)) paste0("Sobran en W: ", paste(sobran_orig, collapse=", "), ". ") else "")
}

row_order <- unlist(lapply(seq_along(country_order), function(i){
  canon_i <- country_order_canon[i]
  which(ctry62_canon == canon_i)
}))
col_order <- row_order
W <- W[row_order, col_order, drop = FALSE]

# ---------- Mapas de posiciones (por id dentro de cada bloque 62×62) ----------
idxA_block <- function(i) ((i-1)*n_sec + 1):(i*n_sec)   # 48×48
idxW_block <- function(i) ((i-1)*62 + 1):(i*62)        # 62×62

ids62_rows <- extract_id(rownames(W))
ids62_cols <- extract_id(colnames(W))

pos_by_id_row <- vector("list", n_ctry)
pos_by_id_col <- vector("list", n_ctry)

for (k in seq_len(n_ctry)) {
  # filas
  ids_r <- ids62_rows[idxW_block(k)]
  pos_r <- match(1:62, ids_r)     # para id=1..62 → posición física (1..62) en filas
  if (anyNA(pos_r)) stop(sprintf("Bloque FILAS país %d no contiene todos los ids 1..62.", k))
  pos_by_id_row[[k]] <- pos_r
  # columnas
  ids_c <- ids62_cols[idxW_block(k)]
  pos_c <- match(1:62, ids_c)     # para id=1..62 → posición física (1..62) en columnas
  if (anyNA(pos_c)) stop(sprintf("Bloque COLUMNAS país %d no contiene todos los ids 1..62.", k))
  pos_by_id_col[[k]] <- pos_c
}

# ---------- Construcción de RES_global ----------
RES_global <- matrix(0, nrow = 62*n_ctry, ncol = 62*n_ctry)

for (k in seq_len(n_ctry)) {                 # país en FILAS
  A_rows <- idxA_block(k)
  W_rows <- idxW_block(k)
  for (l in seq_len(n_ctry)) {               # país en COLUMNAS
    A_cols <- idxA_block(l)
    W_cols <- idxW_block(l)
    
    Akl <- M[A_rows, A_cols, drop = FALSE]   # 48×48 (padre: k→l)
    Wkl <- W[W_rows, W_cols, drop = FALSE]   # 62×62 (hijo:  k→l)
    
    # Posiciones físicas dentro del bloque W(k,l) para cada id 1..62
    pos_r <- pos_by_id_row[[k]]  # filas
    pos_c <- pos_by_id_col[[l]]  # columnas
    
    # RES por id (1..62 × 1..62). Luego se reordenará a físico para insertar.
    RESkl <- matrix(0, nrow = 62, ncol = 62)
    dimnames(RESkl) <- list(as.character(1:62), as.character(1:62))
    
    # ---- Regla 1: Base (no especiales) ----
    for (r in sector_ids) {
      if (r %in% expand_keys) next
      for (c in sector_ids) {
        if (c %in% expand_keys) next
        r_child <- map_simple_child(r)   # mover
        c_child <- map_simple_child(c)   # mover
        val <- Akl[r, c]
        if (val != 0) {
          rr <- pos_r[r_child]           # posición física (filas)
          cc <- pos_c[c_child]           # posición física (cols)
          RESkl[r_child, c_child] <- RESkl[r_child, c_child] + val * Wkl[rr, cc]  # multiplicar
        }
      }
    }
    
    # ---- Regla 3: columnas especiales (3.1) y diagonal (3.2) ----
    for (p_chr in names(expand_sets)) {
      p <- as.integer(p_chr)
      kids_p <- expand_sets[[p_chr]]
      
      # 3.1: fila NO especial → vector a columnas hijas
      for (r in sector_ids) {
        if (r %in% expand_keys) next
        r_child <- map_simple_child(r)   # mover
        val <- Akl[r, p]
        if (val != 0) {
          rr <- pos_r[r_child]
          CC <- pos_c[kids_p]
          RESkl[r_child, kids_p] <- RESkl[r_child, kids_p] + val * Wkl[rr, CC]    # multiplicar
        }
      }
      
      # 3.2: (p,p) → bloque hijos×hijos
      val_pp <- Akl[p, p]
      if (val_pp != 0) {
        RR <- pos_r[kids_p]
        CC <- pos_c[kids_p]
        RESkl[kids_p, kids_p] <- RESkl[kids_p, kids_p] + val_pp * Wkl[RR, CC]     # multiplicar
      }
    }
    
    # ---- Regla 3 simétrica: filas especiales (3.3) ----
    for (p_chr in names(expand_sets)) {
      p <- as.integer(p_chr)
      kids_p <- expand_sets[[p_chr]]
      
      # columna NO especial → vector a filas hijas
      for (c in sector_ids) {
        if (c %in% expand_keys) next
        c_child <- map_simple_child(c)   # mover
        val <- Akl[p, c]
        if (val != 0) {
          RR <- pos_r[kids_p]
          cc <- pos_c[c_child]
          RESkl[kids_p, c_child] <- RESkl[kids_p, c_child] + val * Wkl[RR, cc]    # multiplicar
        }
      }
    }
    
    # ---- Regla 2.3: cruces especiales distintos (q != p) ----
    for (q in expand_keys) {
      kids_q <- expand_sets[[as.character(q)]]
      for (p in expand_keys) {
        if (p == q) next
        kids_p <- expand_sets[[as.character(p)]]
        val_qp <- Akl[q, p]
        if (val_qp != 0) {
          RR <- pos_r[kids_q]   # filas (ids kids_q)
          CC <- pos_c[kids_p]   # cols  (ids kids_p)
          QQ <- pos_r[kids_p]   # filas (ids kids_p) para término transpuesto
          PP <- pos_c[kids_q]   # cols  (ids kids_q) para término transpuesto
          block_qp <- Wkl[RR, CC, drop = FALSE]
          block_pq <- Wkl[QQ, PP, drop = FALSE]
          avg_block <- (block_qp + t(block_pq)) / (length(kids_q) + length(kids_p))
          RESkl[kids_q, kids_p] <- RESkl[kids_q, kids_p] + val_qp * avg_block
        }
      }
    }
    
    # ---- Reordenar RESkl (por id) al orden físico del bloque W(k,l) e insertar ----
    inv_r <- order(pos_r)                  # índice id->posición física (filas)
    inv_c <- order(pos_c)                  # índice id->posición física (cols)
    RESkl_phys <- RESkl[inv_r, inv_c, drop = FALSE]
    
    RES_global[W_rows, W_cols] <- RESkl_phys
  }
}

# ---------- Exportar (openxlsx) ----------
openxlsx::write.xlsx(as.data.frame(RES_global),
                     file = "Coeficientes_tecnicos_finales.xlsx",
                     rowNames = FALSE, overwrite = TRUE)

# ---------- Diagnóstico rápido ----------
cat(sprintf(
  "A: %dx%d (%d países) | W: %dx%d | RES_global: %dx%d (bloques 62x62 x %d×%d)\n",
  nrow(M), ncol(M), n_ctry, nrow(W), ncol(W), nrow(RES_global), ncol(RES_global), n_ctry, n_ctry
))
if (n_ctry >= 2) {
  sum11 <- sum(abs(RES_global[idxW_block(1), idxW_block(1)]))
  sum12 <- sum(abs(RES_global[idxW_block(1), idxW_block(2)]))
  cat("Suma abs bloque (1,1):", sum11, " | bloque (1,2):", sum12, "\n")
}

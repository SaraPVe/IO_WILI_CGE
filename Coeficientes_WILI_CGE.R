install.packages("writexl")   # solo la primera vez
library(writexl)
library(openxlsx)

# Ruta
ruta_base <- "W.xlsx"
hoja_base <- 1  

Matriz_WILIAM <- read.xlsx(ruta_base, sheet = hoja_base, rowNames = TRUE)
Matriz_WILIAM <- data.matrix(Matriz_WILIAM)

# —————————————————————————————————————————————————————
# 0) Helpers y definición de grupos (6 y 21 juntos)
# —————————————————————————————————————————————————————
clean_names <- function(x) gsub("\\s*-\\s*", "-", trimws(x))
extract_id  <- function(x) as.integer(sub(".*-(\\d+)$", "\\1", x))

rownames(Matriz_WILIAM) <- clean_names(rownames(Matriz_WILIAM))
colnames(Matriz_WILIAM) <- clean_names(colnames(Matriz_WILIAM))

codes <- rownames(Matriz_WILIAM)
ids   <- extract_id(codes)
n     <- length(ids)

# Subgrupos de sectores subdivididos (ahora con B5 = {6,21})
B_groups <- list(
  B1 = 9:17,
  B2 = 47:49,
  B3 = 50:51,
  B4 = 58:62,
  B5 = c(6, 21)  # NUEVO: 6 y 21 juntos como un subgrupo
)

# Conjunto de posiciones (índices 1..n) que pertenecen a cualquier B*
B_all <- sort(which(ids %in% unlist(B_groups, use.names = FALSE)))
C2    <- B_all                       # columnas/fila subdivididas
A     <- sort(setdiff(seq_len(n), B_all))  # no subdivididos

# Helpers para obtener índices de grupo o nombre de grupo
get_grp_indices <- function(i_index) {
  id_i <- ids[i_index]
  for (nm in names(B_groups)) if (id_i %in% B_groups[[nm]]) {
    return(which(ids %in% B_groups[[nm]]))
  }
  return(NULL) # está en A
}

# —————————————————————————————————————————————————————
# 1) Construcción de la matriz de coeficientes W
# —————————————————————————————————————————————————————
W <- matrix(NA, n, n, dimnames = list(codes, codes))

for (i in seq_len(n)) {
  inA_i <- i %in% A
  grp_i <- get_grp_indices(i)      # NULL si fila está en A
  for (j in seq_len(n)) {
    xij    <- Matriz_WILIAM[i, j]
    inA_j  <- j %in% A
    inC2_j <- j %in% C2
    grp_j  <- get_grp_indices(j)   # NULL si col está en A
    
    # 1.1 A×A → 1
    if (inA_i && inA_j) {
      W[i,j] <- 1
      
      # 1.2 A×C2 → normalización por suma de fila i sobre C2
    } else if (inA_i && inC2_j) {
      denom <- sum(Matriz_WILIAM[i, C2])
      W[i,j] <- if (denom == 0) 0 else xij/denom
      
      # 1.3 Bx×A → normalización por suma de columna j dentro del grupo de i
    } else if (!is.null(grp_i) && inA_j) {
      denom <- sum(Matriz_WILIAM[grp_i, j])
      W[i,j] <- if (denom == 0) 0 else xij/denom
      
      # 1.4 Bx×By → normalización por suma total del bloque grp_i × grp_j
    } else if (!is.null(grp_i) && !is.null(grp_j)) {
      denom <- sum(Matriz_WILIAM[grp_i, grp_j])
      W[i,j] <- if (denom == 0) 0 else xij/denom
      
      # 1.5 resto → NA
    } else {
      W[i,j] <- NA
    }
  }
}

# —————————————————————————————————————————————————————
# 2) CHEQUEOS de normalización (dinámicos)
# —————————————————————————————————————————————————————
tol <- 1e-8
all_ok <- TRUE

# 2.a) A×A == 1
if (!all(W[A, A, drop=FALSE] == 1)) {
  cat("✖ Error en A×A (identidad)\n"); all_ok <- FALSE
} else cat("✔ A×A OK\n")

# 2.b) A×C2: cada fila i en A (con flujo a C2) suma 1 sobre C2
rs_AC2 <- sapply(A, function(i) {
  d <- sum(Matriz_WILIAM[i, C2])
  if (d == 0) return(NA_real_)
  sum(W[i, C2], na.rm=TRUE)
})
for (k in which(!is.na(rs_AC2))) {
  if (abs(rs_AC2[k] - 1) > tol) {
    cat(sprintf("✖ A×C2 fila %s suma=%.6f\n", codes[A[k]], rs_AC2[k])); all_ok <- FALSE
  }
}
cat("✔ A×C2 comprobado\n")

# 2.c) Bx×A: para cada subgrupo, columnas j en A suman 1
for (nm in names(B_groups)) {
  grp_pos <- which(ids %in% B_groups[[nm]])
  for (j in A) {
    d <- sum(Matriz_WILIAM[grp_pos, j])
    if (d == 0) next
    s <- sum(W[grp_pos, j], na.rm=TRUE)
    if (abs(s-1) > tol) {
      cat(sprintf("✖ %s×A col %s suma=%.6f\n", nm, codes[j], s)); all_ok <- FALSE
    }
  }
}
cat("✔ Bx×A comprobado\n")

# 2.d) Bx×By: cada bloque suma 1
for (g1 in names(B_groups)) {
  grp1_pos <- which(ids %in% B_groups[[g1]])
  for (g2 in names(B_groups)) {
    grp2_pos <- which(ids %in% B_groups[[g2]])
    total <- sum(W[grp1_pos, grp2_pos], na.rm=TRUE)
    if (abs(total - 1) > tol) {
      cat(sprintf("✖ %s×%s suma bloque=%.6f\n", g1, g2, total)); all_ok <- FALSE
    }
  }
}
cat("✔ Bx×By comprobado\n")

if (all_ok) {cat("\n🎉 Todos los checks PASAN (tol =", tol, ")\n")
}else { cat("\n⚠️ Algunos checks FALLARON. Revisa los mensajes.\n")}

# —————————————————————————————————————————————————————
# 2) Exportación
# —————————————————————————————————————————————————————
write_xlsx(as.data.frame(W, stringsAsFactors = FALSE),
           path = "Coeficientes_WILI.xlsx")

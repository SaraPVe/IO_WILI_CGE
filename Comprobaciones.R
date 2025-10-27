# —————————————————————————————————————————————————————
# CHEQUEOS de normalización (dinámicos)
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
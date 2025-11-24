##############################################
# 0. Librerías
##############################################

# Instalar solo la primera vez (si no las tienes)
install.packages("openxlsx")
install.packages("tidyverse")

library(openxlsx)
library(tidyverse)

##############################################
# 1. UNIZAR MATRIX
unizar_m <- read.xlsx("data_Unizar.xlsx", sheet = 1, colNames = FALSE)
dim(unizar_m)      # solo para ver dimensiones
# nrow(unizar_m); ncol(unizar_m)

##############################################
# 2. Leer X_CGE.csv como vector columna
##############################################

# X_CGE.csv = un solo vector columna sin cabecera
X_CGE <- read.csv("X_CGE.csv", header = FALSE)

# Extraer la primera columna como vector numérico
X_vec <- as.numeric(X_CGE[[1]])

# Comprobaciones básicas
cat("Longitud de X_vec: ", length(X_vec), "\n")
cat("Número de columnas de unizar_m: ", ncol(unizar_m), "\n")

if (length(X_vec) != ncol(unizar_m)) {
  stop("⚠️ length(X_vec) y ncol(unizar_m) NO coinciden. Revisa el orden de X.")
}

##############################################
# 3. Asegurarnos de que unizar_m es numérica
##############################################

A_mat <- as.matrix(unizar_m)
storage.mode(A_mat) <- "numeric"

##############################################
# 4. (Opcional) Chequeo rápido de que A_mat tiene pinta de coeficientes
##############################################

#cat("\nResumen de A_mat (posibles coeficientes técnicos):\n")
#print(summary(as.vector(A_mat)))

##############################################
# 5. Construir la matriz IO en valores (Z)
#    Z_ij = a_ij * x_j  →  Z = A * diag(X)
##############################################

IO_unizar <- sweep(A_mat, 2, X_vec, `*`)

cat("\nResumen de IO_unizar (valores monetarios):\n")
print(summary(as.vector(IO_unizar)))

##############################################
# 6. Guardar la matriz IO en Excel
##############################################

# Ojo: un objeto de 1680x1680 puede pesar bastante en Excel,
# pero en principio openxlsx lo aguanta.
write.xlsx(IO_unizar, file = "IO_unizar.xlsx", colnames= FALSE, rownames =FALSE )

cat("\n✅ IO_unizar creada y guardada como 'IO_unizar.xlsx'\n")

# —————————————————————————————————————————————————————
# 0) Prepara mapping_list, helpers y extrae Austria
# —————————————————————————————————————————————————————
mapping_list <- list(
  '1'=1,   '2'=2,   '3'=3,    '4'=4,
  '5'=9:17,'6'=8,   '7'=5,    '8'=21,
  '9'=7,   '10'=18, '11'=19,  '12'=20,
  '13'=22, '14'=23, '15'=24,  '16'=25,
  '17'=26, '18'=27, '19'=28,  '20'=29,
  '21'=30, '22'=31, '23'=32,  '24'=33,
  '25'=34, '26'=35, '27'=36,  '28'=37,
  '29'=38, '30'=39, '31'=40,  '32'=41,
  '33'=42, '34'=c(43,44),'35'=45,'36'=46,
  '37'=c(50,51),'38'=52,'39'=c(47,48,49),
  '40'=53, '41'=54, '42'=55,  '43'=56,
  '44'=57, '45'=58:62,'46'=59,'47'=60,'48'=61
)

clean <- function(x) gsub("\\s*-\\s*","-",trimws(x))
extract_id <- function(x) as.integer(sub(".*-(\\d+)$","\\1", x))

# Limpia nombres y extrae sólo AUSTRIA
rownames(Matriz_WILIAM)   <- clean(rownames(Matriz_WILIAM))
colnames(Matriz_WILIAM)   <- clean(colnames(Matriz_WILIAM))
rownames(Matriz_UNIZAR_Z) <- clean(rownames(Matriz_UNIZAR_Z))
colnames(Matriz_UNIZAR_Z) <- clean(colnames(Matriz_UNIZAR_Z))

AW_codes <- sort(grep("^AUSTRIA-", rownames(Matriz_WILIAM),   value=TRUE))  # 62 filas
ZU_codes <- sort(grep("^AUSTRIA-", rownames(Matriz_UNIZAR_Z), value=TRUE))  # 48 filas

# Matrices restringidas a Austria
M_A   <- Matriz_WILIAM[AW_codes, AW_codes, drop=FALSE]   # 62×62 absoluta
Z_AU  <- Matriz_UNIZAR_Z[ZU_codes, ZU_codes, drop=FALSE] # 48×48 agregada

# Debes tener W_A ya construido (62×62) con dimnames = AW_codes
# —————————————————————————————————————————————————————
# 1) Para cada fila ii, columna jj de W_A: 
#     multiplica W_A[ii,jj] × Z_AU[parent(ii), parent(jj)]
# —————————————————————————————————————————————————————
ids_A    <- extract_id(AW_codes)
m        <- length(ids_A)

# 1.1) Calcula parent_of: para cada posición ii, cuál es el sector "padre" en Z_AU
parent_of <- integer(m)
for(ii in seq_len(m)) {
  id <- ids_A[ii]
  # buscamos en mapping_list en qué vector de hijos está este id
  padres <- names(mapping_list)[ sapply(mapping_list, function(subs) id %in% subs) ]
  # si no está en ninguno, el padre es él mismo
  parent_of[ii] <- if(length(padres)==0) id else as.integer(padres)
}

# 1.2) Expande Z_AU a una matriz 62×62 de padres
Z_exp <- matrix(NA, m, m, dimnames = list(AW_codes, AW_codes))
for(ii in seq_len(m)) {
  for(jj in seq_len(m)) {
    pi <- parent_of[ii]
    pj <- parent_of[jj]
    Z_exp[ii,jj] <- Z_AU[paste0("AUSTRIA-",pi), paste0("AUSTRIA-",pj)]
  }
}

# 1.3) Elementwise multiplicación
M_A62 <- W_A * Z_exp

# —————————————————————————————————————————————————————
# 2) Comprueba que has obtenido un 62×62 y algunos bloques cierren
# —————————————————————————————————————————————————————
stopifnot(all(dim(M_A62)==c(62,62)))

# Por ejemplo, sector agregado 5 → hijos 9:17
orig_5     <- Z_AU["AUSTRIA-5","AUSTRIA-5"]
sum_9_17   <- sum(M_A62[paste0("AUSTRIA-",9:17), paste0("AUSTRIA-",9:17)])
cat("AUSTRIA-5 → 9:17   suma nueva =", sum_9_17, " vs orig =", orig_5, "\n")

# Sector 34 → 43,44
orig_34    <- Z_AU["AUSTRIA-34","AUSTRIA-34"]
sum_43_44  <- sum(M_A62[c("AUSTRIA-43","AUSTRIA-44"),
                        c("AUSTRIA-43","AUSTRIA-44")])
cat("AUSTRIA-34 → 43,44 suma =", sum_43_44, " vs orig =", orig_34, "\n")

# Sector 39 → 47:49
orig_39    <- Z_AU["AUSTRIA-39","AUSTRIA-39"]
sum_47_49  <- sum(M_A62[paste0("AUSTRIA-",47:49),
                        paste0("AUSTRIA-",47:49)])
cat("AUSTRIA-39 → 47:49 suma =", sum_47_49, " vs orig =", orig_39, "\n")

# Sector 37 → 50:51
orig_37    <- Z_AU["AUSTRIA-37","AUSTRIA-37"]
sum_50_51  <- sum(M_A62[paste0("AUSTRIA-",50:51),
                        paste0("AUSTRIA-",50:51)])
cat("AUSTRIA-37 → 50:51 suma =", sum_50_51, " vs orig =", orig_37, "\n")

# Sector 45 → 58:62
orig_45    <- Z_AU["AUSTRIA-45","AUSTRIA-45"]
sum_58_62  <- sum(M_A62[paste0("AUSTRIA-",58:62),
                        paste0("AUSTRIA-",58:62)])
cat("AUSTRIA-45 → 58:62 suma =", sum_58_62, " vs orig =", orig_45, "\n")




# 1) Columnas NO desagregadas
cols_no_desag <- setdiff(ZU_codes, paste0("AUSTRIA-", padres))

# 2) Para cada padre, calcula la suma de filas de sus hijos en M_A62
res_list <- lapply(padres, function(p) {
  padre_code <- paste0("AUSTRIA-", p)
  kids_codes <- paste0("AUSTRIA-", mapping_list[[as.character(p)]])
  
  # comprueba solo las columnas no desagregadas
  df <- data.frame(
    padre       = padre_code,
    columna     = cols_no_desag,
    suma_hijos  = sapply(cols_no_desag, function(cj) sum(M_A62[kids_codes, cj], na.rm=TRUE)),
    original    = as.numeric(Z_AU[padre_code, cols_no_desag]),
    stringsAsFactors = FALSE
  )
  df$diferencia <- df$suma_hijos - df$original
  df
})

# 3) Júntalo en un único data.frame y muéstralo
comprobacion <- do.call(rbind, res_list)
print(comprobacion)

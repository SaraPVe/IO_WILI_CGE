# Instalar el paquete si es necesario
# install.packages("readxl")
# install.packages("writexl") # Para guardar los resultados en un nuevo Excel

# Cargar las librerías
library(readxl)
library(openxlsx)
library(writexl)
library(dplyr) # Necesario si quieres usar las funciones de conteo del final

# --- 1. Definir Nombres de Archivos ---

archivo_wiliam <- "W.xlsx"
archivo_unizar <- "IO_wiliam.xlsx"

# --- 2. Cargar los Datos Aplicando las Omisiones ---

# 2.1. Cargar IO_wiliam: Ignorar la PRIMERA FILA y la PRIMERA COLUMNA

datos_wiliam_completo <- read_excel(archivo_wiliam, sheet = 1, col_names = FALSE)

# Omitimos la primera fila y la primera columna
matriz_wiliam <- datos_wiliam_completo[-1, -1]
colnames(matriz_wiliam) <- NULL # Eliminamos los nombres de columna si se generaron
# Convertir la matriz de Wiliam a numérica
matriz_wiliam_num <- as.matrix(matriz_wiliam)
# Intentamos convertir a tipo "double" (número con decimales)
storage.mode(matriz_wiliam_num) <- "double"
# Convertir la matriz de Unizar a numérica
matriz_unizar_num <- as.matrix(matriz_unizar)
storage.mode(matriz_unizar_num) <- "double"

# 2.2. Cargar IO_unizar: Ignorar solo la PRIMERA FILA
# Usamos `skip = 1` para ignorar la primera fila.
datos_unizar_completo <- read_excel(archivo_unizar, sheet = 1, col_names = FALSE)
matriz_unizar <- datos_unizar_completo[-1, -1]
colnames(matriz_unizar) <- NULL # Eliminamos los nombres de columna si se generaron


# --- 3. Validación de Dimensiones (CRUCIAL) ---

# Usar identical() asegura que el resultado siempre será TRUE o FALSE
if (!identical(dim(matriz_wiliam), dim(matriz_unizar))) {
  stop(paste("ERROR: Las matrices resultantes no tienen las mismas dimensiones. \n",
             "IO_wiliam (sin F1/C1): Filas=", nrow(matriz_wiliam), ", Cols=", ncol(matriz_wiliam), "\n",
             "IO_unizar (sin F1): Filas=", nrow(matriz_unizar), ", Cols=", ncol(matriz_unizar)))
}
# --- 4. Realizar la Resta Celda por Celda ---

# La operación se realiza elemento a elemento
# RESULTADO = (Valor de IO_wiliam) - (Valor de IO_unizar)
df_resta_resultado <- matriz_wiliam_num - matriz_unizar_num

# Convertimos la matriz de R a un DataFrame para el resultado final y guardado
df_resta_resultado <- as.data.frame(df_resta_resultado)

# --- 5. Aplicar Formato Condicional y Guardar en Excel ---

# 5.1. Crear un nuevo libro de Excel de openxlsx
wb <- createWorkbook()

# 5.2. Añadir la hoja de cálculo y escribir los datos
addWorksheet(wb, "Diferencias")
writeData(wb, "Diferencias", df_resta_resultado)
# El rango va desde A1 hasta la última celda de la matriz de resultados.
# Se utiliza paste0() para construir el rango de celdas dinámicamente:
ultima_fila <- nrow(df_resta_resultado)
ultima_columna <- ncol(df_resta_resultado)
rango_datos <- paste0("A1:", int2col(ultima_columna), ultima_fila)

# 5.5. Guardar el archivo Excel con el formato condicional
saveWorkbook(wb, nombre_archivo_salida, overwrite = TRUE)


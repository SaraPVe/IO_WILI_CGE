OBJETIVO:

Soft link entre el CGE y el IAM.

Para eso:

1.- Tienen que tener la misma desagregación de sectores ambas matrices, para eso se ha hecho la matriz con los pesos por sectores agregados: Coeficientes WILIAM Como este paso es vital, se ha generado un script de comprobación: Comprobaciones_WILI_CGE

2.- Generar la tabla con las Z, para lo que se tiene que multiplicar la tabla de coeficientes WILIAM por la matriz A de UNIZAR: Matriz_A_PANTHEON Como este es vital, se ha generado un script de comprobación: Comprobaciones_Matriz_A_PANTHEON

3.- En el modelo de WILIAM la IO entra en tres diferentes scripts, para los cuales, se necesita la IO (por lo que se multiplicará, la matriz por las Z): IO_PANTHEON Los ficheros que convierten la nueva IO en las import shares intermidiate y IS por origin las tecnical coefficient total,m se encuentran en otro documento. UNIFICAR.

Explicación de los diferentes ficheros y scripts
Preparación del entorno y carga de datos

Instala (solo la primera vez) y carga las librerías writexl y openxlsx, necesarias para leer y escribir archivos de Excel en R.
Define la ruta y hoja de cálculo que contienen la matriz original W.xlsx, la lee como data frame con nombres de fila, y la convierte a matriz numérica para facilitar los cálculos posteriores. 2. Limpieza de etiquetas y definición de grupos

Estandariza los nombres de filas y columnas eliminando espacios alrededor de guiones y extrayendo los identificadores numéricos que van al final de cada etiqueta (por ejemplo, “Sector-21” → id 21).
Crea cinco subgrupos B1–B5 para ciertos sectores subdivididos, destacando que B5 agrupa conjuntamente a los sectores con id 6 y 21. A partir de esos subgrupos deriva dos conjuntos de índices:

A, los sectores no subdivididos;

C2 (equivalente a todos los B), los sectores subdivididos que requieren un tratamiento especial.
Define un helper get_grp_indices que, dado un índice de fila o columna, devuelve los índices que pertenecen al mismo subgrupo B; si no pertenece a ninguno, indica que está en A. 3. Construcción de la matriz de coeficientes W

Inicializa una matriz cuadrada W (mismas dimensiones y etiquetas que la original) llena de NA y recorre todas las combinaciones fila/columna para poblarla siguiendo reglas de normalización específicas según si la fila/columna pertenece a A o a alguno de los subgrupos B.

    A×A: fija los coeficientes a 1 (identidad).

    A×C2: normaliza cada entrada dividiéndola por la suma de la fila correspondiente restringida a las columnas subdivididas.

    B×A: normaliza por la suma de la columna dentro del subgrupo de la fila.

    B×B: normaliza por la suma total del bloque definido por los subgrupos de fila y columna.

    En cualquier otra combinación asigna NA.
Chequeos de calidad

Ejecuta comprobaciones dinámicas para verificar que las normalizaciones cumplen con las reglas esperadas: identidad en A×A, sumas unitarias por fila (A×C2), por columna dentro de cada subgrupo (B×A) y por bloque entre subgrupos (B×B). Si detecta desviaciones mayores que la tolerancia 1e-8, informa los errores; en caso contrario, confirma que todos los chequeos pasaron.

Exportación

Convierte la matriz W en data frame y la guarda en un nuevo archivo Coeficientes_WILI.xlsx, produciendo la versión normalizada de los coeficientes lista para su uso posterior.

# IO_WILI_CGE

Proyecto en R para construir una tabla input-output compatible con la desagregacion sectorial de WILIAM a partir de una matriz CGE de UNIZAR.

El flujo genera un "soft link" entre:

- CGE / UNIZAR: 35 paises x 48 sectores = matriz `1680 x 1680`.
- WILIAM: 35 paises x 62 sectores = matriz `2170 x 2170`.

La conversion usa `W.xlsx` como matriz de pesos/estructura sectorial y `info/Correspondance_final.xlsx` como tabla de correspondencia entre sectores UNIZAR (`code_za`) y WILIAM (`code_wi`).

## Estructura

```text
.
├── scripts/                  # Pipeline reproducible actual
├── docs/                     # Informes generados, incluida la validacion
├── legacy/                   # Scripts antiguos conservados como referencia
├── Data_CT/                  # Objetos .RData derivados/intermedios
├── info/                     # Correspondencias y notas originales
├── W.xlsx                    # Entrada: estructura/pesos WILIAM
├── data_UNIZAR.xlsx          # Entrada: matriz A, paises y sectores UNIZAR
├── X_CGE.csv                 # Entrada: produccion CGE
├── W_normalizada.xlsx        # Salida derivada
├── IO_unizar.xlsx            # Salida derivada
└── IO_wiliam.xlsx            # Salida final
```

## Entradas fuente

Estas son las entradas que no deberian sobrescribirse durante el pipeline:

- `W.xlsx`: matriz WILIAM original con etiquetas `PAIS-sector`.
- `data_UNIZAR.xlsx`: contiene la matriz `A`, la lista de sectores y la lista de paises.
- `X_CGE.csv`: vector de produccion CGE, sin cabecera.
- `info/Correspondance_final.xlsx`: correspondencia sectorial, hoja `Rafa_intermediate_wili`.

## Salidas derivadas

- `W_normalizada.xlsx`: pesos sectoriales normalizados por bloque pais-pais.
- `IO_unizar.xlsx`: matriz de flujos monetarios UNIZAR, calculada como `A * diag(X_CGE)`.
- `IO_wiliam.xlsx`: matriz final con desagregacion WILIAM.
- `Data_CT/Data_origin_UNIZAR.RData`: objeto ancho `data_origin`, con la misma estructura que el archivo original de `TECNICAL_COEFFICIENT`.
- `Data_CT/data_unizar.RData` y `Data_CT/data_origin_unizar.RData`: aliases del mismo objeto ancho `data_origin`.
- `Data_CT/wiliam_wide_c.RData` y `Data_CT/IO_wiliam.RData`: version serializada de la salida final.
- `docs/VALIDATION.md`: informe de validacion generado por el script 04.
- `docs/validation_results.csv`: tabla de checks generada por el script 04.
- `analisis_UNIZAR_vs_WILIAM.xlsx`: informe opcional de comparacion.

## Scripts actuales

Ejecutar desde la raiz del proyecto.

```bash
Rscript scripts/01_normalize_w.R
Rscript scripts/02_build_io_unizar.R
Rscript scripts/03_build_io_wiliam.R
Rscript scripts/04_validate_pipeline.R
```

Tambien se puede ejecutar todo el flujo:

```bash
Rscript scripts/run_all.R
```

Comparacion opcional:

```bash
Rscript scripts/05_compare_io.R
```

Exportar `Data_origin_UNIZAR.RData` en formato compatible con `TECNICAL_COEFFICIENT`:

```bash
Rscript scripts/06_export_data_unizar.R
```

## Logica del pipeline

1. `01_normalize_w.R`
   - Lee `W.xlsx`.
   - Normaliza la matriz por cada bloque pais-pais.
   - Aplica reglas especiales a los grupos sectoriales:
     `6/21`, `9-17`, `47-49`, `50-51`, `58-62`.
   - En sectores normales, convierte valores no cero en `1` y ceros en `0`.
   - Genera `W_normalizada.xlsx`.

2. `02_build_io_unizar.R`
   - Lee la matriz tecnica `A` de `data_UNIZAR.xlsx`.
   - Lee el vector `X_CGE.csv`.
   - Calcula:

   ```text
   IO_unizar[i,j] = A[i,j] * X_CGE[j]
   ```

   - Genera `IO_unizar.xlsx`.

3. `03_build_io_wiliam.R`
   - Lee `W_normalizada.xlsx`, `IO_unizar.xlsx` y la correspondencia sectorial.
   - Para cada celda WILIAM, localiza su sector padre UNIZAR.
   - Calcula:

   ```text
   IO_wiliam[hijo_i,hijo_j] =
     W_normalizada[hijo_i,hijo_j] * IO_unizar[padre_i,padre_j]
   ```

   - Genera `IO_wiliam.xlsx` en dimension `2170 x 2170`.

4. `04_validate_pipeline.R`
   - Recalcula `W_normalizada.xlsx` desde `W.xlsx` y compara.
   - Verifica las reglas de normalizacion de W.
   - Recalcula `IO_unizar.xlsx` desde `data_UNIZAR.xlsx` y `X_CGE.csv`.
   - Recalcula `IO_wiliam.xlsx` desde `W_normalizada.xlsx`, `IO_unizar.xlsx` y la correspondencia.
   - Escribe `docs/VALIDATION.md` y falla con codigo distinto de cero si algun check obligatorio falla.

## Requisitos

R 4.0 o superior. Paquetes necesarios:

```r
install.packages(c("openxlsx"))
```

Los scripts antiguos usaban `tidyverse`, `readxl`, `writexl`, `ggplot2` y `plotly`. El pipeline actual solo necesita `openxlsx`; `plotly` queda como dependencia opcional si se quieren graficos interactivos externos.

## Notas de mantenimiento

- Los scripts antiguos se han movido a `legacy/` para trazabilidad.
- No edites directamente `W_normalizada.xlsx`, `IO_unizar.xlsx` o `IO_wiliam.xlsx`: son salidas derivadas y deben regenerarse.
- Si cambia la correspondencia sectorial, vuelve a ejecutar los scripts `03` y `04`.
- Si cambia `W.xlsx`, vuelve a ejecutar desde el script `01`.
- Si cambia `data_UNIZAR.xlsx` o `X_CGE.csv`, vuelve a ejecutar desde el script `02`.

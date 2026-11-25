# Entrega corregida de UNIZAR del 25 de noviembre de 2026

El libro `entradas/unizar/2026.11.EMERGING_agg_corregido - WILLIAMS.xlsx`
conserva el archivo recibido. SHA-256
`053168285fc5185e93fe3d2ac7b5849a05b1838335b940e238f3706b032e94fc`.

| Hoja | Dimensión | Contenido |
|---|---:|---|
| `Z` | 1.680 × 1.680 | Flujos intermedios |
| `X` | 1.680 × 1 | Producción bruta |
| `VA` | 1 × 1.680 | Bloque de valor añadido de la entrega |
| `y` | 1.680 × 105 | Demanda final con columnas sin etiqueta |

Se usa el orden de 35 regiones × 48 sectores del proyecto. El libro carece
de etiquetas en los ejes productivos. La comparación con el vector X y la
A de la entrega anterior aporta evidencia de que coincide el orden.
`Z + VA` por columna reproduce `X` en la valoración de la fuente. El
libro carece de un desglose separado de impuestos y de la clave de las
105 columnas de demanda final. Se conservan sus posiciones de origen.

## Ejecución

Desde esta rama, con R, `openxlsx` y los archivos de Git LFS descargados:

```powershell
Rscript --vanilla IO_completa_a_resultados.R --tipo-entrada entrega --entrada 'entradas/unizar/2026.11.EMERGING_agg_corregido - WILLIAMS.xlsx' --pesos W.xlsx --correspondencia info/Correspondance_final.xlsx --salida resultados/unizar_entrega_20261125
```

En el espacio de trabajo reorganizado, quitar `--pesos` y
`--correspondencia` para utilizar las rutas predeterminadas en `entradas/`.
`referencias/Production.xlsx` aporta proporciones para repartir por X. Se
reparte Z en ambos ejes por las cuotas de producción de los hijos. Se
aplican las mismas cuotas a Y por proveedor y a VA por actividad. Se
comprueba la conservación de cada flujo de Z, cada columna de Y y X y
VA por región y sector padre.

La salida incluye `Coeficientes_intermedios.xlsx` con TC, BIS y BISO
intermedios y los balances de los dos niveles, y `MRIO62_entrega_UNIZAR.rds`
con Z, X, VA e Y de 105 columnas. Para generar las tablas de demanda final
de WILIAM hay que confirmar con UNIZAR qué uso y destino corresponde a cada
columna, y cómo están agrupados los seis usos del modelo.

## Comparación de entregas

- Recalculada como `Z / X` por columna, A coincide con la del 23 de
  noviembre (diferencia máxima de aproximadamente `1,1e-16`). Cambian
  los importes de X y Z. Irlanda pasa de 645.125 a 1.394.270 de X, mientras
  que LROW pasa de 25.852.062 a 25.102.916, en las unidades del libro.
- El mayor residuo de ventas a 48 es 0,00674. El de costes es inferior a
  `1e-6`. A 62, el mayor residuo de ventas es 0,00481. Desaparecen los 21
  casos de ventas intermedias superiores a X y el reparto conserva todos
  los flujos. Ningún TC individual supera uno y los BIS/BISO intermedios
  están dentro del intervalo unitario.
- En Malta persisten las cifras de X de alojamiento e inmobiliario, y sus
  sumas de coeficientes técnicos de aproximadamente 99,05 % y 99,75 %.
  Si el modelo solo cambia TC, BIS y BISO intermedios, la revisión contable
  por sí sola no resuelve el resultado del PIB per cápita de Malta.

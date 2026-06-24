# Pipeline logic

This project converts an IO matrix from UNIZAR sector detail to WILIAM sector detail.

## Main formula

For every WILIAM child cell, the final value is:

```text
IO_wiliam[wi_in, wi_out] =
  W_normalizada[wi_in, wi_out] *
  IO_unizar[za_parent(wi_in), za_parent(wi_out)]
```

`za_parent()` is defined by `info/Correspondance_final.xlsx`, sheet `Rafa_intermediate_wili`.

## Normalization rules for W

Normalization is applied inside every country-country block.

- Normal x normal sectors: non-zero values become `1`; zeros remain `0`.
- Normal x special sectors: values are normalized across the destination special group.
- Special x normal sectors: values are normalized across the origin special group.
- Special x special sectors: values are normalized across the complete special submatrix.

Special WILIAM sector groups:

```text
G6_21  = 6, 21
G9_17  = 9..17
G47_49 = 47..49
G50_51 = 50..51
G58_62 = 58..62
```

## Validation contract

The current output is considered valid only when:

- `W_normalizada.xlsx` is exactly reproducible from `W.xlsx` within numeric tolerance.
- All W normalization rules pass independently.
- `IO_unizar.xlsx` is reproducible from `data_UNIZAR.xlsx` and `X_CGE.csv`.
- `IO_wiliam.xlsx` is reproducible from `W_normalizada.xlsx`, `IO_unizar.xlsx` and the correspondence table.
- The final matrix has `2170 x 2170` numeric values, unique row identifiers and matching columns.

Run:

```bash
Rscript scripts/04_validate_pipeline.R
```


# Validation report

Generated: 2026-06-17 10:30:21 CEST

## Summary

- PASS: 28
- FAIL: 0

## Checks

| Check | Status | Value | Detail |
|---|---:|---|---|
| File exists: W.xlsx | PASS | /Users/portatildesara/Documents/GitHub/IO_WILI_CGE/W.xlsx |  |
| File exists: data_UNIZAR.xlsx | PASS | /Users/portatildesara/Documents/GitHub/IO_WILI_CGE/data_UNIZAR.xlsx |  |
| File exists: X_CGE.csv | PASS | /Users/portatildesara/Documents/GitHub/IO_WILI_CGE/X_CGE.csv |  |
| File exists: Correspondance_final.xlsx | PASS | /Users/portatildesara/Documents/GitHub/IO_WILI_CGE/info/Correspondance_final.xlsx |  |
| File exists: W_normalizada.xlsx | PASS | /Users/portatildesara/Documents/GitHub/IO_WILI_CGE/W_normalizada.xlsx |  |
| File exists: IO_unizar.xlsx | PASS | /Users/portatildesara/Documents/GitHub/IO_WILI_CGE/IO_unizar.xlsx |  |
| File exists: IO_wiliam.xlsx | PASS | /Users/portatildesara/Documents/GitHub/IO_WILI_CGE/IO_wiliam.xlsx |  |
| Optional package plotly available | PASS | FALSE | Only needed for interactive comparison plots. |
| W dimensions | PASS | 2170x2170 |  |
| W_normalizada dimensions | PASS | 2170x2170 |  |
| W labels match normalized output | PASS |  |  |
| W_normalizada finite values | PASS | 0 non-finite values |  |
| W_normalizada reproducible from W.xlsx | PASS | max_abs=5.55112e-16; max_rel=5.55112e-16 |  |
| W_normalizada normalization rules | PASS | max_cell_deviation=5.55112e-16; max_sum_deviation=2.44249e-15; bad_cells=0; bad_sums=0 |  |
| UNIZAR country/sector dimensions | PASS | 35 countries * 48 sectors = 1680 columns |  |
| X_CGE length matches UNIZAR columns | PASS | length(X)=1680; ncol(A)=1680 |  |
| IO_unizar dimensions | PASS | 1680x1680 |  |
| IO_unizar finite values | PASS | 0 non-finite values |  |
| IO_unizar reproducible from data_UNIZAR and X_CGE | PASS | max_abs=3.12924e-07; max_rel=5.08286e-15 |  |
| Sector correspondence has 62 WILIAM rows | PASS | 62 |  |
| Sector correspondence maps all WILIAM sectors once | PASS | 1,10,11,12,13,14,15,16,17,18,19,2,20,21,22,23,24,25,26,27,28,29,3,30,31,32,33,34,35,36,37,38,39,4,40,41,42,43,44,45,46,47,48,49,5,50,51,52,53,54,55,56,57,58,59,6,60,61,62,7,8,9 |  |
| IO_wiliam row identifiers | PASS |  |  |
| IO_wiliam column identifiers | PASS |  |  |
| IO_wiliam dimensions | PASS | 2170x2170 |  |
| IO_wiliam finite values | PASS | 0 non-finite values |  |
| IO_wiliam reproducible from W_normalizada and IO_unizar | PASS | max_abs=5.06639e-07; max_rel=9.3259e-15 |  |
| Data_CT/wiliam_wide_c.RData object exists | PASS |  |  |
| Data_CT/wiliam_wide_c.RData dimensions | PASS | 2170x2171 |  |

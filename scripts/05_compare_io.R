.args <- commandArgs(trailingOnly = FALSE)
.file_arg <- grep("^--file=", .args, value = TRUE)
.this_file <- if (length(.file_arg)) sub("^--file=", "", .file_arg[[1]]) else file.path(getwd(), "scripts", "05_compare_io.R")
SCRIPT_DIR <- dirname(normalizePath(.this_file, mustWork = FALSE))
source(file.path(SCRIPT_DIR, "00_config.R"))

require_packages("openxlsx")
check_files_exist(c(INPUT$w, OUTPUT$io_wiliam))

cat("Reading W.xlsx and IO_wiliam.xlsx...\n")
W <- read_w_matrix(INPUT$w)
IO_wiliam_df <- openxlsx::read.xlsx(OUTPUT$io_wiliam, colNames = TRUE, check.names = FALSE)
IO_wiliam_ids <- as.character(IO_wiliam_df[[1]])
IO_wiliam <- as_numeric_matrix(IO_wiliam_df[, -1, drop = FALSE])
rownames(IO_wiliam) <- IO_wiliam_ids

expected_ids <- gsub("-", "_", rownames(W))
expected_cols <- gsub("-", "_", colnames(W))
if (!identical(rownames(IO_wiliam), expected_ids) || !identical(colnames(IO_wiliam), expected_cols)) {
  stop("IO_wiliam labels do not match W labels.", call. = FALSE)
}

cat("Computing relative variation: (IO_wiliam - W) / W...\n")
zero_w <- W == 0
zero_io <- IO_wiliam == 0
tasa <- (IO_wiliam - W) / W
tasa[zero_w & zero_io] <- 0
tasa[!is.finite(tasa)] <- NA

abs_tasa <- abs(tasa)
summary_global <- data.frame(
  mean_rate = mean(tasa, na.rm = TRUE),
  mean_abs_rate = mean(abs_tasa, na.rm = TRUE),
  median_abs_rate = median(abs_tasa, na.rm = TRUE),
  max_abs_rate = max(abs_tasa, na.rm = TRUE),
  W_zero_IO_nonzero = sum(zero_w & !zero_io, na.rm = TRUE),
  IO_zero_W_nonzero = sum(zero_io & !zero_w, na.rm = TRUE),
  both_zero = sum(zero_w & zero_io, na.rm = TRUE)
)

row_summary <- data.frame(
  id = rownames(tasa),
  mean_abs_rate = rowMeans(abs_tasa, na.rm = TRUE),
  max_abs_rate = apply(abs_tasa, 1, max, na.rm = TRUE),
  stringsAsFactors = FALSE
)
row_summary <- row_summary[order(-row_summary$mean_abs_rate), ]

col_summary <- data.frame(
  id = colnames(tasa),
  mean_abs_rate = colMeans(abs_tasa, na.rm = TRUE),
  max_abs_rate = apply(abs_tasa, 2, max, na.rm = TRUE),
  stringsAsFactors = FALSE
)
col_summary <- col_summary[order(-col_summary$mean_abs_rate), ]

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Resumen_global")
openxlsx::writeData(wb, "Resumen_global", summary_global)
openxlsx::addWorksheet(wb, "Resumen_filas")
openxlsx::writeData(wb, "Resumen_filas", head(row_summary, 500))
openxlsx::addWorksheet(wb, "Resumen_columnas")
openxlsx::writeData(wb, "Resumen_columnas", head(col_summary, 500))
openxlsx::addWorksheet(wb, "Tasa_matriz")
openxlsx::writeData(wb, "Tasa_matriz", as.data.frame(tasa, check.names = FALSE), rowNames = TRUE)
openxlsx::saveWorkbook(wb, OUTPUT$comparison_excel, overwrite = TRUE)

cat("Done: ", OUTPUT$comparison_excel, "\n", sep = "")

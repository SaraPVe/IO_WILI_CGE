.args <- commandArgs(trailingOnly = FALSE)
.file_arg <- grep("^--file=", .args, value = TRUE)
.this_file <- if (length(.file_arg)) sub("^--file=", "", .file_arg[[1]]) else file.path(getwd(), "scripts", "04_validate_pipeline.R")
SCRIPT_DIR <- dirname(normalizePath(.this_file, mustWork = FALSE))
source(file.path(SCRIPT_DIR, "00_config.R"))

require_packages("openxlsx")
ensure_dir(dirname(OUTPUT$validation_report))

checks <- list()

add_check <- function(name, ok, value = "", detail = "") {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = name,
    status = if (isTRUE(ok)) "PASS" else "FAIL",
    value = as.character(value),
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

cat("Validating input and output files...\n")
required_files <- c(
  INPUT$w,
  INPUT$data_unizar,
  INPUT$x_cge,
  INPUT$correspondence,
  OUTPUT$w_normalized,
  OUTPUT$io_unizar,
  OUTPUT$io_wiliam
)

for (path in required_files) {
  add_check(paste("File exists:", basename(path)), file.exists(path), path)
}

optional_plotly <- requireNamespace("plotly", quietly = TRUE)
add_check("Optional package plotly available", TRUE, optional_plotly, "Only needed for interactive comparison plots.")

if (any(!file.exists(required_files))) {
  results <- do.call(rbind, checks)
  write.csv(results, OUTPUT$validation_csv, row.names = FALSE)
  stop("Required files are missing. See docs/validation_results.csv.", call. = FALSE)
}

cat("Validating W normalization...\n")
W_original <- read_w_matrix(INPUT$w)
W_normalized <- read_w_matrix(OUTPUT$w_normalized)

add_check("W dimensions", identical(dim(W_original), c(2170L, 2170L)), paste(dim(W_original), collapse = "x"))
add_check("W_normalizada dimensions", identical(dim(W_normalized), c(2170L, 2170L)), paste(dim(W_normalized), collapse = "x"))
add_check("W labels match normalized output", identical(rownames(W_original), rownames(W_normalized)) && identical(colnames(W_original), colnames(W_normalized)))
add_check("W_normalizada finite values", all(is.finite(W_normalized)), paste(sum(!is.finite(W_normalized)), "non-finite values"))

W_expected <- normalize_w_global(W_original)
w_abs <- max_abs_diff(W_normalized, W_expected)
w_rel <- max_rel_diff(W_normalized, W_expected)
add_check("W_normalizada reproducible from W.xlsx", w_abs <= 1e-12 || w_rel <= 1e-12, sprintf("max_abs=%g; max_rel=%g", w_abs, w_rel))

w_rules <- validate_w_rules(W_original, W_normalized)
add_check(
  "W_normalizada normalization rules",
  w_rules$bad_cells == 0 && w_rules$bad_sums == 0,
  sprintf(
    "max_cell_deviation=%g; max_sum_deviation=%g; bad_cells=%s; bad_sums=%s",
    w_rules$max_cell_deviation,
    w_rules$max_sum_deviation,
    w_rules$bad_cells,
    w_rules$bad_sums
  )
)

cat("Validating IO_unizar...\n")
A <- read_unizar_a(INPUT$data_unizar)
X <- read_x_cge(INPUT$x_cge)
countries_unizar <- read_unizar_countries(INPUT$data_unizar)
IO_unizar_actual <- as_numeric_matrix(openxlsx::read.xlsx(OUTPUT$io_unizar, colNames = FALSE))
IO_unizar_expected <- build_io_unizar(A, X)

add_check("UNIZAR country/sector dimensions", length(countries_unizar) * N_SECTORS_UNIZAR == ncol(A), sprintf("%s countries * %s sectors = %s columns", length(countries_unizar), N_SECTORS_UNIZAR, ncol(A)))
add_check("X_CGE length matches UNIZAR columns", length(X) == ncol(A), sprintf("length(X)=%s; ncol(A)=%s", length(X), ncol(A)))
add_check("IO_unizar dimensions", identical(dim(IO_unizar_actual), dim(IO_unizar_expected)), paste(dim(IO_unizar_actual), collapse = "x"))
add_check("IO_unizar finite values", all(is.finite(IO_unizar_actual)), paste(sum(!is.finite(IO_unizar_actual)), "non-finite values"))

io_u_abs <- max_abs_diff(IO_unizar_actual, IO_unizar_expected)
io_u_rel <- max_rel_diff(IO_unizar_actual, IO_unizar_expected)
add_check("IO_unizar reproducible from data_UNIZAR and X_CGE", io_u_abs <= 1e-6 || io_u_rel <= 1e-10, sprintf("max_abs=%g; max_rel=%g", io_u_abs, io_u_rel))

cat("Validating IO_wiliam...\n")
correspondence <- read_correspondence(INPUT$correspondence)
add_check("Sector correspondence has 62 WILIAM rows", nrow(correspondence) == N_SECTORS_WILIAM, nrow(correspondence))
add_check("Sector correspondence maps all WILIAM sectors once", setequal(as.integer(correspondence$code_wi), seq_len(N_SECTORS_WILIAM)) && !anyDuplicated(correspondence$code_wi), paste(sort(correspondence$code_wi), collapse = ","))

IO_wiliam_expected <- build_io_wiliam_matrix(
  W_normalized = W_normalized,
  IO_unizar = IO_unizar_expected,
  countries_unizar = countries_unizar,
  correspondence = correspondence
)

IO_wiliam_df <- openxlsx::read.xlsx(OUTPUT$io_wiliam, colNames = TRUE, check.names = FALSE)
IO_wiliam_ids <- as.character(IO_wiliam_df[[1]])
IO_wiliam_actual <- as_numeric_matrix(IO_wiliam_df[, -1, drop = FALSE])
rownames(IO_wiliam_actual) <- IO_wiliam_ids

add_check("IO_wiliam row identifiers", identical(IO_wiliam_ids, rownames(IO_wiliam_expected)))
add_check("IO_wiliam column identifiers", identical(colnames(IO_wiliam_actual), colnames(IO_wiliam_expected)))
add_check("IO_wiliam dimensions", identical(dim(IO_wiliam_actual), dim(IO_wiliam_expected)), paste(dim(IO_wiliam_actual), collapse = "x"))
add_check("IO_wiliam finite values", all(is.finite(IO_wiliam_actual)), paste(sum(!is.finite(IO_wiliam_actual)), "non-finite values"))

io_w_abs <- max_abs_diff(IO_wiliam_actual, IO_wiliam_expected)
io_w_rel <- max_rel_diff(IO_wiliam_actual, IO_wiliam_expected)
add_check("IO_wiliam reproducible from W_normalizada and IO_unizar", io_w_abs <= 1e-6 || io_w_rel <= 1e-10, sprintf("max_abs=%g; max_rel=%g", io_w_abs, io_w_rel))

rdata_path <- file.path(OUTPUT$data_ct, "wiliam_wide_c.RData")
if (file.exists(rdata_path)) {
  rdata_env <- new.env(parent = emptyenv())
  load(rdata_path, envir = rdata_env)
  add_check("Data_CT/wiliam_wide_c.RData object exists", exists("wiliam_wide_c", envir = rdata_env, inherits = FALSE))
  if (exists("wiliam_wide_c", envir = rdata_env, inherits = FALSE)) {
    rdata_obj <- get("wiliam_wide_c", envir = rdata_env)
    add_check("Data_CT/wiliam_wide_c.RData dimensions", identical(dim(rdata_obj), dim(IO_wiliam_df)), paste(dim(rdata_obj), collapse = "x"))
  }
} else {
  add_check("Data_CT/wiliam_wide_c.RData exists", FALSE, rdata_path)
}

results <- do.call(rbind, checks)
write.csv(results, OUTPUT$validation_csv, row.names = FALSE)

status_counts <- table(results$status)
failed <- results[results$status != "PASS", , drop = FALSE]

report_lines <- c(
  "# Validation report",
  "",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Summary",
  "",
  paste("- PASS:", if ("PASS" %in% names(status_counts)) status_counts[["PASS"]] else 0),
  paste("- FAIL:", nrow(failed)),
  "",
  "## Checks",
  "",
  "| Check | Status | Value | Detail |",
  "|---|---:|---|---|"
)

escape_md <- function(x) {
  x <- gsub("\\|", "\\\\|", x)
  x <- gsub("\n", "<br>", x)
  x
}

for (i in seq_len(nrow(results))) {
  report_lines <- c(
    report_lines,
    paste0(
      "| ",
      escape_md(results$check[i]),
      " | ",
      results$status[i],
      " | ",
      escape_md(results$value[i]),
      " | ",
      escape_md(results$detail[i]),
      " |"
    )
  )
}

writeLines(report_lines, OUTPUT$validation_report)

cat("Validation report written to ", OUTPUT$validation_report, "\n", sep = "")
cat("Validation CSV written to ", OUTPUT$validation_csv, "\n", sep = "")

if (nrow(failed) > 0) {
  print(failed)
  quit(status = 1)
}

cat("All required validation checks passed.\n")

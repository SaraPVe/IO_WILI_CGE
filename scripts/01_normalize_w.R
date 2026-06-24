.args <- commandArgs(trailingOnly = FALSE)
.file_arg <- grep("^--file=", .args, value = TRUE)
.this_file <- if (length(.file_arg)) sub("^--file=", "", .file_arg[[1]]) else file.path(getwd(), "scripts", "01_normalize_w.R")
SCRIPT_DIR <- dirname(normalizePath(.this_file, mustWork = FALSE))
source(file.path(SCRIPT_DIR, "00_config.R"))

require_packages("openxlsx")
check_files_exist(c(INPUT$w))

cat("Reading W matrix...\n")
W <- read_w_matrix(INPUT$w)

cat("Normalizing W matrix...\n")
W_normalized <- normalize_w_global(W)

cat("Writing ", OUTPUT$w_normalized, "...\n", sep = "")
openxlsx::write.xlsx(
  as.data.frame(W_normalized, check.names = FALSE),
  file = OUTPUT$w_normalized,
  rowNames = TRUE,
  overwrite = TRUE
)

cat("Done: W_normalizada.xlsx\n")

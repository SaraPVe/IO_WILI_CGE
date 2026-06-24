.args <- commandArgs(trailingOnly = FALSE)
.file_arg <- grep("^--file=", .args, value = TRUE)
.this_file <- if (length(.file_arg)) sub("^--file=", "", .file_arg[[1]]) else file.path(getwd(), "scripts", "02_build_io_unizar.R")
SCRIPT_DIR <- dirname(normalizePath(.this_file, mustWork = FALSE))
source(file.path(SCRIPT_DIR, "00_config.R"))

require_packages("openxlsx")
check_files_exist(c(INPUT$data_unizar, INPUT$x_cge))

cat("Reading UNIZAR coefficient matrix and X_CGE vector...\n")
A <- read_unizar_a(INPUT$data_unizar)
X <- read_x_cge(INPUT$x_cge)

cat("Building IO_unizar = A * diag(X)...\n")
IO_unizar <- build_io_unizar(A, X)

cat("Writing ", OUTPUT$io_unizar, "...\n", sep = "")
openxlsx::write.xlsx(IO_unizar, file = OUTPUT$io_unizar, colNames = FALSE, overwrite = TRUE)

cat("Done: IO_unizar.xlsx\n")

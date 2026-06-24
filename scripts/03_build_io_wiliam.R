.args <- commandArgs(trailingOnly = FALSE)
.file_arg <- grep("^--file=", .args, value = TRUE)
.this_file <- if (length(.file_arg)) sub("^--file=", "", .file_arg[[1]]) else file.path(getwd(), "scripts", "03_build_io_wiliam.R")
SCRIPT_DIR <- dirname(normalizePath(.this_file, mustWork = FALSE))
source(file.path(SCRIPT_DIR, "00_config.R"))

require_packages("openxlsx")
check_files_exist(c(OUTPUT$w_normalized, OUTPUT$io_unizar, INPUT$data_unizar, INPUT$correspondence))
ensure_dir(OUTPUT$data_ct)

cat("Reading W_normalizada, IO_unizar and sector correspondence...\n")
W_normalized <- read_w_matrix(OUTPUT$w_normalized)
IO_unizar <- as_numeric_matrix(openxlsx::read.xlsx(OUTPUT$io_unizar, colNames = FALSE))
countries_unizar <- read_unizar_countries(INPUT$data_unizar)
correspondence <- read_correspondence(INPUT$correspondence)

cat("Building IO_wiliam in WILIAM sector detail...\n")
IO_wiliam_matrix <- build_io_wiliam_matrix(
  W_normalized = W_normalized,
  IO_unizar = IO_unizar,
  countries_unizar = countries_unizar,
  correspondence = correspondence
)

IO_wiliam <- data.frame(
  insecou = rownames(IO_wiliam_matrix),
  as.data.frame(IO_wiliam_matrix, check.names = FALSE),
  check.names = FALSE
)

cat("Writing ", OUTPUT$io_wiliam, "...\n", sep = "")
openxlsx::write.xlsx(IO_wiliam, file = OUTPUT$io_wiliam, overwrite = TRUE)

cat("Writing RData outputs...\n")
wiliam_wide_c <- IO_wiliam
save(wiliam_wide_c, file = file.path(OUTPUT$data_ct, "wiliam_wide_c.RData"))
save(IO_wiliam, file = file.path(OUTPUT$data_ct, "IO_wiliam.RData"))

cat("Done: IO_wiliam.xlsx\n")

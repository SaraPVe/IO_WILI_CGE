.args <- commandArgs(trailingOnly = FALSE)
.file_arg <- grep("^--file=", .args, value = TRUE)
.this_file <- if (length(.file_arg)) sub("^--file=", "", .file_arg[[1]]) else file.path(getwd(), "scripts", "06_export_data_unizar.R")
SCRIPT_DIR <- dirname(normalizePath(.this_file, mustWork = FALSE))
source(file.path(SCRIPT_DIR, "00_config.R"))

require_packages("openxlsx")
template_path <- Sys.getenv(
  "DATA_ORIGIN_TEMPLATE",
  "/Users/portatildesara/Documents/GitHub/TECNICAL_COEFFICIENT/Data/Data_origin_UNIZAR.RData"
)
check_files_exist(c(OUTPUT$io_wiliam, template_path))
ensure_dir(OUTPUT$data_ct)

cat("Reading original Data_origin_UNIZAR template...\n")
template_env <- new.env(parent = emptyenv())
load(template_path, envir = template_env)
if (!exists("data_origin", envir = template_env, inherits = FALSE)) {
  stop("Template RData must contain an object named data_origin.", call. = FALSE)
}
data_origin <- get("data_origin", envir = template_env)

cat("Reading IO_wiliam block...\n")
IO_wiliam_df <- openxlsx::read.xlsx(OUTPUT$io_wiliam, colNames = TRUE, check.names = FALSE)
IO_wiliam <- as_numeric_matrix(IO_wiliam_df[, -1, drop = FALSE])

if (!identical(dim(IO_wiliam), c(2170L, 2170L))) {
  stop("IO_wiliam must have a 2170 x 2170 numeric block.", call. = FALSE)
}
if (nrow(data_origin) < 2206 || ncol(data_origin) < 2172) {
  stop("Template does not have the expected Data_origin_UNIZAR shape.", call. = FALSE)
}

cat("Injecting IO_wiliam into the template intermediate block...\n")
data_origin[seq_len(2170L), 3:(2 + 2170L)] <- IO_wiliam
attr(data_origin, "source_files") <- list(
  template = template_path,
  IO_wiliam = OUTPUT$io_wiliam
)
attr(data_origin, "generated_at") <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
attr(data_origin, "note") <- paste(
  "Original Data_origin_UNIZAR structure preserved;",
  "the 2170 x 2170 intermediate block was replaced with IO_wiliam."
)

output_paths <- c(
  file.path(OUTPUT$data_ct, "Data_origin_UNIZAR.RData"),
  file.path(OUTPUT$data_ct, "data_origin_unizar.RData"),
  file.path(OUTPUT$data_ct, "data_unizar.RData")
)

for (output_path in output_paths) {
  cat("Writing ", output_path, "...\n", sep = "")
  save(data_origin, file = output_path, compress = "xz")
}

cat("Done: Data_CT/Data_origin_UNIZAR.RData\n")

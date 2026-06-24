# Shared configuration and helper functions for the IO_WILI_CGE pipeline.

if (!exists("SCRIPT_DIR", inherits = FALSE)) {
  SCRIPT_DIR <- dirname(normalizePath(file.path("scripts", "00_config.R"), mustWork = TRUE))
}

PROJECT_ROOT <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = TRUE)

project_path <- function(...) {
  file.path(PROJECT_ROOT, ...)
}

INPUT <- list(
  w = project_path("W.xlsx"),
  data_unizar = project_path("data_UNIZAR.xlsx"),
  x_cge = project_path("X_CGE.csv"),
  correspondence = project_path("info", "Correspondance_final.xlsx")
)

OUTPUT <- list(
  w_normalized = project_path("W_normalizada.xlsx"),
  io_unizar = project_path("IO_unizar.xlsx"),
  io_wiliam = project_path("IO_wiliam.xlsx"),
  data_ct = project_path("Data_CT"),
  validation_report = project_path("docs", "VALIDATION.md"),
  validation_csv = project_path("docs", "validation_results.csv"),
  comparison_excel = project_path("analisis_UNIZAR_vs_WILIAM.xlsx")
)

SHEETS <- list(
  unizar_a = "A",
  unizar_countries = "country list",
  correspondence = "Rafa_intermediate_wili"
)

N_SECTORS_UNIZAR <- 48L
N_SECTORS_WILIAM <- 62L
EPS <- 1e-8

SPECIAL_GROUPS <- list(
  G6_21 = c(6L, 21L),
  G9_17 = 9L:17L,
  G47_49 = 47L:49L,
  G50_51 = 50L:51L,
  G58_62 = 58L:62L
)

ALL_SPECIAL_IDS <- sort(unique(unlist(SPECIAL_GROUPS)))
NORMAL_IDS <- setdiff(seq_len(N_SECTORS_WILIAM), ALL_SPECIAL_IDS)

require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing R packages: ",
      paste(missing, collapse = ", "),
      ". Install them before running this script.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

check_files_exist <- function(paths) {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop("Missing files:\n", paste(missing, collapse = "\n"), call. = FALSE)
  }
  invisible(TRUE)
}

clean_names <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\\.-\\.", "-", x)
  x <- gsub("\\s*-\\s*", "-", x)
  x <- gsub("[–—−]", "-", x)
  toupper(x)
}

country_of <- function(x) {
  sub("-\\d+$", "", x)
}

extract_id <- function(x) {
  as.integer(sub(".*-(\\d+)$", "\\1", x))
}

canon_country <- function(x) {
  x <- clean_names(x)
  gsub("[^A-Z0-9]", "", x)
}

as_numeric_matrix <- function(x) {
  mat <- as.matrix(x)
  storage.mode(mat) <- "double"
  mat
}

read_w_matrix <- function(path = INPUT$w, sheet = 1) {
  require_packages("openxlsx")
  df <- openxlsx::read.xlsx(path, sheet = sheet, colNames = TRUE, check.names = FALSE)
  if (ncol(df) < 2) {
    stop("W matrix must include row labels in the first column.", call. = FALSE)
  }

  rn <- clean_names(df[[1]])
  mat <- as_numeric_matrix(df[, -1, drop = FALSE])
  colnames(mat) <- clean_names(names(df)[-1])
  rownames(mat) <- rn
  mat
}

read_unizar_a <- function(path = INPUT$data_unizar) {
  require_packages("openxlsx")
  as_numeric_matrix(openxlsx::read.xlsx(path, sheet = SHEETS$unizar_a, colNames = FALSE))
}

read_x_cge <- function(path = INPUT$x_cge) {
  x <- read.csv(path, header = FALSE)
  as.numeric(x[[1]])
}

read_unizar_countries <- function(path = INPUT$data_unizar) {
  require_packages("openxlsx")
  countries <- openxlsx::read.xlsx(path, sheet = SHEETS$unizar_countries, colNames = FALSE)
  canon_country(unlist(countries, use.names = FALSE))
}

read_unizar_sectors <- function(path = INPUT$data_unizar) {
  require_packages("openxlsx")
  sectors <- openxlsx::read.xlsx(path, sheet = "sector list", colNames = FALSE)
  as.character(unlist(sectors, use.names = FALSE))
}

read_correspondence <- function(path = INPUT$correspondence) {
  require_packages("openxlsx")
  mapping <- openxlsx::read.xlsx(path, sheet = SHEETS$correspondence, check.names = FALSE)
  required <- c("code_za", "code_wi")
  missing <- setdiff(required, names(mapping))
  if (length(missing) > 0) {
    stop("Correspondence file is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  mapping[required] <- lapply(mapping[required], function(x) as.character(as.integer(x)))
  mapping
}

parse_w_labels <- function(labels) {
  labels <- clean_names(labels)
  data.frame(
    country = canon_country(country_of(labels)),
    sector = extract_id(labels),
    label = labels,
    stringsAsFactors = FALSE
  )
}

validate_w_structure <- function(W) {
  row_info <- parse_w_labels(rownames(W))
  col_info <- parse_w_labels(colnames(W))
  countries_r <- unique(row_info$country)
  countries_c <- unique(col_info$country)

  if (!setequal(countries_r, countries_c)) {
    stop("Countries in W rows and columns do not match.", call. = FALSE)
  }

  for (country in countries_r) {
    row_ids <- sort(row_info$sector[row_info$country == country])
    col_ids <- sort(col_info$sector[col_info$country == country])
    if (!identical(row_ids, seq_len(N_SECTORS_WILIAM))) {
      stop("Rows for country ", country, " do not contain sectors 1..62.", call. = FALSE)
    }
    if (!identical(col_ids, seq_len(N_SECTORS_WILIAM))) {
      stop("Columns for country ", country, " do not contain sectors 1..62.", call. = FALSE)
    }
  }

  invisible(TRUE)
}

normalize_w_global <- function(W) {
  validate_w_structure(W)

  row_info <- parse_w_labels(rownames(W))
  col_info <- parse_w_labels(colnames(W))
  countries <- unique(row_info$country)
  idx_rows <- setNames(lapply(countries, function(country) which(row_info$country == country)), countries)
  idx_cols <- setNames(lapply(countries, function(country) which(col_info$country == country)), countries)

  W_norm <- W

  for (country_r in countries) {
    iR <- idx_rows[[country_r]]
    idsR <- row_info$sector[iR]

    for (country_c in countries) {
      iC <- idx_cols[[country_c]]
      idsC <- col_info$sector[iC]

      Wkl <- W[iR, iC, drop = FALSE]
      Wkl_new <- Wkl

      for (gR_name in names(SPECIAL_GROUPS)) {
        RR <- which(idsR %in% SPECIAL_GROUPS[[gR_name]])
        if (!length(RR)) next

        for (gC_name in names(SPECIAL_GROUPS)) {
          CC <- which(idsC %in% SPECIAL_GROUPS[[gC_name]])
          if (!length(CC)) next

          sub <- Wkl[RR, CC, drop = FALSE]
          s <- sum(sub, na.rm = TRUE)
          Wkl_new[RR, CC] <- if (is.finite(s) && s > 0) sub / s else 0
        }
      }

      RR_norm <- which(!idsR %in% ALL_SPECIAL_IDS)
      if (length(RR_norm)) {
        for (gC_name in names(SPECIAL_GROUPS)) {
          CC <- which(idsC %in% SPECIAL_GROUPS[[gC_name]])
          if (!length(CC)) next

          for (rr in RR_norm) {
            s <- sum(Wkl[rr, CC], na.rm = TRUE)
            Wkl_new[rr, CC] <- if (is.finite(s) && s > 0) Wkl[rr, CC] / s else 0
          }
        }
      }

      CC_norm <- which(!idsC %in% ALL_SPECIAL_IDS)
      if (length(CC_norm)) {
        for (gR_name in names(SPECIAL_GROUPS)) {
          RR <- which(idsR %in% SPECIAL_GROUPS[[gR_name]])
          if (!length(RR)) next

          for (cc in CC_norm) {
            s <- sum(Wkl[RR, cc], na.rm = TRUE)
            Wkl_new[RR, cc] <- if (is.finite(s) && s > 0) Wkl[RR, cc] / s else 0
          }
        }
      }

      RR_base <- which(!idsR %in% ALL_SPECIAL_IDS)
      CC_base <- which(!idsC %in% ALL_SPECIAL_IDS)
      if (length(RR_base) && length(CC_base)) {
        base_block <- Wkl[RR_base, CC_base, drop = FALSE]
        Wkl_new[RR_base, CC_base] <- ifelse(base_block != 0, 1, 0)
      }

      W_norm[iR, iC] <- Wkl_new
    }
  }

  W_norm
}

validate_w_rules <- function(W_original, W_normalized, eps = EPS) {
  validate_w_structure(W_original)
  validate_w_structure(W_normalized)

  row_info <- parse_w_labels(rownames(W_original))
  col_info <- parse_w_labels(colnames(W_original))
  countries <- unique(row_info$country)
  idx_rows <- setNames(lapply(countries, function(country) which(row_info$country == country)), countries)
  idx_cols <- setNames(lapply(countries, function(country) which(col_info$country == country)), countries)

  max_cell_deviation <- 0
  max_sum_deviation <- 0
  bad_cells <- 0
  bad_sums <- 0

  update_cell <- function(actual, expected) {
    diff <- abs(as.numeric(actual) - as.numeric(expected))
    diff[!is.finite(diff)] <- Inf
    list(max = max(diff, na.rm = TRUE), count = sum(diff > eps, na.rm = TRUE))
  }

  for (country_r in countries) {
    iR <- idx_rows[[country_r]]
    idsR <- row_info$sector[iR]

    for (country_c in countries) {
      iC <- idx_cols[[country_c]]
      idsC <- col_info$sector[iC]

      W0 <- W_original[iR, iC, drop = FALSE]
      W1 <- W_normalized[iR, iC, drop = FALSE]

      RR_base <- which(!idsR %in% ALL_SPECIAL_IDS)
      CC_base <- which(!idsC %in% ALL_SPECIAL_IDS)
      if (length(RR_base) && length(CC_base)) {
        expected <- ifelse(W0[RR_base, CC_base, drop = FALSE] != 0, 1, 0)
        d <- update_cell(W1[RR_base, CC_base, drop = FALSE], expected)
        max_cell_deviation <- max(max_cell_deviation, d$max)
        bad_cells <- bad_cells + d$count
      }

      for (gR_name in names(SPECIAL_GROUPS)) {
        RR <- which(idsR %in% SPECIAL_GROUPS[[gR_name]])
        if (!length(RR)) next

        for (gC_name in names(SPECIAL_GROUPS)) {
          CC <- which(idsC %in% SPECIAL_GROUPS[[gC_name]])
          if (!length(CC)) next

          s0 <- sum(W0[RR, CC, drop = FALSE], na.rm = TRUE)
          expected <- if (is.finite(s0) && s0 > 0) W0[RR, CC, drop = FALSE] / s0 else 0
          d <- update_cell(W1[RR, CC, drop = FALSE], expected)
          max_cell_deviation <- max(max_cell_deviation, d$max)
          bad_cells <- bad_cells + d$count

          target_sum <- if (is.finite(s0) && s0 > 0) 1 else 0
          sum_dev <- abs(sum(W1[RR, CC, drop = FALSE], na.rm = TRUE) - target_sum)
          max_sum_deviation <- max(max_sum_deviation, sum_dev)
          bad_sums <- bad_sums + as.integer(sum_dev > eps)
        }
      }

      RR_norm <- which(!idsR %in% ALL_SPECIAL_IDS)
      if (length(RR_norm)) {
        for (gC_name in names(SPECIAL_GROUPS)) {
          CC <- which(idsC %in% SPECIAL_GROUPS[[gC_name]])
          if (!length(CC)) next

          for (rr in RR_norm) {
            s0 <- sum(W0[rr, CC], na.rm = TRUE)
            expected <- if (is.finite(s0) && s0 > 0) W0[rr, CC] / s0 else rep(0, length(CC))
            d <- update_cell(W1[rr, CC], expected)
            max_cell_deviation <- max(max_cell_deviation, d$max)
            bad_cells <- bad_cells + d$count

            target_sum <- if (is.finite(s0) && s0 > 0) 1 else 0
            sum_dev <- abs(sum(W1[rr, CC], na.rm = TRUE) - target_sum)
            max_sum_deviation <- max(max_sum_deviation, sum_dev)
            bad_sums <- bad_sums + as.integer(sum_dev > eps)
          }
        }
      }

      CC_norm <- which(!idsC %in% ALL_SPECIAL_IDS)
      if (length(CC_norm)) {
        for (gR_name in names(SPECIAL_GROUPS)) {
          RR <- which(idsR %in% SPECIAL_GROUPS[[gR_name]])
          if (!length(RR)) next

          for (cc in CC_norm) {
            s0 <- sum(W0[RR, cc], na.rm = TRUE)
            expected <- if (is.finite(s0) && s0 > 0) W0[RR, cc] / s0 else rep(0, length(RR))
            d <- update_cell(W1[RR, cc], expected)
            max_cell_deviation <- max(max_cell_deviation, d$max)
            bad_cells <- bad_cells + d$count

            target_sum <- if (is.finite(s0) && s0 > 0) 1 else 0
            sum_dev <- abs(sum(W1[RR, cc], na.rm = TRUE) - target_sum)
            max_sum_deviation <- max(max_sum_deviation, sum_dev)
            bad_sums <- bad_sums + as.integer(sum_dev > eps)
          }
        }
      }
    }
  }

  data.frame(
    max_cell_deviation = max_cell_deviation,
    max_sum_deviation = max_sum_deviation,
    bad_cells = bad_cells,
    bad_sums = bad_sums,
    stringsAsFactors = FALSE
  )
}

build_io_unizar <- function(A, X) {
  if (length(X) != ncol(A)) {
    stop("X_CGE length does not match the number of UNIZAR columns.", call. = FALSE)
  }
  sweep(A, 2, X, `*`)
}

build_unizar_labels <- function(countries, n_sectors = N_SECTORS_UNIZAR) {
  paste0(rep(countries, each = n_sectors), "_", rep(seq_len(n_sectors), times = length(countries)))
}

build_unizar_index <- function(countries, n_sectors = N_SECTORS_UNIZAR) {
  data.frame(
    country = rep(countries, each = n_sectors),
    sector = rep(seq_len(n_sectors), times = length(countries)),
    label = build_unizar_labels(countries, n_sectors),
    stringsAsFactors = FALSE
  )
}

build_io_wiliam_matrix <- function(W_normalized, IO_unizar, countries_unizar, correspondence) {
  row_info <- parse_w_labels(rownames(W_normalized))
  col_info <- parse_w_labels(colnames(W_normalized))

  sector_map <- setNames(correspondence$code_za, correspondence$code_wi)
  row_parent_sector <- sector_map[as.character(row_info$sector)]
  col_parent_sector <- sector_map[as.character(col_info$sector)]

  if (anyNA(row_parent_sector) || anyNA(col_parent_sector)) {
    missing_wi <- sort(unique(c(
      row_info$sector[is.na(row_parent_sector)],
      col_info$sector[is.na(col_parent_sector)]
    )))
    stop("Correspondence file does not map WILIAM sectors: ", paste(missing_wi, collapse = ", "), call. = FALSE)
  }

  unizar_labels <- build_unizar_labels(countries_unizar)
  if (!identical(dim(IO_unizar), c(length(unizar_labels), length(unizar_labels)))) {
    stop("IO_unizar dimensions do not match country_count * 48.", call. = FALSE)
  }
  rownames(IO_unizar) <- unizar_labels
  colnames(IO_unizar) <- unizar_labels

  row_parent_label <- paste0(row_info$country, "_", row_parent_sector)
  col_parent_label <- paste0(col_info$country, "_", col_parent_sector)

  row_idx <- match(row_parent_label, rownames(IO_unizar))
  col_idx <- match(col_parent_label, colnames(IO_unizar))

  if (anyNA(row_idx) || anyNA(col_idx)) {
    stop("Could not match WILIAM countries/sectors to UNIZAR labels.", call. = FALSE)
  }

  output <- W_normalized * IO_unizar[row_idx, col_idx, drop = FALSE]
  rownames(output) <- paste0(row_info$country, "_", row_info$sector)
  colnames(output) <- paste0(col_info$country, "_", col_info$sector)
  output
}

max_abs_diff <- function(actual, expected) {
  max(abs(as.numeric(actual) - as.numeric(expected)), na.rm = TRUE)
}

max_rel_diff <- function(actual, expected) {
  diff <- abs(as.numeric(actual) - as.numeric(expected))
  scale <- pmax(1, abs(as.numeric(expected)))
  max(diff / scale, na.rm = TRUE)
}

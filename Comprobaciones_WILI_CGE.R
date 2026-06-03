suppressPackageStartupMessages({
  library(openxlsx)
})

clean_names <- function(x) {
  x <- trimws(x)
  x <- gsub("\\.-\\.", "-", x)
  x <- gsub("\\s*-\\s*", "-", x)
  x <- gsub("[–—−]", "-", x)
  toupper(x)
}

country_of <- function(x) sub("-\\d+$", "", x)
extract_id <- function(x) as.integer(sub(".*-(\\d+)$", "\\1", x))
canon_country <- function(x) {
  x <- clean_names(x)
  gsub("[^A-Z0-9]", "", x)
}

read_W_matrix <- function(path = "W.xlsx", sheet = 1) {
  df <- openxlsx::read.xlsx(path, sheet = sheet, colNames = TRUE)
  first_col <- df[[1]]

  if (is.character(first_col)) {
    rn <- clean_names(first_col)
    mat <- as.matrix(df[, -1, drop = FALSE])
    storage.mode(mat) <- "double"
    colnames(mat) <- clean_names(colnames(df)[-1])
    rownames(mat) <- rn
    return(mat)
  }

  mat <- as.matrix(df)
  storage.mode(mat) <- "double"
  if (is.null(rownames(mat))) {
    stop("El Excel debe tener la primera columna con nombres tipo 'PAIS-<id>'.")
  }
  rownames(mat) <- clean_names(rownames(mat))
  colnames(mat) <- clean_names(colnames(mat))
  mat
}

SPECIAL_GROUPS <- list(
  G6_21 = c(6, 21),
  G9_17 = 9:17,
  G47_49 = 47:49,
  G50_51 = 50:51,
  G58_62 = 58:62
)
ALL_SPECIAL_IDS <- sort(unique(unlist(SPECIAL_GROUPS)))

safe_sum <- function(x) sum(x, na.rm = TRUE)

add_issue <- function(issues, rule, country_row, country_col, row_label, col_label, expected, observed, detail) {
  issues[[length(issues) + 1L]] <- data.frame(
    rule = rule,
    country_row = country_row,
    country_col = country_col,
    row_label = row_label,
    col_label = col_label,
    expected = expected,
    observed = observed,
    detail = detail,
    stringsAsFactors = FALSE
  )
  issues
}

summarize_negatives <- function(mat, source_name) {
  idx <- which(mat < 0, arr.ind = TRUE)
  if (!nrow(idx)) {
    return(data.frame(
      source = character(),
      row_label = character(),
      col_label = character(),
      value = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    source = source_name,
    row_label = rownames(mat)[idx[, "row"]],
    col_label = colnames(mat)[idx[, "col"]],
    value = mat[idx],
    stringsAsFactors = FALSE
  )
}

check_normalized_matrix <- function(W_original, W_normalized, idxWr_by, idxWc_by, id_r, id_c, eps = 1e-8) {
  issues <- list()
  countries <- names(idxWr_by)

  for (country_row in countries) {
    iR <- idxWr_by[[country_row]]
    idsR <- id_r[iR]

    for (country_col in countries) {
      iC <- idxWc_by[[country_col]]
      idsC <- id_c[iC]
      Wkl <- W_original[iR, iC, drop = FALSE]
      Wkl_norm <- W_normalized[iR, iC, drop = FALSE]

      for (group_row_name in names(SPECIAL_GROUPS)) {
        RR <- which(idsR %in% SPECIAL_GROUPS[[group_row_name]])
        if (!length(RR)) {
          next
        }

        for (group_col_name in names(SPECIAL_GROUPS)) {
          CC <- which(idsC %in% SPECIAL_GROUPS[[group_col_name]])
          if (!length(CC)) {
            next
          }

          original_sum <- safe_sum(Wkl[RR, CC, drop = FALSE])
          observed_sum <- safe_sum(Wkl_norm[RR, CC, drop = FALSE])
          expected_sum <- if (is.finite(original_sum) && original_sum > 0) 1 else 0

          if (abs(observed_sum - expected_sum) > eps) {
            issues <- add_issue(
              issues,
              "special_special",
              country_row,
              country_col,
              group_row_name,
              group_col_name,
              expected_sum,
              observed_sum,
              sprintf("Suma del subbloque %s x %s", group_row_name, group_col_name)
            )
          }
        }
      }

      RR_norm <- which(!idsR %in% ALL_SPECIAL_IDS)
      for (rr in RR_norm) {
        for (group_col_name in names(SPECIAL_GROUPS)) {
          CC <- which(idsC %in% SPECIAL_GROUPS[[group_col_name]])
          if (!length(CC)) {
            next
          }

          original_sum <- safe_sum(Wkl[rr, CC])
          observed_sum <- safe_sum(Wkl_norm[rr, CC])
          expected_sum <- if (is.finite(original_sum) && original_sum > 0) 1 else 0

          if (abs(observed_sum - expected_sum) > eps) {
            issues <- add_issue(
              issues,
              "normal_special",
              country_row,
              country_col,
              rownames(Wkl)[rr],
              group_col_name,
              expected_sum,
              observed_sum,
              "La suma por fila sobre el grupo especial no es la esperada"
            )
          }
        }
      }

      CC_norm <- which(!idsC %in% ALL_SPECIAL_IDS)
      for (cc in CC_norm) {
        for (group_row_name in names(SPECIAL_GROUPS)) {
          RR <- which(idsR %in% SPECIAL_GROUPS[[group_row_name]])
          if (!length(RR)) {
            next
          }

          original_sum <- safe_sum(Wkl[RR, cc])
          observed_sum <- safe_sum(Wkl_norm[RR, cc])
          expected_sum <- if (is.finite(original_sum) && original_sum > 0) 1 else 0

          if (abs(observed_sum - expected_sum) > eps) {
            issues <- add_issue(
              issues,
              "special_normal",
              country_row,
              country_col,
              group_row_name,
              colnames(Wkl)[cc],
              expected_sum,
              observed_sum,
              "La suma por columna sobre el grupo especial no es la esperada"
            )
          }
        }
      }

      RR_base <- which(!idsR %in% ALL_SPECIAL_IDS)
      CC_base <- which(!idsC %in% ALL_SPECIAL_IDS)
      if (length(RR_base) && length(CC_base)) {
        expected_block <- ifelse(Wkl[RR_base, CC_base, drop = FALSE] != 0, 1, 0)
        observed_block <- Wkl_norm[RR_base, CC_base, drop = FALSE]
        mismatch <- which(abs(observed_block - expected_block) > eps, arr.ind = TRUE)

        if (nrow(mismatch)) {
          for (ii in seq_len(nrow(mismatch))) {
            r_local <- mismatch[ii, "row"]
            c_local <- mismatch[ii, "col"]
            issues <- add_issue(
              issues,
              "normal_normal",
              country_row,
              country_col,
              rownames(observed_block)[r_local],
              colnames(observed_block)[c_local],
              expected_block[r_local, c_local],
              observed_block[r_local, c_local],
              "Las celdas normales deben quedar en 0 o 1"
            )
          }
        }
      }
    }
  }

  if (!length(issues)) {
    return(data.frame(
      rule = character(),
      country_row = character(),
      country_col = character(),
      row_label = character(),
      col_label = character(),
      expected = numeric(),
      observed = numeric(),
      detail = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, issues)
}

W0 <- read_W_matrix("W.xlsx")
W1 <- read_W_matrix("W_normalizada.xlsx")

stopifnot(identical(dim(W0), dim(W1)))
stopifnot(identical(clean_names(rownames(W0)), clean_names(rownames(W1))))
stopifnot(identical(clean_names(colnames(W0)), clean_names(colnames(W1))))

rn <- clean_names(rownames(W0))
cn <- clean_names(colnames(W0))
ct_r <- canon_country(country_of(rn))
ct_c <- canon_country(country_of(cn))
id_r <- extract_id(rn)
id_c <- extract_id(cn)
countries <- unique(ct_r)

if (!identical(sort(unique(ct_r)), sort(unique(ct_c)))) {
  stop("Los paises de filas y columnas no coinciden entre si.")
}

idxWr_by <- setNames(lapply(countries, function(cc) which(ct_r == cc)), countries)
idxWc_by <- setNames(lapply(countries, function(cc) which(ct_c == cc)), countries)

issues <- check_normalized_matrix(W0, W1, idxWr_by, idxWc_by, id_r, id_c)
negatives_W0 <- summarize_negatives(W0, "W.xlsx")
negatives_W1 <- summarize_negatives(W1, "W_normalizada.xlsx")

cat("Dimension W0:", dim(W0)[1], "x", dim(W0)[2], "\n")
cat("Dimension W1:", dim(W1)[1], "x", dim(W1)[2], "\n")
cat("Negativos en W.xlsx:", nrow(negatives_W0), "\n")
cat("Negativos en W_normalizada.xlsx:", nrow(negatives_W1), "\n")
cat("Incidencias de normalizacion:", nrow(issues), "\n")

if (!nrow(issues)) {
  cat("Todos los chequeos de normalizacion han pasado correctamente.\n")
} else {
  cat("Se han detectado incidencias. Revisa 'comprobaciones_WILI_CGE.xlsx'.\n")
}

openxlsx::write.xlsx(
  list(
    resumen = data.frame(
      metrica = c(
        "filas_W0",
        "columnas_W0",
        "filas_W1",
        "columnas_W1",
        "negativos_W0",
        "negativos_W1",
        "incidencias_normalizacion"
      ),
      valor = c(
        nrow(W0),
        ncol(W0),
        nrow(W1),
        ncol(W1),
        nrow(negatives_W0),
        nrow(negatives_W1),
        nrow(issues)
      )
    ),
    negativos_W0 = negatives_W0,
    negativos_W1 = negatives_W1,
    incidencias = issues
  ),
  file = "comprobaciones_WILI_CGE.xlsx",
  overwrite = TRUE
)
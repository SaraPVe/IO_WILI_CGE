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

get_group_name <- function(id) {
  matches <- names(Filter(function(group_ids) id %in% group_ids, SPECIAL_GROUPS))
  if (!length(matches)) {
    return(NA_character_)
  }
  matches[[1]]
}

get_rule_type <- function(row_id, col_id) {
  row_special <- row_id %in% ALL_SPECIAL_IDS
  col_special <- col_id %in% ALL_SPECIAL_IDS

  if (!row_special && !col_special) {
    return("normal_normal")
  }
  if (!row_special && col_special) {
    return("normal_special")
  }
  if (row_special && !col_special) {
    return("special_normal")
  }
  "special_special"
}

impact_message <- function(rule_type) {
  switch(
    rule_type,
    normal_normal = "El signo no altera pesos relativos: la regla solo convierte distinto de cero en 1.",
    normal_special = "Puede alterar el reparto horizontal del grupo especial porque entra en la suma de la fila sobre el grupo.",
    special_normal = "Puede alterar el reparto vertical del grupo especial porque entra en la suma de la columna sobre el grupo.",
    special_special = "Puede alterar el reparto del subbloque especial porque entra en la suma usada para normalizar todo el bloque."
  )
}

block_descriptor <- function(W0, W1, row_idx, col_idx, row_id, col_id) {
  rule_type <- get_rule_type(row_id, col_id)
  row_ids <- extract_id(rownames(W0))
  col_ids <- extract_id(colnames(W0))
  row_country <- country_of(rownames(W0)[row_idx])
  col_country <- country_of(colnames(W0)[col_idx])
  same_row_country <- which(country_of(rownames(W0)) == row_country)
  same_col_country <- which(country_of(colnames(W0)) == col_country)

  if (rule_type == "normal_normal") {
    affected_rows <- row_idx
    affected_cols <- col_idx
    denom <- NA_real_
  } else if (rule_type == "normal_special") {
    col_group <- get_group_name(col_id)
    affected_rows <- row_idx
    affected_cols <- same_col_country[col_ids[same_col_country] %in% SPECIAL_GROUPS[[col_group]]]
    denom <- sum(W0[affected_rows, affected_cols, drop = FALSE], na.rm = TRUE)
  } else if (rule_type == "special_normal") {
    row_group <- get_group_name(row_id)
    affected_rows <- same_row_country[row_ids[same_row_country] %in% SPECIAL_GROUPS[[row_group]]]
    affected_cols <- col_idx
    denom <- sum(W0[affected_rows, affected_cols, drop = FALSE], na.rm = TRUE)
  } else {
    row_group <- get_group_name(row_id)
    col_group <- get_group_name(col_id)
    affected_rows <- same_row_country[row_ids[same_row_country] %in% SPECIAL_GROUPS[[row_group]]]
    affected_cols <- same_col_country[col_ids[same_col_country] %in% SPECIAL_GROUPS[[col_group]]]
    denom <- sum(W0[affected_rows, affected_cols, drop = FALSE], na.rm = TRUE)
  }

  list(
    rule_type = rule_type,
    denominator = denom,
    normalized_value = W1[row_idx, col_idx],
    normalized_block_has_negative = any(W1[affected_rows, affected_cols, drop = FALSE] < 0, na.rm = TRUE),
    impact_message = impact_message(rule_type)
  )
}

W0 <- read_W_matrix("W.xlsx")
W1 <- read_W_matrix("W_normalizada.xlsx")

neg_idx <- which(W0 < 0, arr.ind = TRUE)
diagnostic_rows <- vector("list", nrow(neg_idx))

io_wiliam_available <- file.exists("Data_CT/IO_wiliam.RData")
if (io_wiliam_available) {
  load("Data_CT/IO_wiliam.RData")
}

for (ii in seq_len(nrow(neg_idx))) {
  row_idx <- neg_idx[ii, "row"]
  col_idx <- neg_idx[ii, "col"]
  row_label <- rownames(W0)[row_idx]
  col_label <- colnames(W0)[col_idx]
  row_id <- extract_id(row_label)
  col_id <- extract_id(col_label)
  block_info <- block_descriptor(W0, W1, row_idx, col_idx, row_id, col_id)

  io_value <- NA_real_
  io_negative <- NA
  if (io_wiliam_available) {
    row_key <- gsub("-", "_", row_label)
    col_key <- gsub("-", "_", col_label)
    row_match <- match(row_key, IO_wiliam[[1]])
    col_match <- match(col_key, names(IO_wiliam))
    if (!is.na(row_match) && !is.na(col_match)) {
      io_value <- IO_wiliam[row_match, col_match][[1]]
      io_negative <- isTRUE(io_value < 0)
    }
  }

  diagnostic_rows[[ii]] <- data.frame(
    row_label = row_label,
    col_label = col_label,
    row_country = country_of(row_label),
    col_country = country_of(col_label),
    row_id = row_id,
    col_id = col_id,
    raw_value_W = W0[row_idx, col_idx],
    rule_type = block_info$rule_type,
    denominator_used = block_info$denominator,
    value_in_W_normalizada = block_info$normalized_value,
    negative_persists_in_W_normalizada = isTRUE(block_info$normalized_value < 0),
    affected_block_has_negative_in_W_normalizada = block_info$normalized_block_has_negative,
    value_in_IO_wiliam = io_value,
    negative_persists_in_IO_wiliam = io_negative,
    impact_message = block_info$impact_message,
    stringsAsFactors = FALSE
  )
}

diagnostic <- if (length(diagnostic_rows)) {
  do.call(rbind, diagnostic_rows)
} else {
  data.frame(
    row_label = character(),
    col_label = character(),
    row_country = character(),
    col_country = character(),
    row_id = integer(),
    col_id = integer(),
    raw_value_W = numeric(),
    rule_type = character(),
    denominator_used = numeric(),
    value_in_W_normalizada = numeric(),
    negative_persists_in_W_normalizada = logical(),
    affected_block_has_negative_in_W_normalizada = logical(),
    value_in_IO_wiliam = numeric(),
    negative_persists_in_IO_wiliam = logical(),
    impact_message = character(),
    stringsAsFactors = FALSE
  )
}

summary_df <- data.frame(
  metrica = c(
    "negativos_en_W",
    "negativos_en_W_normalizada",
    "negativos_en_IO_wiliam"
  ),
  valor = c(
    nrow(diagnostic),
    sum(W1 < 0, na.rm = TRUE),
    if (io_wiliam_available) sum(as.matrix(IO_wiliam[, -1, drop = FALSE]) < 0, na.rm = TRUE) else NA_real_
  )
)

cat("Negativos encontrados en W.xlsx:", nrow(diagnostic), "\n")
if (nrow(diagnostic)) {
  print(diagnostic)
}

openxlsx::write.xlsx(
  list(
    resumen = summary_df,
    diagnostico = diagnostic
  ),
  file = "diagnostico_negativos_W.xlsx",
  overwrite = TRUE
)
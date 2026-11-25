# A/X UNIZAR 48 -> Z/X 62 -> TC / BIS / BISO intermedios.
# Tambien conserva los modos anteriores de entrada completa.
# Archivo independiente. Solo requiere R y openxlsx; no ejecuta otros scripts.
# Ejecutar con --ayuda para ver entradas y opciones.

# Clasificacion WILIAM (35 paises, 62 sectores, 6 usos finales).
CLASE <- list(countries = c("AUSTRIA", "BELGIUM", "BULGARIA", "CROATIA",
"CYPRUS", "CZECHREPUBLIC", "DENMARK", "ESTONIA", "FINLAND", "FRANCE",
"GERMANY", "GREECE", "HUNGARY", "IRELAND", "ITALY", "LATVIA",
"LITHUANIA", "LUXEMBOURG", "MALTA", "NETHERLANDS", "POLAND",
"PORTUGAL", "ROMANIA", "SLOVAKIA", "SLOVENIA", "SPAIN", "SWEDEN",
"UK", "CHINA", "EASOC", "INDIA", "LATAM", "RUSSIA", "USMCA",
"LROW"), codes = c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE",
"DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA",
"LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK",
"SVN", "ESP", "SWE", "UK", "China", "EASOC", "India", "LATAM",
"Russia", "USMCA", "LROW"), sectors = c("CROPS", "ANIMALS", "FORESTRY",
"FISHNG", "MINING_COAL", "EXTRACTION_OIL", "EXTRACTION_GAS",
"EXTRACTION_OTHER_GAS", "MINING_AND_MANUFACTURING_URANIUM_THORIUM",
"MINING_AND_MANUFACTURING_IRON", "MINING_AND_MANUFACTURING_COPPER",
"MINING_AND_MANUFACTURING_NICKEL", "MINING_AND_MANUFACTURING_ALUMINIUM",
"MINING_AND_MANUFACTURING_PRECIOUS_METALS", "MINING_AND_MANUFACTURING_LEAD_ZINC_TIN",
"MINING_AND_MANUFACTURING_OTHER_METALS", "MINING_NON_METALS",
"MANUFACTURE_FOOD", "MANUFACTURE_WOOD", "COKE", "REFINING", "MANUFACTURE_CHEMICAL",
"MANUFACTURE_PLASTIC", "MANUFACTURE_OTHER_NON_METAL", "HYDROGEN_PRODUCTION",
"MANUFACTURE_METAL_PRODUCTS", "MANUFACTURE_ELECTRONICS", "MANUFACTURE_ELECTRICAL_EQUIPMENT",
"MANUFACTURE_MACHINERY", "MANUFACTURE_VEHICLES", "MANUFACTURE_OTHER",
"ELECTRICITY_COAL", "ELECTRICITY_GAS", "ELECTRICITY_NUCLEAR",
"ELECTRICITY_HYDRO", "ELECTRICITY_WIND", "ELECTRICITY_OIL", "ELECTRICITY_SOLAR_PV",
"ELECTRICITY_SOLAR_THERMAL", "ELECTRICITY_OTHER", "DISTRIBUTION_ELECTRICITY",
"DISTRIBUTION_GAS", "STEAM_HOT_WATER", "WASTE_MANAGEMENT", "CONSTRUCTION",
"TRADE_REPAIR_VEHICLES", "TRANSPORT_RAIL", "TRANSPORT_OTHER_LAND",
"TRANSPORT_PIPELINE", "TRANSPORT_SEA", "TRANSPORT_INLAND_WATER",
"TRANSPORT_AIR", "ACCOMMODATION", "TELECOMMUNICATIONS", "FINANCE",
"REAL_ESTATE", "OTHER_SERVICES", "PUBLIC_ADMINISTRATION", "EDUCATION",
"HEALTH", "ENTERTAIMENT", "PRIVATE_HOUSEHOLDS"), final = c("HOUSEHOLDS_FINAL_CONSUMPTION_EXPENDITURE",
"NON-PROFIT_INSTITUTIONS_SERVING_HOUSEHOLDS", "GENERAL_GOVERNMENT_FINAL_CONSUMPTION",
"GROSS_FIXED_CAPITAL_FORMATION", "CHANGE_IN_INVENTORIES_AND_VALUABLES",
"DIRECT_PURCHASES_ABROAD"), regions = c("AUSTRIA", "BELGIUM",
"BULGARIA", "CROATIA", "CYPRUS", "CZECH_REPUBLIC", "DENMARK",
"ESTONIA", "FINLAND", "FRANCE", "GERMANY", "GREECE", "HUNGARY",
"IRELAND", "ITALY", "LATVIA", "LITHUANIA", "LUXEMBOURG", "MALTA",
"NETHERLANDS", "POLAND", "PORTUGAL", "ROMANIA", "SLOVAKIA", "SLOVENIA",
"SPAIN", "SWEDEN", "UK", "CHINA", "EASOC", "INDIA", "LATAM",
"RUSSIA", "USMCA", "LROW"), sector_ranges = c("CROPS", "ANIMALS",
"FORESTRY", "FISHING", "MINING_COAL", "EXTRACTION_OIL", "EXTRACTION_GAS",
"EXTRACTION_OTHER_GAS", "MINING_URANIUM_THORIUM", "MINING_IRON",
"MINING_COPPER", "MINING_NICKEL", "MINING_ALUMINIUM", "MINING_PRECIOUS_METALS",
"MINING_LEAD_ZINC_TIN", "MINING_OTHER_METALS", "MINING_NON_METALS",
"MANUFACTURE_FOOD", "MANUFACTURE_WOOD", "COKE", "REFINING", "MANUFACTURE_CHEMICAL",
"MANUFACTURE_PLASTIC", "MANUFACTURE_OTHER_NON_METAL", "MANUFACTURE_BASIC_METALS",
"MANUFACTURE_METAL_PRODUCTS", "MANUFACTURE_ELECTRONICS", "MANUFACTURE_ELECTRICAL_EQUIPMENT",
"MANUFACTURE_MACHINERY", "MANUFACTURE_VEHICLES", "MANUFACTURE_OTHER",
"ELECTRICITY_COAL", "ELECTRICITY_GAS", "ELECTRICITY_NUCLEAR",
"ELECTRICITY_HYDRO", "ELECTRICITY_WIND", "ELECTRICITY_OIL", "ELECTRICITY_SOLAR_PV",
"ELECTRICITY_SOLAR_THERMAL", "ELECTRICITY_OTHER", "DISTRIBUTION_ELECTRICITY",
"DISTRIBUTION_GAS", "STEAM_HOT_WATER", "WASTE_MANAGEMENT", "CONSTRUCTION",
"TRADE_REPAIR_VEHICLES", "TRANSPORT_RAIL", "TRANSPORT_OTHER_LAND",
"TRANSPORT_PIPELINE", "TRANSPORT_SEA", "TRANSPORT_INLAND_WATER",
"TRANSPORT_AIR", "ACCOMMODATION", "TELECOMMUNICATIONS", "FINANCE",
"REAL_ESTATE", "OTHER_SERVICES", "PUBLIC_ADMINISTRATION", "EDUCATION",
"HEALTH", "ENTERTAIMENT", "PRIVATE_HOUSEHOLDS"))


# 1. Entrada completa y configuracion ---------------------------------------
canon <- function(x) gsub("[^A-Z0-9]", "", toupper(trimws(as.character(x))))
numeric_matrix <- function(x) {
  m <- as.matrix(x)
  suppressWarnings(storage.mode(m) <- "double")
  if (any(!is.finite(m))) stop("Hay valores ausentes, texto o valores no finitos en la tabla contable.")
  m
}
read_object <- function(path, sheet = NULL) {
  if (!file.exists(path)) stop("No existe: ", path)
  ext <- tolower(tools::file_ext(path))
  if (ext == "rds") return(readRDS(path))
  if (ext %in% c("rdata", "rda")) {
    e <- new.env(parent = emptyenv()); load(path, e)
    if (!exists("data_origin", e, inherits = FALSE)) stop("El RData debe contener data_origin.")
    return(e$data_origin)
  }
  if (ext == "xlsx") {
    sheets <- openxlsx::getSheetNames(path)
    selected <- if (is.null(sheet)) { if ("IO_final" %in% sheets) "IO_final" else sheets[1] } else sheet
    if (!selected %in% sheets) stop("No existe la hoja: ", selected)
    probe <- openxlsx::read.xlsx(path, sheet = selected, rows = 1:6, cols = 1:3,
      colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
    double_header <- nrow(probe) >= 3 && identical(canon(probe[2, 1]), "PAIS") &&
      canon(probe[2, 2]) %in% c("SECTOR", "SECTORES")
    if (double_header) {
      raw <- openxlsx::read.xlsx(path, sheet = selected, cols = 1:2382,
        colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE)
      headers <- paste(canon(unlist(raw[1, -(1:2)], use.names = FALSE)),
        trimws(as.character(unlist(raw[2, -(1:2)], use.names = FALSE))), sep = "_")
      d <- raw[-(1:2), , drop = FALSE]
      names(d) <- c("Pais", "Sector", headers)
      attr(d, "note") <- paste("Entrada directa del Excel", basename(path), "hoja", selected,
        "con dos filas de cabecera. Se usan todos sus bloques contables, sin plantilla externa.")
    } else {
      d <- openxlsx::read.xlsx(path, sheet = selected, check.names = FALSE)
    }
    if ("PROCEDENCIA_IO" %in% sheets) {
      info <- openxlsx::read.xlsx(path, sheet = "PROCEDENCIA_IO")
      attr(d, "note") <- paste(info$Valor[info$Concepto %in% c("Nota_entrada", "Conversion")], collapse = " ")
    }
    return(d)
  }
  if (ext == "csv") return(read.csv(path, check.names = FALSE))
  stop("Formatos de IO completa admitidos: RData, RDS, XLSX y CSV.")
}
read_intermediate_io <- function(path, country_file) {
  if (!file.exists(path)) stop("No existe: ", path)
  ext <- tolower(tools::file_ext(path))
  d <- switch(ext, xlsx = openxlsx::read.xlsx(path, colNames = FALSE, check.names = FALSE),
    csv = read.csv(path, header = FALSE, check.names = FALSE), rds = readRDS(path),
    stop("La entrada intermedia admite XLSX, CSV o RDS con matriz de flujos 1680 x 1680."))
  keys <- paste(rep(CLASE$countries, each = 48), rep(1:48, 35), sep = "_")
  normalize_key <- function(x) {
    x <- gsub("\\.-\\.", "-", trimws(as.character(x)))
    paste(canon(sub("[-_][0-9]+$", "", x)),
      as.integer(sub(".*[-_]([0-9]+)$", "\\1", x)), sep = "_")
  }
  metadata <- character()
  if (identical(dim(d), c(1681L, 1681L))) {
    rows <- normalize_key(d[-1, 1]); cols <- normalize_key(unlist(d[1, -1], use.names = FALSE))
    m <- numeric_matrix(d[-1, -1])
  } else if (identical(dim(d), c(1680L, 1680L))) {
    m <- numeric_matrix(d)
    if (is.matrix(d) && !is.null(rownames(d)) && !is.null(colnames(d))) {
      rows <- normalize_key(rownames(d)); cols <- normalize_key(colnames(d))
    } else {
      if (!file.exists(country_file)) stop("La IO no tiene etiquetas; falta el archivo con country list: ", country_file)
      countries <- canon(unlist(openxlsx::read.xlsx(country_file, sheet = "country list", colNames = FALSE), use.names = FALSE))
      if (length(countries) != 35 || anyDuplicated(countries) || !setequal(countries, CLASE$countries)) stop("country list no coincide con los 35 paises.")
      rows <- cols <- paste(rep(countries, each = 48), rep(1:48, 35), sep = "_")
      metadata <- country_file
    }
  } else stop("La IO UNIZAR intermedia debe ser 1680 x 1680, o 1681 x 1681 con etiquetas.")
  if (anyNA(rows) || anyNA(cols) || anyDuplicated(rows) || anyDuplicated(cols) ||
      !setequal(rows, keys) || !setequal(cols, keys)) stop("Etiquetas de la IO UNIZAR invalidas.")
  list(values = m[match(keys, rows), match(keys, cols), drop = FALSE], ns = 48L,
    intermediate = TRUE, metadata_files = metadata, source = normalizePath(path),
    note = "Entrada: flujos intermedios UNIZAR de 48 sectores; no son coeficientes A.")
}
read_full_io <- function(path, sheet = NULL) {
  d <- read_object(path, sheet)
  if (!is.data.frame(d) || ncol(d) < 3) stop("Se espera una tabla completa con dos columnas iniciales: Pais y Sector.")
  note <- attr(d, "note")
  names(d)[1:2] <- c("Pais", "Sector")
  tax <- which(as.character(d$Sector) == "TAXES_LESS_SUBSIDIES_ON_PRODUCTS")
  if (length(tax) != 35) stop("La IO completa debe incluir 35 filas TAXES_LESS_SUBSIDIES_ON_PRODUCTS; una matriz Z sola no basta.")
  n <- min(tax) - 1L
  ns <- n / 35L
  if (!ns %in% c(48L, 62L)) stop("Se esperan 35 paises con 48 o 62 sectores por pais.")
  if (!identical(tax, n + seq_len(35)) || as.character(d$Sector[n + 36]) != "VALUE_ADDED") stop("El orden requerido es productos, 35 impuestos y VALUE_ADDED.")
  if (ncol(d) != 2 + n + 35 * 6) stop("Deben existir las columnas intermedias y 6 usos finales por pais.")
  countries <- vapply(seq_len(35), function(i) canon(d$Pais[(i - 1) * ns + 1]), character(1))
  if (!setequal(countries, CLASE$countries) || anyDuplicated(countries)) stop("Los paises de la IO no coinciden con la clasificacion WILIAM.")
  sectors <- as.character(d$Sector[seq_len(ns)])
  if (anyDuplicated(sectors) || !identical(as.character(d$Sector[seq_len(n)]), rep(sectors, 35))) stop("Orden de sectores inconsistente entre paises.")
  expected <- c(paste(rep(countries, each = ns), rep(sectors, 35), sep = "_"),
    paste(rep(countries, each = 6), rep(CLASE$final, 35), sep = "_"))
  actual <- names(d)[-(1:2)]
  # Los RData antiguos tienen sufijos numericos de importacion.
  if (!identical(actual, expected) && !identical(sub("[0-9]+$", "", actual), expected)) stop("Las cabeceras no coinciden con el orden pais-sector/uso.")
  if (!identical(canon(d$Pais[seq_len(n)]), rep(countries, each = ns))) stop("Paises desalineados en las filas de productos.")
  if (!identical(canon(d$Pais[tax]), countries)) stop("Las filas de impuestos no siguen el orden de paises.")
  m <- numeric_matrix(d[seq_len(n + 36), -(1:2)])
  country_order <- match(CLASE$countries, countries)
  sector_order <- if (ns == 62) match(CLASE$sectors,
    ifelse(sectors=="MANUFACTURE_BASIC_METALS","HYDROGEN_PRODUCTION",sectors)) else seq_len(ns)
  if (anyNA(sector_order)) stop("No se reconocen los 62 sectores WILIAM.")
  product_order <- unlist(lapply(country_order, function(i) (i - 1) * ns + sector_order))
  final_order <- unlist(lapply(country_order, function(i) n + (i - 1) * 6 + seq_len(6)))
  m <- m[c(product_order, n + country_order, n + 36), c(product_order, final_order), drop = FALSE]
  list(values = m, ns = as.integer(ns), note = note, source = normalizePath(path))
}
io_table <- function(values, sector_labels = CLASE$sectors) {
  countries <- rep(CLASE$countries, each = 62)
  sectors <- rep(sector_labels, 35)
  keys <- c(paste(countries, sectors, sep = "_"),
    paste(rep(CLASE$countries, each = 6), rep(CLASE$final, 35), sep = "_"))
  d <- data.frame(Pais = c(countries, CLASE$countries, "ZZZ"),
    Sector = c(sectors, rep("TAXES_LESS_SUBSIDIES_ON_PRODUCTS", 35), "VALUE_ADDED"),
    values, check.names = FALSE)
  names(d)[-(1:2)] <- keys
  d
}

# 2. Conversion de TODOS los bloques ---------------------------------------
convert_full_io <- function(origin, mapping = NULL, weights = NULL, reference = NULL) {
  if (origin$ns == 62L) return(list(values = origin$values,
    checks = data.frame(Control = "IO ya clasificada en 62 sectores: identidad", Valor = 0, Resultado = "BIEN"),
    policy = "Entrada completa ya WILIAM: se conservan todos sus valores contables."))
  if (is.null(mapping) || is.null(weights) || is.null(reference)) stop(
    "Para 48 -> 62 sectores hacen falta correspondencia, W y --referencia-completa. No se rellenan los bloques ausentes con valores de otra tabla.")
  if (reference$ns != 62L) stop("La referencia de reparto debe tener 62 sectores.")
  wi <- as.integer(mapping$code_wi); za <- as.integer(mapping$code_za)
  if (anyNA(wi) || anyNA(za) || anyDuplicated(wi) || !setequal(wi, 1:62) || !setequal(za, 1:48)) stop("Correspondencia invalida.")
  parent_sector <- za[match(1:62, wi)]
  parent <- rep((0:34) * 48, each = 62) + rep(parent_sector, 35)
  aggregate_both <- function(x) t(rowsum(t(rowsum(x, parent)), parent))
  sums <- aggregate_both(weights)
  counts <- tabulate(parent_sector, 48)
  split <- rep(counts[parent_sector] > 1L, 35)
  split_pairs <- outer(split, split, "|")
  expanded_sums <- sums[parent, parent]
  w <- matrix(0, 2170, 2170)
  active <- split_pairs & expanded_sums > 0
  w[active] <- weights[active] / expanded_sums[active]
  single <- !split_pairs
  # One-to-one mappings preserve the input flow even if reference W is zero.
  w[single] <- 1
  support <- outer(rep(counts == 1L, 35), rep(counts == 1L, 35), "&") | sums > 0
  weight_error <- max(abs(aggregate_both(w) - support))
  z <- w * origin$values[parent, parent]
  if (isTRUE(origin$intermediate)) {
    # Flujo original: convertir solo Z y completar con los bloques fijos
    # documentados. No se presentan estos ultimos como reconstruidos UNIZAR.
    values <- reference$values
    values[1:2170, 1:2170] <- z
    parent_expected <- origin$values * support
    z_error <- max(abs(aggregate_both(z) - parent_expected) / pmax(1, abs(parent_expected)))
    if (weight_error > 1e-10 || z_error > 1e-10) stop("Falla la conservacion de la conversion 48 -> 62.")
    stopifnot(identical(values[, 2171:2380], reference$values[, 2171:2380]),
      identical(values[2171:2206, ], reference$values[2171:2206, ]))
    lost <- origin$values[!support]
    return(list(values = values, checks = data.frame(
      Control = c("Conversion real 48 -> 62: conservacion pesos", "Conservacion Z donde hay soporte W",
        "Demanda final, impuestos y VA conservados de plantilla", "Pares UNIZAR sin soporte W", "Flujo UNIZAR omitido por falta de soporte W"),
      Valor = c(weight_error, z_error, 0, sum(lost != 0), sum(lost)),
      Resultado = c(rep("BIEN", 3), rep(if (any(lost != 0)) "REVISAR" else "BIEN", 2))),
      policy = paste("Primera etapa: Z UNIZAR de 1680 x 1680 (35 x 48) se convierte con W y correspondencia a Z de 2170 x 2170 (35 x 62).",
        "Segunda etapa: TC, BIS y BISO se calculan de ESA Z nueva, completada con demanda final, impuestos y VA de la plantilla fija.",
        "Tabla hibrida: los bloques no-Z se conservan, no se convierten desde UNIZAR ni se equilibran automaticamente.")))
  }
  # Demanda final: participaciones firmadas de la referencia, normalizadas
  # dentro del mismo padre, pais de origen y uso final/pais destino.
  ref_y <- reference$values[1:2170, 2171:2380, drop = FALSE]
  sy <- rowsum(ref_y, parent)
  wy <- matrix(0, 2170, 210)
  singleton_rows <- which(!split)
  wy[singleton_rows, ] <- 1
  split_rows <- which(split)
  denominators <- sy[parent[split_rows], , drop = FALSE]
  active_y <- abs(denominators) > 1e-12
  source_y <- origin$values[1:1680, 1681:1890, drop = FALSE]
  if (any(!active_y & abs(source_y[parent[split_rows], , drop = FALSE]) > 1e-12)) stop("Falta soporte en la referencia para repartir demanda final no nula.")
  tmp <- ref_y[split_rows, , drop = FALSE]
  tmp[active_y] <- tmp[active_y] / denominators[active_y]
  tmp[!active_y] <- 0
  wy[split_rows, ] <- tmp
  y <- wy * source_y[parent, , drop = FALSE]
  # Impuestos y VA: se distribuyen las cifras ORIGINALES segun las
  # participaciones de output de las actividades hijas. No se copian valores.
  ref_output <- colSums(reference$values[, 1:2170, drop = FALSE])
  if (any(ref_output < -1e-12)) stop("La referencia tiene output negativo; requiere una regla de reparto explicita.")
  output_sum <- as.numeric(rowsum(matrix(ref_output), parent))
  wc <- rep(1, 2170)
  active_c <- output_sum[parent] > 1e-12
  wc[split & active_c] <- ref_output[split & active_c] / output_sum[parent[split & active_c]]
  wc[split & !active_c] <- 0
  original_cost <- origin$values[1681:1716, 1:1680, drop = FALSE]
  if (any(abs(original_cost[, parent[split & !active_c], drop = FALSE]) > 1e-12)) stop("Falta soporte de output para repartir impuestos/VA no nulos.")
  cost <- sweep(original_cost[, parent, drop = FALSE], 2, wc, "*")
  values <- rbind(cbind(z, y), cbind(cost, origin$values[1681:1716, 1681:1890, drop = FALSE]))
  relative_error <- function(a, b) max(abs(a - b) / pmax(1, abs(b)))
  errors <- c(weight_error, relative_error(aggregate_both(z), origin$values[1:1680, 1:1680] * support),
    relative_error(rowsum(y, parent), source_y), relative_error(t(rowsum(t(cost), parent)), original_cost))
  if (any(!is.finite(errors)) || any(errors > 1e-10)) stop("Falla la conservacion por sectores padre en la conversion.")
  lost <- origin$values[1:1680, 1:1680][!support]
  checks <- data.frame(Control = c("Conservacion pesos Z", "Conservacion Z donde existe soporte W", "Conservacion demanda final", "Conservacion impuestos y VA",
    "Pares CGE sin soporte W", "Flujo CGE omitido por falta de soporte W"),
    Valor = c(errors, sum(lost != 0), sum(lost)),
    Resultado = c(rep("BIEN", 4), rep(if (any(lost != 0)) "REVISAR" else "BIEN", 2)))
  list(values = values, checks = checks, policy = paste(
    "Z: misma regla de pesos W por correspondencia del flujo anterior.",
    "Y: reparto por participaciones firmadas de demanda final de la referencia.",
    "Impuestos/VA: reparto por output de las actividades hijas en la referencia.",
    "La referencia aporta PESOS: todos los importes proceden de la IO de entrada."))
}
read_weights <- function(path) {
  d <- openxlsx::read.xlsx(path, check.names = FALSE)
  normalize_key <- function(x) {
    x <- gsub("[–—−]", "-", toupper(trimws(x)))
    x <- gsub("\\.-\\.", "-", x)
    x <- gsub("\\s*-\\s*", "-", x)
    country <- canon(sub("[-_][0-9]+$", "", x))
    sector <- as.integer(sub(".*[-_]([0-9]+)$", "\\1", x))
    paste(country, sector, sep = "_")
  }
  rows <- normalize_key(as.character(d[[1]])); cols <- normalize_key(names(d)[-1])
  expected <- paste(rep(CLASE$countries, each = 62), rep(1:62, 35), sep = "_")
  if (anyDuplicated(rows) || anyDuplicated(cols) || !setequal(rows, expected) || !setequal(cols, expected)) stop("Etiquetas de W invalidas.")
  numeric_matrix(d[match(expected, rows), match(expected, cols) + 1, drop = FALSE])
}

# 3. TC/BIS/BISO; calibracion experimental opcional de las shares -----------
calibrate_share_flows <- function(flows, policy = c("ninguna", "coque", "coque-niquel")) {
  policy <- match.arg(policy)
  rules <- data.frame(Origen = "LROW", Producto = "COKE", Destino = "NLD", Uso = "ANIMALS", Factor = 1.05)
  if (policy == "coque-niquel") rules <- rbind(rules, data.frame(
    Origen = c("CYP", "LTU", "LTU"), Producto = "MINING_AND_MANUFACTURING_NICKEL",
    Destino = c("CYP", "LTU", "LTU"),
    Uso = c("MANUFACTURE_METAL_PRODUCTS", "FORESTRY", "MANUFACTURE_MACHINERY"), Factor = 0))
  if (policy == "ninguna") return(list(values = flows, policy = policy, rules = rules[0, ],
    bis_changes = data.frame(), biso_changes = data.frame(),
    note = "Calibracion shares: ninguna; BIS/BISO proceden directamente de la IO segun la politica de negativos."))
  stopifnot(identical(dim(flows), c(2170L, 2380L)))
  before <- flows
  ri <- (match(rules$Origen, CLASE$codes) - 1L) * 62L + match(rules$Producto, CLASE$sectors)
  cj <- (match(rules$Destino, CLASE$codes) - 1L) * 62L + match(rules$Uso, CLASE$sectors)
  if (anyNA(ri) || anyNA(cj)) stop("Identificadores de calibracion invalidos.")
  rules$Valor_previo <- before[cbind(ri, cj)]
  rules$Valor_usado <- rules$Valor_previo * rules$Factor
  rules$Delta <- rules$Valor_usado - rules$Valor_previo
  flows[cbind(ri, cj)] <- rules$Valor_usado
  safe <- function(a, b, tol) if (abs(b) <= tol) a * 0 else a / b
  bis_changes <- biso_changes <- list()
  for (i in seq_len(nrow(rules))) {
    destination <- match(rules$Destino[i], CLASE$codes)
    rows <- seq(match(rules$Producto[i], CLASE$sectors), 2170, by = 62)
    a <- before[rows, cj[i]]; b <- flows[rows, cj[i]]
    ia <- sum(a[-destination]); ib <- sum(b[-destination])
    ids <- rules[i, c("Producto", "Destino", "Uso"), drop = FALSE]
    bis_changes[[i]] <- data.frame(ids, Directo = safe(ia, ia + a[destination], 1e-12),
      Experimental = safe(ib, ib + b[destination], 1e-12),
      Importado_directo = ia, Importado_usado = ib, Domestico_directo = a[destination], Domestico_usado = b[destination])
    sa <- safe(a, ia, 1e-9); sb <- safe(b, ib, 1e-9)
    sa[destination] <- sb[destination] <- 0
    biso_changes[[i]] <- data.frame(Origen = CLASE$codes, ids[rep(1L, 35), , drop = FALSE],
      Directo = sa, Experimental = sb, Delta = sb - sa)
  }
  list(values = flows, policy = policy, rules = rules,
    bis_changes = do.call(rbind, bis_changes), biso_changes = do.call(rbind, biso_changes),
    note = paste0("EXPERIMENTAL: calibracion ", policy,
      ". Se ajusta solo una copia de flujos para BIS/BISO, despues de la politica de negativos; TC e IO_final conservan la IO de entrada. ",
      "Coque: LROW -> NLD / ANIMALS multiplicado por 1.05, calibrado en MRIO_original. ",
      if (policy == "coque-niquel") "Niquel: tres domesticos CYP/LTU multiplicados por cero; supone ausencia domestica tambien en cualquier IO nueva. " else "Niquel: sin calibrar. ",
      "BIS/BISO dejan de ser exclusivamente el calculo directo de IO_final. Se recalculan todos los origenes; no se copian shares oficiales. La generalizacion a otra IO no esta validada."))
}

calculate_coefficients <- function(values, negative_policy = c("existencias", "conservar"), tc_policy = c("cero", "wiliam"), calibration = c("ninguna", "coque", "coque-niquel"), reference_output = NULL, sector_labels = CLASE$sectors, unallocated_final = NULL) {
  negative_policy <- match.arg(negative_policy)
  tc_policy <- match.arg(tc_policy)
  calibration <- match.arg(calibration)
  stopifnot(identical(dim(values), c(2206L, 2380L)))
  if (any(!is.finite(values))) stop("IO final no finita.")
  uses <- c(sector_labels, CLASE$final)
  sector_id <- rep(1:62, 35)
  cost <- colSums(values[, 1:2170, drop = FALSE])
  output <- if (is.null(reference_output)) cost else reference_output
  stopifnot(length(output)==2170L, all(is.finite(output)))
  numerator <- rowsum(values[1:2170, 1:2170, drop = FALSE], sector_id)
  tc_wide <- sweep(numerator, 2, output, "/")
  tc_wide[, abs(output) <= 1e-12] <- 0
  tc <- do.call(rbind, lapply(0:34, function(i) tc_wide[, i * 62 + 1:62, drop = FALSE]))
  colnames(tc) <- sector_labels
  if (any(!is.finite(tc))) stop("TC no finitos.")
  tc_ids <- data.frame(Country = rep(CLASE$codes, each = 62), Text = rep(sector_labels, 35))
  tc_raw <- tc
  replacements <- data.frame(Pais = character(), Sector_destino = character(), Metodo = character())
  if (tc_policy == "wiliam") {
    average <- matrix(0, 62, 62)
    for (i in 1:62) for (j in 1:62) {
      v <- tc_raw[seq(i, 2170, by = 62), j]
      v <- v[v != 0]
      average[i, j] <- if (length(v)) mean(v) else 0
    }
    # Production/Average_technology usa la tecnologia quimica para hydrogen.
    hydrogen <- match("HYDROGEN_PRODUCTION",sector_labels,nomatch=0L)
    if (hydrogen) average[, hydrogen] <- average[, 22]
    for (country in 1:35) for (j in 1:62) {
      rows <- (country - 1) * 62 + 1:62
      # Identify_zeroes en Production suma la columna TC de cada pais.
      if (sum(tc_raw[rows, j]) == 0) {
        tc[rows, j] <- average[, j]
        replacements[nrow(replacements) + 1L, ] <- list(CLASE$codes[country], sector_labels[j],
          if (j == hydrogen) "Media no nula de MANUFACTURE_CHEMICAL" else "Media no nula del mismo sector entre paises")
      }
    }
  }
  bad <- which(tc < -1e-12 | tc > .85 + 1e-12, arr.ind = TRUE)
  dest <- ((bad[, 1] - 1L) %/% 62L) * 62L + bad[, 2]
  entry <- (bad[, 1] - 1L) %% 62L + 1L
  tc_bad <- data.frame(Pais = CLASE$countries[(bad[, 1] - 1L) %/% 62L + 1L],
    Sector_entrada = sector_labels[entry], Sector_destino = sector_labels[bad[, 2]], Valor = tc[bad],
    Numerador = numerator[cbind(entry, dest)], Output_total = output[dest],
    Impuestos = colSums(values[2171:2205, 1:2170, drop = FALSE])[dest], VA = values[2206, dest])
  raw <- values[1:2170, , drop = FALSE]
  neg <- which(raw < 0, arr.ind = TRUE)
  use_ids <- c(rep(1:62, 35), rep(63:68, 35))
  country_ids <- c(rep(1:35, each = 62), rep(1:35, each = 6))
  negatives <- data.frame(Pais_origen = CLASE$codes[(neg[, 1] - 1L) %/% 62L + 1L],
    Producto = sector_labels[(neg[, 1] - 1L) %% 62L + 1L],
    Pais_destino = CLASE$codes[country_ids[neg[, 2]]], Uso = uses[use_ids[neg[, 2]]],
    Valor_original = raw[neg], Valor_usado = if (negative_policy == "conservar") raw[neg] else rep(0, nrow(neg)))
  outside_inventory <- sum(negatives$Uso != "CHANGE_IN_INVENTORIES_AND_VALUABLES")
  if (outside_inventory && negative_policy == "existencias") stop("Negativos fuera de existencias: no se aplica el ajuste BIS/BISO sin diagnostico. Para una prueba con todos los signos originales usa --negativos conservar.")
  adjusted <- raw
  if (negative_policy == "existencias") adjusted[adjusted < 0] <- 0
  share_calibration <- calibrate_share_flows(adjusted, calibration)
  adjusted <- share_calibration$values
  share_calibration$values <- NULL
  negatives$Valor_usado <- adjusted[neg]
  bis <- matrix(0, 2170, 68, dimnames = list(NULL, uses))
  bis_raw <- bis
  biso <- matrix(0, 75950, 68, dimnames = list(NULL, uses))
  bis_error <- biso_error <- 0
  bis_fail <- biso_fail <- 0L
  safe_div <- function(a, b, tol) { out <- a * 0; ok <- abs(b) > tol; out[ok] <- a[ok] / b[ok]; out }
  for (destination in 1:35) {
    cols <- c((destination - 1) * 62 + 1:62, 2170 + (destination - 1) * 6 + 1:6)
    for (product in 1:62) {
      rows <- seq(product, 2170, by = 62)
      m <- adjusted[rows, cols, drop = FALSE]
      imported <- colSums(m[-destination, , drop = FALSE])
      domestic <- m[destination, ]
      total <- imported + domestic
      row <- (destination - 1) * 62 + product
      bis[row, ] <- safe_div(imported, total, 1e-12)
      original <- raw[rows, cols, drop = FALSE]
      raw_import <- colSums(original[-destination, , drop = FALSE])
      bis_raw[row, ] <- safe_div(raw_import, raw_import + original[destination, ], 1e-12)
      error <- abs(bis[row, ] + safe_div(domestic, total, 1e-12) - as.numeric(abs(total) > 1e-12))
      bis_error <- max(bis_error, error); bis_fail <- bis_fail + sum(error > 1e-9)
      shares <- sweep(m, 2, imported, "/")
      shares[, abs(imported) <= 1e-9] <- 0
      shares[destination, ] <- 0
      if (any(!is.finite(shares))) stop("BISO no finitos.")
      error <- abs(colSums(shares) - as.numeric(abs(imported) > 1e-9))
      biso_error <- max(biso_error, error); biso_fail <- biso_fail + sum(error > 1e-9)
      out_rows <- ((0:34) * 62 + product - 1) * 35 + destination
      biso[out_rows, ] <- shares
    }
  }
  bis_ids <- data.frame(Pais = rep(CLASE$codes, each = 62), Sector = rep(sector_labels, 35))
  biso_ids <- data.frame(Pais = rep(CLASE$codes, each = 62 * 35),
    Sector_Fila = rep(rep(sector_labels, each = 35), 35), Pais_col = rep(CLASE$codes, 2170))
  range_bad <- function(m, tol) sum(!is.finite(m) | m < -tol | m > 1 + tol)
  balance <- data.frame(Pais = rep(CLASE$countries, each = 62), Sector = rep(sector_labels, 35),
    Output = output, Demanda = rowSums(raw))
  if (!is.null(unallocated_final)) {
    balance$Demanda_asignada <- balance$Demanda
    balance$Ajuste_Y_no_asignado <- unallocated_final
    balance$Demanda <- balance$Demanda + unallocated_final
  }
  balance$Diferencia <- balance$Demanda - balance$Output
  if (!is.null(reference_output)) {
    balance$Costes <- cost
    balance$Costes_menos_X <- cost - output
  }
  raw_bad <- which(bis_raw < -1e-10 | bis_raw > 1 + 1e-10, arr.ind = TRUE)
  raw_detail <- data.frame(Pais = bis_ids$Pais[raw_bad[, 1]], Sector = bis_ids$Sector[raw_bad[, 1]],
    Uso = uses[raw_bad[, 2]], BIS_sin_ajuste = bis_raw[raw_bad])
  summary <- data.frame(Control = c("TC individuales fuera de [0,0.85]", "BIS rango [0,1]", "BISO rango [0,1]", "BIS suma importado+domestico", "BISO suma origen", "Balance MRIO >1e-6",
    "Comparacion Production.xlsx", "Comparacion Trade.xlsx", "Comparacion PP_to_BP.xlsx"),
    Incidencias = c(nrow(bad), range_bad(bis, 1e-10), range_bad(biso, 1e-12), bis_fail, biso_fail,
      sum(abs(balance$Diferencia) > 1e-6), NA, NA, NA),
    Max_error = c(if (nrow(bad)) max(pmax(-tc[bad], tc[bad] - .85, 0)) else 0,
      max(0, -min(bis), max(bis) - 1), max(0, -min(biso), max(biso) - 1), bis_error, biso_error,
      max(abs(balance$Diferencia)), NA, NA, NA))
  summary$Resultado <- ifelse(is.na(summary$Incidencias), "PENDIENTE", ifelse(summary$Incidencias == 0, "BIEN", "REVISAR"))
  if (any(summary$Incidencias[4:5] > 0) || (negative_policy == "existencias" && any(summary$Incidencias[2:3] > 0))) stop("Falla un control interno de shares.")
  if (negative_policy == "conservar") summary <- rbind(summary, data.frame(
    Control = "Negativos fuera de existencias conservados (sin ajuste)", Incidencias = outside_inventory,
    Max_error = if (outside_inventory) max(abs(negatives$Valor_original[negatives$Uso != "CHANGE_IN_INVENTORIES_AND_VALUABLES"])) else 0,
    Resultado = if (outside_inventory) "REVISAR" else "BIEN"))
  if (calibration != "ninguna") summary <- rbind(summary, data.frame(
    Control = paste("Calibracion experimental de shares:", calibration),
    Incidencias = sum(share_calibration$rules$Delta != 0),
    Max_error = max(abs(share_calibration$rules$Delta)), Resultado = "EXPERIMENTAL"))
  list(tc = data.frame(tc_ids, tc, check.names = FALSE), bis = data.frame(bis_ids, bis, check.names = FALSE),
    biso = data.frame(biso_ids, biso, check.names = FALSE), summary = summary, tc_bad = tc_bad,
    negatives = negatives, balance = balance, reference_output = reference_output, raw_detail = raw_detail, negative_policy = negative_policy,
    tc_policy = tc_policy, tc_raw = data.frame(tc_ids, tc_raw, check.names = FALSE), tc_replacements = replacements,
    calibration = share_calibration,
    policy = if (calibration != "ninguna") paste("BIS/BISO: politica de negativos", negative_policy, "antes de la calibracion experimental; cada cambio se registra en CALIBRACION_FLUJOS. El control de suma sigue siendo obligatorio.") else if (negative_policy == "conservar") "BIS/BISO: calculo con TODOS los valores y signos originales de la IO. No se ajustan flujos negativos. Las shares fuera de [0,1] se muestran como REVISAR; el control de suma sigue siendo obligatorio." else
      "BIS/BISO ajustados: negativos de existencias a cero; se conserva cada ajuste y el diagnostico de BIS bruto.",
    zero_output = data.frame(Pais = rep(CLASE$countries, each = 62)[abs(output) <= 1e-12],
      Sector = rep(sector_labels, 35)[abs(output) <= 1e-12]))
}

# Comparacion con los parametros originales, sin copiar sus resultados.
compare_model_references <- function(coefficients, production = NULL, trade = NULL, pp = NULL) {
  uses <- c(CLASE$sectors, CLASE$final)
  country_map <- setNames(rep(CLASE$codes, 3), canon(c(CLASE$countries, CLASE$codes, CLASE$regions)))
  read_ref <- function(path, sheet, ids, columns, expected) {
    d <- openxlsx::read.xlsx(path, sheet = sheet, startRow = 4, check.names = FALSE)
    names(d)[seq_along(ids)] <- ids
    d <- d[!is.na(d[[ids[1]]]) & !is.na(d[[ids[2]]]), , drop = FALSE]
    for (field in intersect(ids, c("Country", "Pais", "Pais_col"))) {
      d[[field]] <- unname(country_map[canon(d[[field]])])
    }
    key <- function(x) do.call(paste, c(x[ids], sep = "\r"))
    if (anyNA(d[ids]) || anyDuplicated(key(d))) stop("Claves de referencia invalidas: ", sheet)
    idx <- match(key(expected), key(d))
    if (anyNA(idx) || !all(columns %in% names(d))) stop("Referencia incompleta o incompatible: ", sheet)
    m <- as.matrix(d[idx, columns, drop = FALSE]); suppressWarnings(storage.mode(m) <- "double")
    list(values = m, physical_row = idx + 4L)
  }
  summary <- list(); details <- list()
  add <- function(name, actual, reference, tolerance, columns, ids, source, sheet, different_source = FALSE) {
    valid <- is.finite(reference$values)
    error <- abs(actual - reference$values)
    bad <- valid & error > tolerance; strict <- valid & error > 1e-10
    status <- if (different_source) "FUENTE_DISTINTA" else if (any(bad)) "REVISAR" else if (any(!valid)) "PENDIENTE" else if (any(strict)) "DENTRO_TOLERANCIA" else "BIEN"
    summary[[length(summary) + 1L]] <<- data.frame(Control = name, Total = length(actual), Referencia_vacia = sum(!valid),
      Diferencias_1e_10 = sum(strict), Fuera_tolerancia = sum(bad), Tolerancia = tolerance,
      Max_error = if (any(valid)) max(error[valid]) else NA_real_, Resultado = status)
    pos <- which(strict | !valid, arr.ind = TRUE)
    if (nrow(pos)) {
      order <- order(ifelse(valid[pos], error[pos], Inf), decreasing = TRUE)
      pos <- pos[head(order, if (different_source) 100L else 500L), , drop = FALSE]
      block <- data.frame(Control = name, ids[pos[, 1], , drop = FALSE], Uso = columns[pos[, 2]],
        Calculado = actual[pos], Referencia = reference$values[pos], Error_abs = error[pos],
        Archivo = basename(source), Hoja = sheet,
        Celda = paste0(openxlsx::int2col(ncol(ids) + match(columns, uses)[pos[, 2]]), reference$physical_row[pos[, 1]]),
        stringsAsFactors = FALSE)
      details[[length(details) + 1L]] <<- block
    }
  }
  numeric_block <- function(d, columns) as.matrix(d[, columns, drop = FALSE])
  if (!is.null(production)) {
    raw <- read_ref(production, "EXO_Technical_coefficients_1", c("Country", "Text"), CLASE$sectors, coefficients$tc)
    final <- read_ref(production, "EXO_Technical_coefficients", c("Country", "Text"), CLASE$sectors, coefficients$tc)
    add("TC bruto MRIO frente a bruto oficial", numeric_block(coefficients$tc_raw, CLASE$sectors), raw, 2e-6,
      CLASE$sectors, coefficients$tc[1:2], production, "EXO_Technical_coefficients_1")
    add("TC calculado frente a final Production", numeric_block(coefficients$tc, CLASE$sectors), final, 2e-6,
      CLASE$sectors, coefficients$tc[1:2], production, "EXO_Technical_coefficients")
  }
  if (!is.null(trade)) {
    bis <- read_ref(trade, "BASE_Import_shares", c("Pais", "Sector"), uses, coefficients$bis)
    biso <- read_ref(trade, "EXO_Import_origin_shares", c("Pais", "Sector_Fila", "Pais_col"), uses, coefficients$biso)
    for (block in c("intermedios", "demanda final")) {
      columns <- if (block == "intermedios") CLASE$sectors else CLASE$final
      idx <- match(columns, uses)
      bref <- list(values = bis$values[, idx, drop = FALSE], physical_row = bis$physical_row)
      oref <- list(values = biso$values[, idx, drop = FALSE], physical_row = biso$physical_row)
      add(paste("BIS", block, "frente a Trade"), numeric_block(coefficients$bis, columns), bref, 1e-4, columns,
        coefficients$bis[1:2], trade, "BASE_Import_shares")
      add(paste("BISO", block, "frente a Trade"), numeric_block(coefficients$biso, columns), oref, 1e-4, columns,
        coefficients$biso[1:3], trade, "EXO_Import_origin_shares", different_source = block == "demanda final")
    }
    if (!is.null(pp)) {
      final_pp <- read_ref(pp, "BASE_Import_origin_shares_PP", c("Pais", "Sector_Fila", "Pais_col"), CLASE$final, coefficients$biso)
      final_trade <- list(values = biso$values[, 63:68, drop = FALSE], physical_row = biso$physical_row)
      add("BISO demanda final PP frente a Trade (comprobacion de fuente)", final_pp$values, final_trade, 1e-10,
        CLASE$final, coefficients$biso[1:3], trade, "EXO_Import_origin_shares")
    }
  }
  # Unir los distintos identificadores sin perder los datos de cada bloque.
  all_names <- unique(unlist(lapply(details, names)))
  if (length(details)) details <- do.call(rbind, lapply(details, function(d) {
    for (n in setdiff(all_names, names(d))) d[[n]] <- NA_character_
    d[, all_names, drop = FALSE]
  })) else details <- data.frame(Nota = "Sin diferencias estrictas")
  list(summary = if (length(summary)) do.call(rbind, summary) else data.frame(), details = details)
}

# 4. Dos libros finales; sin Excel/RData/CSV intermedios ---------------------
write_sheet <- function(wb, sheet, data) {
  if (!ncol(data)) data <- data.frame(Nota = "Sin registros")
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, data)
  openxlsx::freezePane(wb, sheet, firstRow = TRUE, firstCol = TRUE)
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(fgFill = "#1F4E78", fontColour = "#FFFFFF", textDecoration = "bold"),
    rows = 1, cols = seq_len(ncol(data)), gridExpand = TRUE)
  openxlsx::setColWidths(wb, sheet, cols = seq_len(min(ncol(data), 3)), widths = 28)
}
write_results <- function(io, coefficients, conversion, files, destination, note = NULL, entry_sheet = NULL, reference_audit = NULL) {
  if (dir.exists(destination) && length(list.files(destination, all.files = TRUE, no.. = TRUE))) stop("La carpeta de salida no esta vacia; se conserva su contenido.")
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  wb <- openxlsx::createWorkbook()
  write_sheet(wb, "IO_final", io_table(io,coefficients$balance$Sector[1:62]))
  write_sheet(wb, "PROCEDENCIA_IO", data.frame(
    Concepto = c("Nota_entrada", "Conversion", "Archivo_entrada", "Hoja_entrada", "MD5_entrada", "Calibracion_shares"),
    Valor = c(if (is.null(note)) "Sin nota de procedencia adicional en la entrada." else note,
      conversion$policy, normalizePath(files[1]), if (is.null(entry_sheet)) "No indicada" else entry_sheet,
      unname(tools::md5sum(files[1])), coefficients$calibration$note)))
  openxlsx::saveWorkbook(wb, file.path(destination, "IO_final.xlsx"), overwrite = FALSE)
  rm(wb); invisible(gc(FALSE))
  wb <- openxlsx::createWorkbook()
  sheets <- c("EXO_Technical_coefficients_UNIZ", "BASE_IMPORT_SHARES_UNIZ", "EXO_Import_origin_shares_UNIZ")
  write_sheet(wb, "INDICE", data.frame(Bloque = c("TC", "BIS", "BISO"), Hoja = sheets,
    Destino_WILIAM = c("Production.xlsx", "Trade.xlsx", "Trade.xlsx"),
    Calibracion_shares = coefficients$calibration$policy))
  for (i in 1:3) write_sheet(wb, sheets[i], coefficients[[c("tc", "bis", "biso")[i]]])
  for (country in 1:35) {
    rows <- 2 + (country - 1) * 62 + 0:61
    openxlsx::createNamedRegion(wb, sheets[1], rows = rows, cols = 3:64, name = paste0("A_MATRIX_TOTAL_", CLASE$regions[country], "_UNIZ"))
    openxlsx::createNamedRegion(wb, sheets[2], rows = rows, cols = 3:64, name = paste0("BASE_IMPORT_SHARES_INTERMEDIATES_", CLASE$regions[country], "_UNIZ"))
    for (sector in 1:62) {
      rows <- 2 + ((country - 1) * 62 + sector - 1) * 35 + 0:34
      openxlsx::createNamedRegion(wb, sheets[3], rows = rows, cols = 4:65,
        name = paste0("BASE_IMPORT_SHARES_BY_ORIGIN_BY_SECTORS_", CLASE$regions[country], "_", CLASE$sector_ranges[sector], "_UNIZ"))
    }
  }
  tables <- list(COMPROBACIONES = coefficients$summary, CONVERSION = conversion$checks,
    TC_FUERA_RANGO = coefficients$tc_bad, TC_OUTPUT_CERO = coefficients$zero_output,
    NEGATIVOS_AJUSTADOS = coefficients$negatives, BIS_RANGO_SIN_AJUSTE = coefficients$raw_detail,
    BALANCE = coefficients$balance,
    METODOLOGIA = data.frame(Descripcion = c(conversion$policy, if (!is.null(note)) note,
      if (is.null(coefficients$reference_output)) "TC bruto: denominador con productos, 35 impuestos y VA. Output cero se calcula como cero antes de cualquier tecnologia media. El control 0.85 no recorta coeficientes." else "TC bruto: denominador X historica de UNIZAR, repartida con proporciones de produccion WILIAM. BALANCE compara por separado ventas y costes con esa X.",
      if (coefficients$tc_policy == "wiliam") "TC final: regla Production. Columnas TC totalmente nulas reciben tecnologia media no nula entre paises; hydrogen usa la media quimica. Se conserva tambien TC bruto." else "TC: se conserva la matriz bruta, sin tecnologia media.",
      coefficients$policy,
      coefficients$calibration$note,
      "BIS: 62 usos intermedios y 6 usos finales de la misma MRIO (2170 filas y 68 coeficientes, mas Pais y Sector). Las columnas finales pertenecen a BASE_IMPORT_SHARES_UNIZ; no se genera la variable independiente BASE_IMPORT_SHARES_FINAL_DEMAND a precios de comprador.",
      "BIS: tolerancia denominador 1e-12. BISO: tolerancia denominador 1e-9. Sumas de shares: tolerancia 1e-9.",
      if (is.null(reference_audit)) "Referencias externas no comparadas: controles PENDIENTES." else "Comparaciones con originales en COMPARACION_ORIGINALES. Se conservan diferencias y referencias vacias. BISO demanda final Trade procede de PP y no se sustituye por ella el calculo MRIO a precios basicos.",
      "Se guardan solo las filas contables de IO_final; las filas auxiliares antiguas no intervienen en ningun calculo.",
      "BISO: Pais es origen, Pais_col es destino. La diagonal domestica es cero; la suma por origen es 1 o 0 sin flujo.")),
    PROCEDENCIA = data.frame(Archivo = normalizePath(files), Hoja = c(if (is.null(entry_sheet)) NA_character_ else entry_sheet,
      rep(NA_character_, length(files) - 1L)), MD5 = unname(tools::md5sum(files))),
    VERSIONES = data.frame(Informacion = capture.output(sessionInfo())))
  if (!is.null(conversion$balance48)) tables$BALANCE_48 <- conversion$balance48
  if (!is.null(conversion$cuadre)) tables$CUADRE_48 <- conversion$cuadre
  if (!is.null(conversion$split_detail)) tables$REPARTO_X_Z <- conversion$split_detail
  if (!is.null(conversion$pending)) tables$SIN_PESOS_W <- conversion$pending
  if (!is.null(conversion$recovered)) tables$REPARTO_PROPORCIONAL <- conversion$recovered
  if (!is.null(conversion$closure)) {
    tables$CIERRE <- conversion$closure$summary
    tables$AJUSTE_POR_ACTIVIDAD <- conversion$closure$detail
  }
  if (identical(coefficients$negative_policy, "conservar")) names(tables)[names(tables) == "NEGATIVOS_AJUSTADOS"] <- "NEGATIVOS_CONSERVADOS"
  if (identical(coefficients$tc_policy, "wiliam")) {
    tables$TC_BRUTO <- coefficients$tc_raw
    tables$TC_TECNOLOGIA_MEDIA <- coefficients$tc_replacements
  }
  if(isTRUE(coefficients$bis_bounded)) {
    tables$BIS_DIRECTO<-coefficients$bis_original
    tables$BIS_AJUSTES<-coefficients$bis_bound_changes
  }
  if (coefficients$calibration$policy != "ninguna") {
    tables$CALIBRACION_FLUJOS <- coefficients$calibration$rules
    tables$BIS_DIRECTO_VS_EXPERIMENTAL <- coefficients$calibration$bis_changes
    tables$BISO_DIRECTO_VS_EXPERIMENTAL <- coefficients$calibration$biso_changes
  }
  if (!is.null(reference_audit)) {
    tables$COMPARACION_ORIGINALES <- reference_audit$summary
    tables$DIFERENCIAS_ORIGINALES <- reference_audit$details
  }
  for (name in names(tables)) write_sheet(wb, name, tables[[name]])
  openxlsx::saveWorkbook(wb, file.path(destination, "Resultados.xlsx"), overwrite = FALSE)
  cat("\nGuardados IO_final.xlsx y Resultados.xlsx en:\n", normalizePath(destination), "\n")
}

# Recorte explicito de BIS solicitado por el usuario. No cambia la MRIO
# ni BISO; se conservan los cocientes originales y todos los ajustes.
bound_bis <- function(coefficients) {
  k<-coefficients
  original<-as.matrix(k$bis[,-(1:2),drop=FALSE])
  if(any(!is.finite(original)))stop("No se acota un BIS con valores no finitos.")
  bounded<-pmin(1,pmax(0,original))
  dim(bounded)<-dim(original);dimnames(bounded)<-dimnames(original)
  ij<-which(bounded!=original,arr.ind=TRUE)
  changes<-k$bis[ij[,1],1:2,drop=FALSE];rownames(changes)<-NULL
  changes$Uso<-colnames(original)[ij[,2]]
  changes$BIS_directo<-original[ij];changes$BIS_acotado<-bounded[ij]
  changes$Delta<-changes$BIS_acotado-changes$BIS_directo
  changes$Regla<-ifelse(changes$BIS_directo<0,"Negativo a cero","Superior a uno a uno")
  k$bis_original<-k$bis;k$bis[,-(1:2)]<-bounded
  k$bis_bound_changes<-changes;k$bis_bounded<-TRUE
  if(!is.null(k$bis_values)){k$bis_values<-bounded;k$bis_outside<-0L}
  if(!is.null(k$summary)) {
    k$summary$Control[2]<-"BIS acotado rango [0,1]"
    k$summary$Incidencias[2]<-0;k$summary$Max_error[2]<-0;k$summary$Resultado[2]<-"BIEN"
    k$summary$Control[4]<-"BIS directo suma importado+domestico (antes de acotar)"
    k$summary<-rbind(k$summary,data.frame(Control="BIS modificados por recorte explicito",
      Incidencias=nrow(changes),Max_error=if(nrow(changes))max(abs(changes$Delta)) else 0,Resultado="AJUSTADO"))
  }
  k$policy<-paste("BIS entregado = min(1,max(0,BIS directo)). No se modifican flujos de la MRIO ni BISO.",
    "BIS_DIRECTO conserva los cocientes anteriores y BIS_AJUSTES registra cada cambio.",
    "El control importado+domestico se refiere al BIS directo: el recorte no cambia las participaciones domesticas derivadas de la fuente.")
  k
}

# Exportar solo parametros cuando la entrada ya proporciona todos los usos.
write_parameter_results <- function(values, coefficients, files, destination, entry_sheet=NULL, conversion=NULL) {
  if(dir.exists(destination)&&length(list.files(destination,all.files=TRUE,no..=TRUE)))stop("La carpeta de salida no esta vacia.")
  dir.create(destination,recursive=TRUE,showWarnings=FALSE)
  k<-coefficients
  x<-k$balance$Output
  raw_tc<-as.matrix(k$tc_raw[,-(1:2)])
  final_tc<-as.matrix(k$tc[,-(1:2)])
  bis<-as.matrix(k$bis[,-(1:2)]);biso<-as.matrix(k$biso[,-(1:3)])
  bis_direct<-if(isTRUE(k$bis_bounded))as.matrix(k$bis_original[,-(1:2)]) else bis
  outside<-function(m) m< -1e-10|m>1+1e-10
  checks<-data.frame(Control=c("TC directo superiores a 1","TC calculado superiores a 1",
    "TC directo superiores a 0.85 (control del modelo)","TC maximo directo", "BIS directo intermedios fuera de [0,1]",
    "BIS directo usos finales fuera de [0,1]","BISO intermedios fuera de [0,1]","BISO usos finales fuera de [0,1]",
    "Error suma BIS directo mas participacion domestica","Error suma BISO entre origenes extranjeros",
    "Producciones X negativas","Columnas con tecnologia media"),
    Valor=c(sum(raw_tc>1),sum(final_tc>1),sum(raw_tc>.85),max(raw_tc),
      sum(outside(bis_direct[,1:62])),sum(outside(bis_direct[,63:68])),sum(outside(biso[,1:62])),sum(outside(biso[,63:68])),
      k$summary$Max_error[4],k$summary$Max_error[5],sum(x<0),nrow(k$tc_replacements)))
  if(isTRUE(k$bis_bounded))checks<-rbind(checks,data.frame(
    Control=c("BIS entregado fuera de [0,1]","BIS modificados por recorte"),Valor=c(sum(outside(bis)),nrow(k$bis_bound_changes))))
  detail<-function(d,ids) {
    m<-as.matrix(d[,-seq_len(ids),drop=FALSE]);ij<-which(outside(m),arr.ind=TRUE)
    out<-d[ij[,1],seq_len(ids),drop=FALSE];rownames(out)<-NULL
    out$Uso<-names(d)[ids+ij[,2]];out$Valor<-m[ij];out
  }
  methods<-data.frame(Asunto=c("Entrada","Alcance","Datos utilizados","TC directo","TC con regla WILIAM",
    "BIS y BISO","Signos","Usos finales","Produccion X","Comparaciones"),
    Explicacion=c(paste("Archivo de entrada:",basename(files[1]),". Hoja:",entry_sheet),
      "Solo se exportan coeficientes e incidencias. No se genera otra IO ni se convierte de 48 a 62: esta matriz ya tiene 62 sectores.",
      "Se calculan los parametros a partir de la hoja seleccionada, sin aplicar la A de UNIZAR, su correspondencia ni sus repartos de respaldo.",
      "TC_DIRECTO divide compras intermedias del producto, sumadas entre paises de origen, entre la produccion de la actividad compradora. Produccion nula se trata como en el codigo anterior: TC cero.",
      if(k$tc_policy=="wiliam")"TC_WILIAM aplica ademas la tecnologia media a columnas TC totalmente nulas; para el codigo 25 usa la media quimica. Se entrega TC_DIRECTO para distinguir ambos resultados." else "No se aplica tecnologia media a los TC.",
      "BIS directo = importado / (domestico + importado). Si se solicita acotarlo, BIS entregado = min(1,max(0,BIS directo)). BISO = flujo de cada origen extranjero / importado total; su diagonal domestica es cero.",
      k$policy,
      "BIS y BISO incluyen 62 usos intermedios y 6 usos finales disponibles en la misma matriz. No se calcula la variable independiente BASE_IMPORT_SHARES_FINAL_DEMAND.",
      "X se obtiene sumando las filas contables de la propia matriz en cada columna intermedia. Los valores negativos se identifican en la hoja X, sin cambiarlos.",
      "No se exige que esta matriz modificada reproduzca los parametros oficiales de la MRIO original. Los controles incluidos se refieren a los coeficientes y sus denominadores."))
  if (!is.null(conversion)) {
    methods$Explicacion[methods$Asunto=="Alcance"] <- "Escenario con A nueva. Se conserva la demanda final, los impuestos netos y el VA de la plantilla de compatibilidad."
    methods$Explicacion[methods$Asunto=="Datos utilizados"] <- conversion$policy
    methods$Explicacion[methods$Asunto=="Produccion X"] <- "X historica de UNIZAR, repartida con las proporciones de produccion WILIAM. BALANCE compara las ventas y los costes con esa X."
    if (!is.null(conversion$closure)) {
      methods$Explicacion[methods$Asunto=="Alcance"] <- "Cierre contable estimado con A y X fijas. Se ajustan los usos finales, impuestos netos y VA."
      methods$Explicacion[methods$Asunto=="Usos finales"] <- "BIS y BISO de los seis usos finales se calculan con Y estimada. El ajuste final sin destino ni uso se incluye solo en BALANCE."
    }
  }
  wb<-openxlsx::createWorkbook()
  hs<-openxlsx::createStyle(fontName="Arial",fontSize=10,fontColour="white",fgFill="#1F4E78",textDecoration="bold",wrapText=TRUE,halign="center",valign="center")
  add<-function(name,d,widths,small=FALSE,height=34) {
    openxlsx::addWorksheet(wb,name,gridLines=FALSE);openxlsx::writeData(wb,name,d)
    openxlsx::setColWidths(wb,name,cols=seq_len(ncol(d)),widths=widths)
    openxlsx::addStyle(wb,name,hs,rows=1,cols=seq_len(ncol(d)),gridExpand=TRUE)
    openxlsx::setRowHeights(wb,name,rows=1,heights=60)
    openxlsx::freezePane(wb,name,firstRow=TRUE)
    if(small&&nrow(d)) {
      bs<-openxlsx::createStyle(fontName="Arial",fontSize=10,wrapText=TRUE,valign="center")
      openxlsx::addStyle(wb,name,bs,rows=2:(nrow(d)+1),cols=seq_len(ncol(d)),gridExpand=TRUE)
      openxlsx::setRowHeights(wb,name,rows=2:(nrow(d)+1),heights=height)
    }
  }
  add("METODO",methods,c(35,150),TRUE,68)
  add("CONTROLES",checks,c(85,30),TRUE)
  if (!is.null(conversion)) {
    add("CONVERSION",conversion$checks,c(85,28),TRUE)
    if (!is.null(conversion$cuadre)) add("CUADRE_48",conversion$cuadre,c(70,28),TRUE)
    add("BALANCE",k$balance,c(23,55,rep(27,ncol(k$balance)-2)))
    add("BALANCE_48",conversion$balance48,c(23,18,rep(24,8)))
    add("REPARTO_X_Z",conversion$split_detail,c(23,18,18,24,24,24,24,24,24))
    add("SIN_PESOS_W",conversion$pending,c(23,18,23,18,24,45,45))
    if (!is.null(conversion$recovered)) add("REPARTO_PROPORCIONAL",conversion$recovered,c(23,18,23,18,24,45,45))
    if (!is.null(conversion$closure)) {
      add("CIERRE",conversion$closure$summary,c(85,28),TRUE)
      add("AJUSTE_POR_ACTIVIDAD",conversion$closure$detail,c(23,18,rep(25,ncol(conversion$closure$detail)-2)))
    }
  }
  add("TC_DIRECTO",k$tc_raw,c(14,55,rep(25,62)))
  if(k$tc_policy=="wiliam")add("TC_WILIAM",k$tc,c(14,55,rep(25,62)))
  add("BIS",k$bis,c(14,55,rep(25,68)))
  if(isTRUE(k$bis_bounded)) {
    add("BIS_DIRECTO",k$bis_original,c(14,55,rep(25,68)))
    add("BIS_AJUSTES",k$bis_bound_changes,c(14,55,65,28,28,28,45),TRUE,42)
  }
  add("BISO",k$biso,c(14,55,14,rep(25,68)))
  for(name in c("TC_DIRECTO","BIS",if(k$tc_policy=="wiliam")"TC_WILIAM"))
    openxlsx::freezePane(wb,name,firstActiveRow=2,firstActiveCol=3)
  openxlsx::freezePane(wb,"BISO",firstActiveRow=2,firstActiveCol=4)
  output<-data.frame(Pais=rep(CLASE$codes,each=62),Sector=k$balance$Sector,X=x,
    Estado=ifelse(x<0,"NEGATIVA",ifelse(x==0,"CERO","POSITIVA")))
  add("X",output,c(14,65,30,20))
  tc_bad<-k$tc_bad[,c("Pais","Sector_entrada","Sector_destino","Valor","Numerador","Output_total"),drop=FALSE]
  add("TC_SUPERAN_085",tc_bad,c(24,65,65,28,28,28),TRUE,45)
  add("BIS_FUERA_RANGO",detail(if(isTRUE(k$bis_bounded))k$bis_original else k$bis,2),c(14,55,65,28),TRUE,42)
  add("BISO_FUERA_RANGO",detail(k$biso,3),c(14,55,14,65,28),TRUE,42)
  if(k$tc_policy=="wiliam")add("TC_MEDIA",k$tc_replacements,c(14,65,90),TRUE,42)
  sheet_sources<-c(if(is.null(entry_sheet))"No indicada" else entry_sheet,rep(NA_character_,length(files)-1L))
  add("FUENTES",data.frame(Archivo=normalizePath(files),Hoja=sheet_sources,MD5=unname(tools::md5sum(files))),c(150,25,45),TRUE,45)
  openxlsx::saveWorkbook(wb,file.path(destination,"Coeficientes.xlsx"),overwrite=FALSE)
  print(checks,row.names=FALSE)
  invisible(checks)
}

# 5. Flujo A/X UNIZAR: solo intermedios, sin construir una IO completa ------
read_unizar_AX <- function(path, matrix_path = path) {
  a <- numeric_matrix(openxlsx::read.xlsx(matrix_path, sheet="A", colNames=FALSE,
    skipEmptyRows=FALSE, skipEmptyCols=FALSE))
  x <- as.numeric(unlist(openxlsx::read.xlsx(path, sheet="X", colNames=FALSE), use.names=FALSE))
  countries <- canon(unlist(openxlsx::read.xlsx(path, sheet="country list", colNames=FALSE), use.names=FALSE))
  sectors <- as.integer(unlist(openxlsx::read.xlsx(path,sheet="sector list",colNames=FALSE),use.names=FALSE))
  if (!identical(dim(a),c(1680L,1680L)) || length(x)!=1680L ||
      length(countries)!=35L || anyDuplicated(countries) || !setequal(countries,CLASE$countries) ||
      !identical(sectors,1:48) || any(!is.finite(x)) || any(x<0)) stop("A/X UNIZAR no tienen dimensiones, paises, sectores o producciones validas.")
  ix <- unlist(lapply(match(CLASE$countries,countries),function(i)(i-1L)*48L+1:48))
  a <- a[ix,ix,drop=FALSE]
  x <- x[ix]
  list(A=a, X=x, Z=sweep(a,2,x,"*"))
}

read_unizar_delivery <- function(path) {
  read_block <- function(sheet, dims) {
    m <- numeric_matrix(openxlsx::read.xlsx(path,sheet=sheet,colNames=FALSE,
      skipEmptyRows=FALSE,skipEmptyCols=FALSE))
    if (!identical(dim(m),dims)) stop("Dimensiones inesperadas en la hoja ",sheet,".")
    m
  }
  z <- read_block("Z",c(1680L,1680L))
  x <- as.numeric(read_block("X",c(1680L,1L)))
  va <- as.numeric(read_block("VA",c(1L,1680L)))
  y <- read_block("y",c(1680L,105L))
  if (any(x<0) || any(x==0 & colSums(abs(z))>0)) stop("X no es compatible con Z.")
  list(Z=z,X=x,VA=va,Y=y,delivery=TRUE)
}

read_reference_output <- function(path) {
  d <- openxlsx::read.xlsx(path,sheet="BASE_Output_real",startRow=4,check.names=FALSE)
  country_map <- setNames(rep(CLASE$countries,3),canon(c(CLASE$countries,CLASE$codes,CLASE$regions)))
  countries <- unname(country_map[canon(d[[1]])])
  ix <- match(CLASE$countries,countries)
  if(anyNA(ix) || anyDuplicated(countries[!is.na(countries)]) ||
     !all(CLASE$sectors %in% names(d))) stop("No se reconoce BASE_Output_real de Production.")
  m <- numeric_matrix(d[ix,CLASE$sectors,drop=FALSE])
  if(any(m<0)) stop("La referencia de produccion tiene valores negativos.")
  as.numeric(t(m))
}

convert_intermediate_AX <- function(z48,x48,mapping,weights,x_reference, fallback=c("produccion","ninguno"),
    buyer_profile=c("wiliam","produccion"), seller_profile=c("wiliam","produccion")) {
  fallback <- match.arg(fallback)
  buyer_profile <- match.arg(buyer_profile)
  seller_profile <- match.arg(seller_profile)
  wi <- as.integer(mapping$code_wi); za <- as.integer(mapping$code_za)
  if(anyNA(wi)||anyNA(za)||anyDuplicated(wi)||!setequal(wi,1:62)||!setequal(za,1:48)) stop("Correspondencia invalida.")
  p <- za[match(1:62,wi)]
  parent <- rep((0:34)*48L,each=62)+rep(p,35)
  counts <- tabulate(p,48)
  split <- rep(counts[p]>1L,35)
  # Production is used only to derive dimensionless output shares. All new
  # production amounts come from the UNIZAR X supplied by the user.
  shares <- rep(1,2170); method <- rep("Uno a uno",2170)
  for(country in 1:35) for(sector in which(counts>1L)) {
    children <- which(p==sector); ids <- (country-1L)*62L+children
    v <- x_reference[ids]
    if(sum(v)>0) method[ids] <- "Produccion del mismo pais" else {
      v <- vapply(children,function(j)sum(x_reference[seq(j,2170,62)]),numeric(1))
      method[ids] <- "Produccion conjunta de 35 regiones"
    }
    if(sum(v)<=0) stop("No hay produccion de referencia local ni conjunta para repartir el padre ",sector,".")
    shares[ids] <- v/sum(v)
  }
  x62 <- x48[parent]*shares
  aggregate_both <- function(m)t(rowsum(t(rowsum(m,parent)),parent))
  sums <- aggregate_both(weights)
  one <- outer(rep(counts==1L,35),rep(counts==1L,35),"&")
  supported <- one | sums>0
  den <- sums[parent,parent]
  w <- matrix(0,2170,2170)
  active <- outer(split,split,"|") & den>0
  w[active] <- weights[active]/den[active]
  w[!outer(split,split,"|")] <- 1
  no_weights <- !supported
  used_fallback <- no_weights & z48!=0
  if(fallback=="produccion") {
    child_missing <- no_weights[parent,parent]
    independent <- outer(shares,shares)
    w[child_missing] <- independent[child_missing]
    target <- matrix(1,1680,1680)
  } else target <- supported
  if (buyer_profile=="produccion") {
    # Keep each supplying child share for every parent-to-parent flow and
    # give the buying children the same proportions as their output.
    origin_weights <- t(rowsum(t(w),parent))
    w <- sweep(origin_weights[,parent],2,shares,"*")
  }
  if (seller_profile=="produccion") {
    # Keep each buying child's share of the parent pair and distribute sales
    # among supplying children in proportion to their share of output.
    destination_weights <- rowsum(w,parent)
    w <- sweep(destination_weights[parent,],1,shares,"*")
  }
  z62 <- w*z48[parent,parent]
  weight_error <- max(abs(aggregate_both(w)-target))
  z_error <- max(abs(aggregate_both(z62)-z48*target)/pmax(1,abs(z48*target)))
  x_error <- max(abs(as.numeric(rowsum(matrix(x62),parent))-x48)/pmax(1,abs(x48)))
  if(max(weight_error,z_error,x_error)>1e-10)stop("Falla la conservacion de Z o X por padre.")
  ij <- which(used_fallback,arr.ind=TRUE)
  if(nrow(ij)) ij <- ij[order(abs(z48[ij]),decreasing=TRUE),,drop=FALSE]
  profile <- matrix("Uno a uno",35,48)
  for(country in 1:35)for(sector in which(counts>1L))
    profile[country,sector]<-method[(country-1)*62+which(p==sector)[1]]
  detail <- data.frame(Pais_origen=CLASE$countries[(ij[,1]-1)%/%48+1],Producto48=(ij[,1]-1)%%48+1,
    Pais_destino=CLASE$countries[(ij[,2]-1)%/%48+1],Uso48=(ij[,2]-1)%%48+1,Flujo_padre=z48[ij],
    Perfil_origen=profile[cbind((ij[,1]-1)%/%48+1,(ij[,1]-1)%%48+1)],
    Perfil_destino=profile[cbind((ij[,2]-1)%/%48+1,(ij[,2]-1)%%48+1)])
  output_detail <- data.frame(Pais=rep(CLASE$codes,each=62),Sector62=rep(1:62,35),
    Nombre_referencia=rep(CLASE$sectors,35),Padre48=rep(p,35),X_UNIZAR_padre=x48[parent],
    Produccion_referencia=x_reference,Proporcion=shares,X62=x62,Metodo=method)
  list(Z=z62,X=x62,output_detail=output_detail,fallback_detail=detail,parent=parent,
    fallback=fallback,buyer_profile=buyer_profile,seller_profile=seller_profile,
    old_missing_amount=sum(z48[no_weights]),old_missing_pairs=sum(used_fallback),
    remaining_amount=if(fallback=="ninguno")sum(z48[no_weights]) else 0,
    remaining_pairs=if(fallback=="ninguno")sum(used_fallback) else 0,
    weight_error=weight_error,z_error=z_error,x_error=x_error)
}

calculate_intermediate_coefficients <- function(z,x,sector_labels=CLASE$sectors) {
  if(!identical(dim(z),c(2170L,2170L)) || length(x)!=2170L ||
     any(!is.finite(z)) || any(!is.finite(x)) || any(x<0))stop("Z/X intermedias invalidas.")
  numerator <- rowsum(z,rep(1:62,35))
  t <- sweep(numerator,2,x,"/")
  empty <- x==0
  t[,empty] <- 0
  impossible <- which(empty & colSums(abs(numerator))>0)
  if(length(impossible))t[,impossible]<-NA_real_
  tc <- do.call(rbind,lapply(0:34,function(i)t[,i*62+1:62,drop=FALSE]))
  bis <- matrix(0,2170,62);biso<-matrix(0,75950,62)
  bis_error<-biso_error<-0
  for(destination in 1:35)for(product in 1:62) {
    rows <- seq(product,2170,62);cols<-(destination-1)*62+1:62
    m <- z[rows,cols,drop=FALSE]
    imported <- colSums(m[-destination,,drop=FALSE]);domestic<-m[destination,];total<-imported+domestic
    row <- (destination-1)*62+product
    ok<-total!=0;bis[row,ok]<-imported[ok]/total[ok]
    domestic_share<-rep(0,62);domestic_share[ok]<-domestic[ok]/total[ok]
    bis_error<-max(bis_error,abs(bis[row,]+domestic_share-as.numeric(ok)))
    origins<-matrix(0,35,62);ok_import<-imported!=0
    origins[,ok_import]<-sweep(m[,ok_import,drop=FALSE],2,imported[ok_import],"/")
    origins[destination,]<-0
    biso_error<-max(biso_error,abs(colSums(origins)-as.numeric(ok_import)))
    biso[((0:34)*62+product-1)*35+destination,]<-origins
  }
  if(any(!is.finite(bis))||any(!is.finite(biso))||max(bis_error,biso_error)>1e-9)stop("Falla el calculo interno de shares.")
  colnames(tc)<-colnames(bis)<-colnames(biso)<-sector_labels
  tc_ids<-data.frame(Country=rep(CLASE$codes,each=62),Text=rep(sector_labels,35))
  bis_ids<-data.frame(Pais=rep(CLASE$codes,each=62),Sector=rep(sector_labels,35))
  biso_ids<-data.frame(Pais=rep(CLASE$codes,each=62*35),Sector_Fila=rep(rep(sector_labels,each=35),35),Pais_col=rep(CLASE$codes,2170))
  list(tc=data.frame(tc_ids,tc,check.names=FALSE),bis=data.frame(bis_ids,bis,check.names=FALSE),
    biso=data.frame(biso_ids,biso,check.names=FALSE),tc_values=tc,bis_values=bis,biso_values=biso,
    tc_undefined=impossible,tc_above_085=sum(tc>.85,na.rm=TRUE),tc_above_1=sum(tc>1,na.rm=TRUE),
    bis_outside=sum(bis< -1e-10|bis>1+1e-10),biso_outside=sum(biso< -1e-10|biso>1+1e-10),
    bis_error=bis_error,biso_error=biso_error)
}

write_intermediate_results <- function(origin,converted,coefficients,mapping,files,out) {
  if(dir.exists(out)&&length(list.files(out,all.files=TRUE,no..=TRUE)))stop("La carpeta de salida no esta vacia.")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  c<-converted;k<-coefficients
  controls<-data.frame(Control=c("Flujo Z48","Flujo Z62","Flujo antes sin reparto W","Pares antes sin reparto W",
    "Flujo pendiente tras respaldo","Pares pendientes tras respaldo","Error conservacion pesos","Error relativo conservacion Z",
    "Error relativo conservacion X","TC con denominador cero y numerador no nulo","TC individuales superiores a 0.85 (control del modelo)",
    "TC individuales superiores a 1","BIS fuera de [0,1]","BISO fuera de [0,1]","Error BIS mas cuota domestica","Error BISO suma origen"),
    Valor=c(sum(origin$Z),sum(c$Z),c$old_missing_amount,c$old_missing_pairs,c$remaining_amount,c$remaining_pairs,
      c$weight_error,c$z_error,c$x_error,length(k$tc_undefined),k$tc_above_085,k$tc_above_1,
      k$bis_outside,k$biso_outside,k$bis_error,k$biso_error))
  notes<-data.frame(Asunto=c("Alcance","Fuente de cantidades","Reparto de X","Reparto cuando W es cero",
    "Hipotesis del respaldo","Sector 25","A de origen","TC","Shares sin operaciones","Signos"),
    Explicacion=c("Solo Z, X, TC, BIS y BISO intermedios. No se construye una IO completa ni se requieren impuestos, VA o demanda final.",
      "Todos los importes proceden de A y X de UNIZAR. Production aporta proporciones de reparto, no importes que se copien a la nueva IO.",
      "Sectores divididos: proporciones de BASE_Output_real del mismo pais; si todas son cero, proporciones conjuntas de las 35 regiones. Casos identificados en X62.",
      if(c$fallback=="produccion")"Se conservan pesos W disponibles. Solo donde no habia soporte se usa el producto de las proporciones de produccion del origen y del destino." else "No se aplica reparto de respaldo; los flujos sin soporte quedan pendientes.",
      "El producto de proporciones supone independencia entre los hijos del proveedor y del comprador en esos casos sin informacion. No son transacciones bilaterales observadas.",
      "La correspondencia solicitada coloca Basic metals de UNIZAR en el codigo 25, que Production llama HYDROGEN_PRODUCTION. Se conserva la asignacion numerica y se senala la discrepancia.",
      "Se mantienen los coeficientes originales, sus repeticiones y factores regionales. Su interpretacion sigue pendiente de aclaracion. Eso puede producir TC elevados.",
      "TC directo = suma por producto de todos los origenes / X62 de la actividad compradora. No se recortan TC ni se sustituye por tecnologia media.",
      "BIS=0 si el uso total es cero. BISO=0 si no hay importaciones; diagonal domestica=0. Las sumas unitarias se comprueban solo cuando existe denominador.",
      "Se conservan los signos de la fuente. Las shares fuera de [0,1] se registran, no se recortan."))
  if (!is.null(origin$external_matrix)) {
    notes$Explicacion[notes$Asunto=="A de origen"] <- paste(
      "A externa de", basename(origin$external_matrix),
      "(hoja A). El ensayo supone el mismo orden de regiones y sectores en ambos ejes y compatibilidad con X y W del libro UNIZAR de apoyo. Los controles de conservacion verifican el reparto, no esas hipotesis.")
    totals <- colSums(origin$A)
    residual <- origin$X - rowSums(origin$Z)
    active_output <- c$X > 0
    input_share <- colSums(c$Z)[active_output] / c$X[active_output]
    controls <- rbind(controls, data.frame(Control=c(
      "Mayor suma de columna de A externa", "Columnas de A externa con suma mayor que 1 (tol. 1e-10)",
      "Actividades con X de apoyo cero y A externa no nula en columna",
      "Filas con residuo X48 menos ventas intermedias negativo (tol. 1e-10)",
      "Menor residuo X48 menos ventas intermedias",
      "Actividades de 62 sectores con consumos intermedios mayores que X62 (tol. 1e-10 en el cociente)",
      "Mayor cociente consumos intermedios entre X62"),
      Valor=c(max(totals), sum(totals > 1+1e-10), sum(origin$X==0 & colSums(abs(origin$A))>0),
        sum(residual < -1e-10), min(residual), sum(input_share > 1+1e-10),
        if (length(input_share)) max(input_share) else NA_real_)))
  }
  if (isTRUE(origin$delivery)) {
    controls <- rbind(controls,c$delivery_checks)
    notes <- data.frame(Asunto=c("Fuente", "Orden", "Conversion a 62", "Valoracion", "Usos finales", "Parametros", "Sector 25"),
      Explicacion=c("Z, X, VA e y se toman de la entrega corregida. A se obtiene, si se necesita, dividiendo Z por X por columna.",
        "Las hojas son numericas y sin etiquetas. Se usa el orden anterior de regiones y sectores en los ejes productivos. Las columnas de y conservan sus posiciones originales.",
        "Ventas y compras de cada hijo siguen su cuota de produccion. VA e y se distribuyen por esas mismas cuotas dentro de cada padre.",
        "En la fuente, Z mas VA reproduce X por columna. La entrega no trae desglose separado de impuestos.",
        "Se guardan las 105 columnas de y en el RDS con identificador posicional. La entrega carece de una clave para convertirlas en los seis usos finales de WILIAM.",
        "Se calculan TC, BIS y BISO intermedios. Se conservan los signos y se verifican los denominadores.",
        "El codigo 25 se rotula metales basicos segun la correspondencia UNIZAR."))
  }
  if(isTRUE(k$bis_bounded)) {
    notes<-rbind(notes,data.frame(Asunto="BIS acotado por peticion expresa",Explicacion=k$policy))
    notes$Explicacion[notes$Asunto=="Signos"]<-"Se conservan los signos de los flujos de la fuente. Solo se acota el coeficiente BIS; BISO conserva sus cocientes directos."
    controls$Control[controls$Control=="Error BIS mas cuota domestica"]<-"Error BIS directo mas cuota domestica (antes de acotar)"
  }
  wb<-openxlsx::createWorkbook()
  hs<-openxlsx::createStyle(fontName="Arial",fontSize=10,fontColour="white",fgFill="#1F4E78",textDecoration="bold",wrapText=TRUE,halign="center",valign="center")
  add<-function(name,d,widths,small=FALSE,height=34) {
    openxlsx::addWorksheet(wb,name,gridLines=FALSE);openxlsx::writeData(wb,name,d,keepNA=FALSE)
    openxlsx::setColWidths(wb,name,cols=1:ncol(d),widths=widths)
    openxlsx::addStyle(wb,name,hs,rows=1,cols=1:ncol(d),gridExpand=TRUE)
    openxlsx::setRowHeights(wb,name,rows=1,heights=55)
    openxlsx::freezePane(wb,name,firstRow=TRUE)
    if(small&&nrow(d)) {
      bs<-openxlsx::createStyle(fontName="Arial",fontSize=10,wrapText=TRUE,valign="center")
      openxlsx::addStyle(wb,name,bs,rows=2:(nrow(d)+1),cols=1:ncol(d),gridExpand=TRUE)
      openxlsx::setRowHeights(wb,name,rows=2:(nrow(d)+1),heights=height)
    }
  }
  add("METODO",notes,c(35,150),TRUE,70)
  add("CONTROLES",controls,c(80,30),TRUE)
  if (isTRUE(origin$delivery)) {
    add("BALANCE_48",c$balance48,c(23,16,rep(25,8)))
    add("BALANCE_62",c$balance62,c(23,16,rep(25,8)))
  }
  add("TC_62",k$tc,c(14,55,rep(25,62)))
  add("BIS_62",k$bis,c(14,55,rep(25,62)))
  if(isTRUE(k$bis_bounded)) {
    add("BIS_DIRECTO",k$bis_original,c(14,55,rep(25,62)))
    add("BIS_AJUSTES",k$bis_bound_changes,c(14,55,65,28,28,28,45),TRUE,42)
  }
  add("BISO_62",k$biso,c(14,55,14,rep(25,62)))
  for(name in c("TC_62","BIS_62"))openxlsx::freezePane(wb,name,firstActiveRow=2,firstActiveCol=3)
  openxlsx::freezePane(wb,"BISO_62",firstActiveRow=2,firstActiveCol=4)
  keys<-paste(rep(CLASE$countries,each=62),rep(1:62,35),sep="_")
  z<-data.frame(Pais=rep(CLASE$countries,each=62),Sector62=rep(1:62,35),c$Z,check.names=FALSE)
  names(z)[-(1:2)]<-keys
  add("Z62_intermedios",z,c(24,14,rep(22,2170)));rm(z);invisible(gc(FALSE))
  add("X62",c$output_detail,c(14,14,55,14,28,28,25,28,50))
  add("REPARTO_RESPALDO",c$fallback_detail,c(24,16,24,16,30,48,48))
  m<-mapping[order(as.integer(mapping$code_wi)),c("code_za","code_wi")]
  m$Nombre_referencia<-c$output_detail$Nombre_referencia[as.integer(m$code_wi)]
  m$Aviso<-ifelse(as.integer(m$code_wi)==25,"UNIZAR: metales basicos; referencia WILIAM: hidrogeno.","")
  add("CORRESPONDENCIA",m,c(16,16,60,100),TRUE,42)
  provenance<-data.frame(Archivo=normalizePath(files),MD5=unname(tools::md5sum(files)))
  add("FUENTES",provenance,c(150,45),TRUE,45)
  openxlsx::saveWorkbook(wb,file.path(out,"Coeficientes_intermedios.xlsx"),overwrite=FALSE)
  invisible(controls)
}

fit_trade_block <- function(seed, target_rows, target_cols) {
  rows <- which(target_rows>0)
  cols <- which(target_cols>0)
  fit <- matrix(0,nrow(seed),ncol(seed))
  if (!length(rows) && !length(cols)) {
    attr(fit,"prior_weight") <- 0
    return(fit)
  }
  if (!length(rows) || !length(cols)) stop("Margenes incompatibles en un bloque del cuadre.")
  r <- target_rows[rows]
  c <- target_cols[cols]
  if (abs(sum(r)-sum(c))>1e-6) stop("Los margenes del cuadre no tienen el mismo total.")
  c <- c*(sum(r)/sum(c))
  start <- seed[rows,cols,drop=FALSE]
  prior <- outer(r,c)/sum(r)
  empty_rows <- rowSums(start)==0
  start[empty_rows,] <- prior[empty_rows,]
  empty_cols <- colSums(start)==0
  start[,empty_cols] <- prior[,empty_cols]
  # Keep the seed support unless it prevents convergence. In that case the
  # small prior gives positive support throughout the economically active block.
  for (prior_weight in c(0,1e-4)) {
    block <- start+prior_weight*prior
    for (iteration in 1:600) {
      block <- block*(r/rowSums(block))
      block <- sweep(block,2,c/colSums(block),"*")
      if (max(abs(rowSums(block)-r),abs(colSums(block)-c))<1e-7) {
        fit[rows,cols] <- block
        attr(fit,"prior_weight") <- prior_weight
        return(fit)
      }
    }
  }
  stop("El ajuste de los margenes no converge con las dos estructuras de partida.")
}

balance_compatibility_AX <- function(origin, mapping, reference) {
  sectors <- as.integer(mapping$code_za[match(1:62,as.integer(mapping$code_wi))])
  parent <- rep((0:34)*48L,each=62)+rep(sectors,35)
  aggregate <- function(v) as.numeric(rowsum(matrix(v),parent))
  x <- origin$X
  y <- aggregate(rowSums(reference$values[1:2170,2171:2380]))
  costs <- aggregate(colSums(reference$values[2171:2206,1:2170]))
  row_target <- x-y
  col_target <- x-costs
  if (abs(sum(row_target)-sum(col_target))>1e-6) stop("Los bloques fijos requieren totales intermedios diferentes por ventas y costes.")
  if (any(x==0 & abs(col_target)>1e-10)) stop("Una actividad de produccion cero presenta un margen intermedio incompatible con A.")
  neg_rows <- pmax(-row_target,0)
  neg_cols <- pmax(-col_target,0)
  if (sum(neg_cols)>sum(neg_rows)) {
    profile <- rowSums(pmax(origin$Z[,neg_cols>0,drop=FALSE],0))*(row_target>0)
    if (sum(profile)==0) profile <- pmax(row_target,0)
    neg_rows <- neg_rows+(sum(neg_cols)-sum(neg_rows))*profile/sum(profile)
  } else if (sum(neg_rows)>sum(neg_cols)) {
    profile <- colSums(pmax(origin$Z[neg_rows>0,,drop=FALSE],0))*(col_target>0)
    if (sum(profile)==0) profile <- pmax(col_target,0)
    neg_cols <- neg_cols+(sum(neg_rows)-sum(neg_cols))*profile/sum(profile)
  }
  seed <- origin$Z
  positive <- fit_trade_block(pmax(seed,0),pmax(row_target+neg_rows,0),pmax(col_target+neg_cols,0))
  negative <- fit_trade_block(abs(seed),neg_rows,neg_cols)
  z <- positive-negative
  tol <- 1e-6
  stopifnot(max(abs(rowSums(z)-row_target),abs(colSums(z)-col_target))<tol)
  stopifnot(abs(sum(abs(z[z<0]))-max(sum(pmax(-row_target,0)),sum(pmax(-col_target,0))))<tol)
  if (any(x==0)) stopifnot(max(abs(z[,x==0]))<tol)
  a <- sweep(z,2,ifelse(x==0,1,x),"/")
  keys <- paste(rep(CLASE$countries,each=48),rep(1:48,35),sep="_")
  dimnames(a) <- list(keys,keys)
  origin$cuadre <- data.frame(Control=c("Filas con margen Z negativo", "Columnas con margen Z negativo", "Celdas Z negativas antes",
    "Celdas Z negativas despues", "Importe absoluto de Z negativa antes", "Importe absoluto minimo de Z negativa despues",
    "Ceros del Z original activados", "Maximo residuo ventas a 48", "Maximo residuo costes a 48",
    "Columnas A con suma negativa", "Columnas A con suma mayor que 1 (tol. 1e-10)",
    "Peso previo adicional en ajuste positivo", "Peso previo adicional en ajuste negativo"),
    Valor=c(sum(row_target<0),sum(col_target<0),sum(seed<0),sum(z<0),sum(abs(seed[seed<0])),sum(abs(z[z<0])),sum(seed==0 & z!=0),
      max(abs(rowSums(z)-row_target)),max(abs(colSums(z)-col_target)),sum(colSums(a)< -1e-10),sum(colSums(a)>1+1e-10),
      attr(positive,"prior_weight"),attr(negative,"prior_weight")))
  origin$Z <- z
  origin$A <- a
  origin
}

close_compatibility_accounts <- function(values, x) {
  z <- values[1:2170,1:2170]
  old_y <- values[1:2170,2171:2380]
  old_tax <- values[2171:2205,1:2170]
  old_va <- values[2206,1:2170]

  final_target <- x-rowSums(z)
  final_positive <- pmax(old_y,0)
  final_negative <- pmin(old_y,0)
  historical_positive <- rowSums(final_positive)
  scale_y <- numeric(2170)
  has_positive <- historical_positive>0
  scale_y[has_positive] <- pmax(final_target[has_positive]-rowSums(final_negative)[has_positive],0)/historical_positive[has_positive]
  y <- final_negative+final_positive*scale_y
  unallocated <- final_target-rowSums(y)
  unallocated[abs(unallocated)<1e-8] <- 0

  cost_target <- x-colSums(z)
  if (any(cost_target< -1e-6)) stop("Para cerrar costes sin VA negativo, el consumo intermedio debe ser menor o igual a X. Usa --reparto-compras produccion.")
  target_nonnegative <- pmax(cost_target,0)
  positive_tax <- pmax(old_tax,0)
  negative_tax <- pmin(old_tax,0)
  net_tax_old <- colSums(old_tax)
  tax_scale <- rep(1,2170)
  capped <- net_tax_old>target_nonnegative
  tax_scale[capped] <- (target_nonnegative[capped]-colSums(negative_tax)[capped])/colSums(positive_tax)[capped]
  tax <- negative_tax+sweep(positive_tax,2,tax_scale,"*")
  net_tax <- colSums(tax)
  va <- pmax(target_nonnegative-net_tax,0)

  values[1:2170,2171:2380] <- y
  values[2171:2205,1:2170] <- tax
  values[2206,1:2170] <- va
  error_sales <- max(abs(rowSums(z)+rowSums(y)+unallocated-x))
  error_costs <- max(abs(colSums(z)+net_tax+va-x))
  stopifnot(max(error_sales,error_costs)<1e-6, all(is.finite(values)))
  summary <- data.frame(Control=c("Demanda final historica", "Demanda final requerida con X y A fijas", "Demanda final estimada en los seis usos",
    "Ajuste de demanda final sin asignar neto", "Ajustes sin asignar de signo positivo", "Ajustes sin asignar de signo negativo (importe absoluto)",
    "Productos con ajuste sin asignar positivo", "Productos con ajuste sin asignar negativo", "Productos con demanda final neta requerida negativa",
    "Actividades con impuestos reducidos", "Impuestos netos historicos", "Impuestos netos resultantes", "VA historico", "VA residual",
    "Maximo residuo ventas con ajuste sin asignar incluido", "Maximo residuo costes"),
    Valor=c(sum(old_y),sum(final_target),sum(y),sum(unallocated),sum(pmax(unallocated,0)),sum(pmax(-unallocated,0)),
      sum(unallocated>1e-6),sum(unallocated< -1e-6),sum(final_target< -1e-6),sum(net_tax_old-net_tax>1e-6),
      sum(net_tax_old),sum(net_tax),sum(old_va),sum(va),error_sales,error_costs))
  detail <- data.frame(Pais=rep(CLASE$countries,each=62),Sector62=rep(1:62,35),X=x,Y_historica=rowSums(old_y),
    Y_requerida=final_target,Y_estimada=rowSums(y),Ajuste_Y_no_asignado=unallocated,
    Impuestos_historicos=net_tax_old,Impuestos_estimados=net_tax,VA_historico=old_va,VA_estimado=va)
  list(values=values,unallocated=unallocated,summary=summary,detail=detail)
}

combine_compatibility_AX <- function(origin, converted, reference, close_accounts=FALSE) {
  values <- reference$values
  values[1:2170,1:2170] <- converted$Z
  stopifnot(identical(values[,2171:2380],reference$values[,2171:2380]),
    identical(values[2171:2206,],reference$values[2171:2206,]))
  closure <- NULL
  if (close_accounts) {
    if (converted$remaining_pairs>0) stop("El cierre con A fija requiere conservar todos los flujos de Z48. Usa --reparto-sin-pesos produccion.")
    closure <- close_compatibility_accounts(values,converted$X)
    values <- closure$values
    closure$values <- NULL
  }
  revenue_adjustment <- if (is.null(closure)) rep(0,2170) else closure$unallocated

  parent <- converted$parent
  aggregate <- function(v) as.numeric(rowsum(matrix(v),parent))
  y48 <- aggregate(rowSums(values[1:2170,2171:2380])+revenue_adjustment)
  tax48 <- aggregate(colSums(values[2171:2205,1:2170]))
  va48 <- aggregate(values[2206,1:2170])
  purchases48 <- colSums(origin$Z)
  sales48 <- rowSums(origin$Z)
  balance48 <- data.frame(Pais=rep(CLASE$countries,each=48),Sector48=rep(1:48,35),X=origin$X,
    Ventas_Z48=sales48,Demanda_final=y48,Ventas_menos_X=sales48+y48-origin$X,
    Compras_Z48=purchases48,Impuestos_netos=tax48,VA=va48,Costes_menos_X=purchases48+tax48+va48-origin$X)

  purchases62 <- colSums(converted$Z)
  ids <- which(tabulate(parent,1680)[parent]>1)
  output <- converted$output_detail
  parent_purchases <- purchases48[parent[ids]]
  child_output <- converted$X[ids]
  split <- data.frame(Pais=output$Pais[ids],Padre48=output$Padre48[ids],Sector62=output$Sector62[ids],
    X62=child_output,Cuota_X=output$Proporcion[ids],Compras_Z62=purchases62[ids],Compras_Z48=parent_purchases,
    Cuota_Z=ifelse(parent_purchases!=0,purchases62[ids]/parent_purchases,NA_real_),
    Compras_sobre_X=ifelse(child_output!=0,purchases62[ids]/child_output,NA_real_))
  split <- split[order(split$Compras_sobre_X,decreasing=TRUE,na.last=TRUE),]
  unsupported <- converted$fallback_detail
  recovered <- if (converted$fallback=="produccion") unsupported else NULL
  pending <- if (is.null(recovered)) unsupported else unsupported[FALSE,]
  sales_diff <- rowSums(values[1:2170,])+revenue_adjustment-converted$X
  cost_diff <- colSums(values[,1:2170])-converted$X
  input_share <- purchases62[converted$X>0]/converted$X[converted$X>0]
  checks <- data.frame(Control=c("Error relativo conservacion X al reagrupar", "Error relativo conservacion Z con reparto W",
    "Pares de Z48 sin reparto W", "Importe absoluto sin reparto W", "Porcentaje absoluto de Z48 sin reparto W",
    "Productos 48 con discrepancia ventas vs X (>1e-6)", "Actividades 48 con discrepancia costes vs X (>1e-6)",
    "Productos 62 con discrepancia ventas vs X (>1e-6)", "Actividades 62 con discrepancia costes vs X (>1e-6)",
    "Actividades 62 con consumos intermedios mayores que X (tol. 1e-10)", "Maximo consumo intermedio sobre X62",
    "Pares recuperados con reparto proporcional", "Importe absoluto recuperado con reparto proporcional"),
    Valor=c(converted$x_error,converted$z_error,nrow(pending),sum(abs(pending$Flujo_padre)),
      100*sum(abs(pending$Flujo_padre))/sum(abs(origin$Z)),
      sum(abs(balance48$Ventas_menos_X)>1e-6),sum(abs(balance48$Costes_menos_X)>1e-6),
      sum(abs(sales_diff)>1e-6),sum(abs(cost_diff)>1e-6),sum(input_share>1+1e-10),max(input_share),
      if (is.null(recovered)) 0L else nrow(recovered),sum(abs(recovered$Flujo_padre))))
  policy <- if (is.null(origin$cuadre)) "Solo se sustituye A; Z48 = A nueva por X historica." else
    "Ensayo firmado: se cuadra Z48 con el menor importe negativo compatible con X y los demas bloques fijos. La A entregada incorpora esos ajustes."
  weight_note <- if (is.null(recovered)) "Los flujos sin pesos se registran como pendientes." else
    "Si el bloque W es cero, el producto de las proporciones de produccion WILIAM de proveedor y comprador conserva el par de origen y destino."
  buyer_note <- if (converted$buyer_profile=="produccion")
    "Las compras de cada hijo siguen su cuota de produccion dentro del padre." else ""
  seller_note <- if (converted$seller_profile=="produccion")
    "Las ventas de cada hijo siguen su cuota de produccion dentro del padre. Se conservan los flujos por par de regiones y sectores padre." else
    "Se conservan las cuotas WILIAM de los hijos proveedores en cada par padre."
  accounts_note <- if (is.null(closure)) "Se conservan demanda final, impuestos y VA de la plantilla historica." else
    "Se reescala la demanda final positiva segun su perfil historico y se conservan sus valores negativos. El resto se identifica sin asignacion de uso ni destino. Solo se reducen impuestos positivos si el neto excede el margen. El VA se obtiene como residuo."
  list(values=values,output=converted$X,checks=checks,balance48=balance48,split_detail=split,pending=pending,recovered=recovered,cuadre=origin$cuadre,
    closure=closure,
    policy=paste(policy,"Regla de compatibilidad: WILIAM W normalizada",
      "en cada bloque padre, peso 1 para pares uno a uno.",weight_note,buyer_note,seller_note,
      accounts_note))
}

run_unizar_intermediate <- function(config) {
  if(is.null(config$produccion_referencia))stop("El modo unizar necesita --produccion-referencia Production.xlsx para repartir X.")
  if(!config$reparto_sin_pesos %in% c("produccion","ninguno"))stop("--reparto-sin-pesos debe ser produccion o ninguno.")
  files<-unique(c(config$entrada,config$matriz_a,config$produccion_referencia,config$correspondencia))
  checksum<-tools::md5sum(files)
  cat("1/4 Leyendo A y X de UNIZAR; referencia de proporciones de produccion...\n")
  if (is.null(config$matriz_a)) {
    origin <- read_unizar_AX(config$entrada)
  } else {
    cat("Ensayo con A externa. Se supone el orden de ambos ejes y la compatibilidad con X y W del libro de apoyo.\n")
    origin <- read_unizar_AX(config$entrada, config$matriz_a)
    origin$external_matrix <- config$matriz_a
  }
  mapping<-openxlsx::read.xlsx(config$correspondencia,sheet="Rafa_intermediate_wili")
  d<-openxlsx::read.xlsx(config$entrada,sheet="W",check.names=FALSE)
  # Reuse read_weights' canonical ordering without exporting a temporary workbook.
  key<-function(v){v<-gsub("[–—−]","-",toupper(trimws(v)));v<-gsub("\\.-\\.","-",v);v<-gsub("\\s*-\\s*","-",v);paste(canon(sub("-\\d+$","",v)),as.integer(sub(".*-(\\d+)$","\\1",v)),sep="_")}
  expected<-paste(rep(CLASE$countries,each=62),rep(1:62,35),sep="_")
  rows<-key(as.character(d[[1]]));cols<-key(names(d)[-1])
  if(anyDuplicated(rows)||anyDuplicated(cols)||!setequal(rows,expected)||!setequal(cols,expected))stop("Etiquetas W invalidas.")
  weights<-numeric_matrix(d[match(expected,rows),match(expected,cols)+1,drop=FALSE])
  reference<-read_reference_output(config$produccion_referencia)
  cat("2/4 Convirtiendo Z y X de 48 a 62 sectores...\n")
  converted<-convert_intermediate_AX(origin$Z,origin$X,mapping,weights,reference,config$reparto_sin_pesos)
  cat("3/4 Calculando TC, BIS y BISO solo intermedios...\n")
  coefficients<-calculate_intermediate_coefficients(converted$Z,converted$X)
  if(config$bis_acotado=="si")coefficients<-bound_bis(coefficients)
  cat("4/4 Guardando un unico libro de resultados...\n")
  controls<-write_intermediate_results(origin,converted,coefficients,mapping,files,config$salida)
  stopifnot(identical(checksum,tools::md5sum(files)))
  print(controls,row.names=FALSE)
  invisible(list(origin=origin,converted=converted,coefficients=coefficients,controls=controls))
}

run_unizar_delivery <- function(config) {
  files <- c(config$entrada,config$pesos,config$correspondencia,config$produccion_referencia)
  checksum <- tools::md5sum(files)
  cat("1/3 Leyendo Z, X, VA e y de la entrega...\n")
  origin <- read_unizar_delivery(config$entrada)
  mapping <- openxlsx::read.xlsx(config$correspondencia,sheet="Rafa_intermediate_wili")
  labels <- CLASE$sectors
  if (any(as.integer(mapping$code_za)==16L & as.integer(mapping$code_wi)==25L,na.rm=TRUE))
    labels[25] <- "MANUFACTURE_BASIC_METALS"

  cat("2/3 Repartiendo Z, X, VA e y a 62 sectores...\n")
  converted <- convert_intermediate_AX(origin$Z,origin$X,mapping,read_weights(config$pesos),
    read_reference_output(config$produccion_referencia),"produccion","produccion","produccion")
  parent <- converted$parent
  shares <- converted$output_detail$Proporcion
  converted$VA <- origin$VA[parent]*shares
  converted$Y <- origin$Y[parent,,drop=FALSE]*shares
  va_error <- max(abs(as.numeric(rowsum(matrix(converted$VA),parent))-origin$VA)/pmax(1,abs(origin$VA)))
  y_error <- max(abs(rowsum(converted$Y,parent)-origin$Y)/pmax(1,abs(origin$Y)))
  if (max(va_error,y_error)>1e-10) stop("Falla la conservacion de VA o y por padre.")
  keys <- paste(rep(CLASE$countries,each=62),rep(1:62,35),sep="_")
  dimnames(converted$Z) <- list(keys,keys)
  dimnames(converted$Y) <- list(keys,paste0("USO_POSICION_",seq_len(ncol(converted$Y))))
  converted$output_detail$Nombre_referencia <- rep(labels,35)

  balance <- function(z,x,va,y,n) {
    sales <- rowSums(z)
    purchases <- colSums(z)
    y_total <- rowSums(y)
    ratio <- numeric(length(x))
    ratio[x>0] <- purchases[x>0]/x[x>0]
    data.frame(Region=rep(CLASE$countries,each=n),Sector=rep(seq_len(n),35),X=x,
      Ventas_intermedias=sales,Y=y_total,Ventas_menos_X=sales+y_total-x,
      Compras_intermedias=purchases,VA=va,Costes_menos_X=purchases+va-x,Suma_A=ratio)
  }
  converted$balance48 <- balance(origin$Z,origin$X,origin$VA,origin$Y,48)
  converted$balance62 <- balance(converted$Z,converted$X,converted$VA,converted$Y,62)
  b48 <- converted$balance48
  b62 <- converted$balance62
  converted$delivery_checks <- data.frame(Control=c("Maximo residuo absoluto de ventas a 48", "Maximo residuo absoluto de costes a 48",
    "Maximo residuo absoluto de ventas a 62", "Maximo residuo absoluto de costes a 62", "Balance de ventas agregado a 48",
    "Filas con ventas intermedias superiores a X a 48 (tol. 1e-6)", "Filas con ventas intermedias superiores a X a 62 (tol. 1e-6)",
    "Columnas de A con suma mayor que uno a 48 (tol. 1e-10)", "Columnas de A con suma mayor que uno a 62 (tol. 1e-10)",
    "Error relativo al reagrupar Y", "Error relativo al reagrupar VA"),
    Valor=c(max(abs(b48$Ventas_menos_X)),max(abs(b48$Costes_menos_X)),max(abs(b62$Ventas_menos_X)),max(abs(b62$Costes_menos_X)),
      sum(b48$Ventas_menos_X),sum(b48$Ventas_intermedias-b48$X>1e-6),sum(b62$Ventas_intermedias-b62$X>1e-6),
      sum(b48$Suma_A>1+1e-10),sum(b62$Suma_A>1+1e-10),y_error,va_error))
  coefficients <- calculate_intermediate_coefficients(converted$Z,converted$X,labels)
  if (config$bis_acotado=="si") coefficients <- bound_bis(coefficients)
  cat("3/3 Exportando los controles y parametros intermedios...\n")
  controls <- write_intermediate_results(origin,converted,coefficients,mapping,files,config$salida)
  saveRDS(list(Z=converted$Z,X=setNames(converted$X,keys),VA=setNames(converted$VA,keys),Y=converted$Y),
    file.path(config$salida,"MRIO62_entrega_UNIZAR.rds"))
  stopifnot(identical(checksum,tools::md5sum(files)))
  print(controls,row.names=FALSE)
  invisible(list(origin=origin,converted=converted,coefficients=coefficients,controls=controls))
}

# 6. Un unico punto de ejecucion -------------------------------------------
main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  root <- if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[1]))) else getwd()
  default_template <- file.path(root, "entradas", "compatibilidad", "plantilla_historica", "preserved_nonZ_baseline.RData")
  config <- list(entrada = file.path(root, "entradas", "unizar", "data_UNIZAR.xlsx"), tipo_entrada = "unizar", hoja = NULL, negativos = "existencias",
    tecnologia = "cero", calibracion = "ninguna", production = NULL, trade = NULL, pp = NULL,
    matriz_a = NULL,
    cuadrar_a = "no",
    cerrar_cuentas = "no",
    reparto_compras = "wiliam",
    reparto_ventas = "wiliam",
    paises = file.path(root, "entradas", "unizar", "data_UNIZAR.xlsx"), plantilla = default_template,
    pesos = file.path(root, "entradas", "compatibilidad", "W.xlsx"), correspondencia = file.path(root, "entradas", "unizar", "Correspondance_final.xlsx"),
    referencia_completa = NULL, produccion_referencia = file.path(root, "referencias", "Production.xlsx"), reparto_sin_pesos = "produccion", solo_parametros = "no", bis_acotado = "no",
    salida = file.path(root, "resultados", format(Sys.time(), "%Y%m%d-%H%M%S")))
  if ("--ayuda" %in% args) {
    cat("Por defecto: A/X/W de entradas/unizar/data_UNIZAR.xlsx -> Z/X de 62 sectores -> TC/BIS/BISO solo intermedios.\n",
      "Rscript IO_completa_a_resultados.R [--salida carpeta_nueva]\n",
      "--tipo-entrada entrega --entrada libro.xlsx lee Z, X, VA e y (105 columnas posicionales), reparte por X y calcula parametros intermedios.\n",
      "--matriz-a libro.xlsx usa su hoja A (1680 x 1680) y la X y las listas de --entrada. En modo intermedia conserva los bloques de la plantilla y usa --pesos de compatibilidad.\n",
      "--cuadrar-a minimo-negativo prueba un cuadre firmado de A en modo intermedia con --matriz-a.\n",
      "--cerrar-cuentas si conserva A y X y estima demanda final, impuestos y VA en modo intermedia con --matriz-a.\n",
      "--reparto-compras produccion mantiene las cuotas de los proveedores y reparte las compras de los hijos en proporcion a X; solo con --tipo-entrada intermedia --matriz-a.\n",
      "--reparto-ventas produccion reparte las ventas de los hijos en proporcion a X; solo con --tipo-entrada intermedia --matriz-a.\n",
      "Produccion de referencia predeterminada: referencias/Production.xlsx; se cambia con --produccion-referencia.\n",
      "--reparto-sin-pesos produccion usa proporciones de produccion cuando W carece de reparto; ninguno conserva esos casos pendientes.\n",
      "El modo unizar no lee demanda final, impuestos o VA y genera un unico libro.\n",
      "Modos anteriores: --tipo-entrada intermedia usa entradas/compatibilidad/IO_unizar.xlsx, o --tipo-entrada completa --entrada tabla_completa.\n",
      "--plantilla tabla_completa.RData aporta los bloques fijos de demanda final, impuestos y VA.\n",
      "Entrada sin etiquetas: --paises data_UNIZAR.xlsx aporta solo country list; no se leen A ni X_CGE.\n",
      "Opcional: --tipo-entrada completa --entrada IO_completa.RData; si tiene 48 sectores requiere --referencia-completa para repartir todos los bloques.\n")
    cat("Excel con varias hojas: --hoja MRIO_test selecciona la matriz de entrada. Se admiten las dos filas de cabecera de Calculos limpio MRIO_JN.xlsx.\n")
    cat("Con --tipo-entrada completa, --solo-parametros si guarda solo las tablas de coeficientes en un libro y no exporta otra IO.\n")
    cat("--bis-acotado si recorta solo BIS a [0,1] y conserva BIS_DIRECTO y BIS_AJUSTES; no modifica la MRIO ni BISO.\n")
    cat("--negativos conservar calcula sobre todos los flujos originales, informa rangos REVISAR y no aplica ajustes de signo. Por defecto: existencias (ajuste anterior).\n")
    cat("--tecnologia wiliam aplica la tecnologia media de Production a columnas TC nulas. --production archivo --trade archivo [--pp archivo] comparan con los originales sin sustituir lo calculado.\n")
    cat("BIS incluye 62 usos intermedios y 6 finales de la MRIO; no genera la variable independiente BASE_IMPORT_SHARES_FINAL_DEMAND a precios de comprador.\n")
    cat("--calibracion ninguna (predeterminada), coque (factor 1.05 en un flujo) o coque-niquel (ademas anula tres domesticos). Solo modifica una copia para BIS/BISO; TC e IO_final conservan la entrada. Es experimental, sin validacion para otras IO.\n")
    return(invisible(NULL))
  }
  if (length(args) %% 2) stop("Las opciones son pares --nombre valor. Usa --ayuda.")
  if (length(args)) for (i in seq(1, length(args), by = 2)) {
    key <- gsub("-", "_", sub("^--", "", args[i]))
    if (!key %in% names(config)) stop("Opcion desconocida: ", args[i])
    config[[key]] <- args[i + 1]
  }
  lib <- file.path(root, ".runtime", "R-library")
  if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Instala openxlsx en R: install.packages('openxlsx').")
  if (!config$tipo_entrada %in% c("unizar", "intermedia", "completa", "entrega"))
    stop("--tipo-entrada debe ser unizar, intermedia, completa o entrega.")
  if (!is.null(config$matriz_a) && !config$tipo_entrada %in% c("unizar","intermedia"))
    stop("--matriz-a se admite en los modos unizar e intermedia.")
  if (!config$cuadrar_a %in% c("no","minimo-negativo")) stop("--cuadrar-a debe ser no o minimo-negativo.")
  if (!config$cerrar_cuentas %in% c("si","no")) stop("--cerrar-cuentas debe ser si o no.")
  if (config$cerrar_cuentas=="si" && (config$tipo_entrada!="intermedia" || is.null(config$matriz_a) || config$cuadrar_a!="no"))
    stop("--cerrar-cuentas requiere modo intermedia con --matriz-a y A fija.")
  if (config$cuadrar_a!="no" && (config$tipo_entrada!="intermedia" || is.null(config$matriz_a))) stop("--cuadrar-a requiere modo intermedia con --matriz-a.")
  if (!config$reparto_compras %in% c("wiliam","produccion")) stop("--reparto-compras debe ser wiliam o produccion.")
  if (!config$reparto_ventas %in% c("wiliam","produccion")) stop("--reparto-ventas debe ser wiliam o produccion.")
  if (config$reparto_compras!="wiliam" && (config$tipo_entrada!="intermedia" || is.null(config$matriz_a)))
    stop("--reparto-compras requiere modo intermedia con --matriz-a.")
  if (config$reparto_ventas!="wiliam" && (config$tipo_entrada!="intermedia" || is.null(config$matriz_a)))
    stop("--reparto-ventas requiere modo intermedia con --matriz-a.")
  if(!config$solo_parametros %in% c("si","no"))stop("--solo-parametros debe ser si o no.")
  if(!config$bis_acotado %in% c("si","no"))stop("--bis-acotado debe ser si o no.")
  if(config$tipo_entrada=="entrega") {
    if (!"--entrada" %in% args) stop("Indica la entrega corregida con --entrada.")
    return(run_unizar_delivery(config))
  }
  if(config$tipo_entrada=="unizar")return(run_unizar_intermediate(config))
  compatibility_AX <- config$tipo_entrada=="intermedia" && !is.null(config$matriz_a)
  sector_labels <- CLASE$sectors
  if(config$tipo_entrada=="intermedia" && !compatibility_AX && !"--entrada" %in% args)config$entrada<-file.path(root,"entradas","compatibilidad","IO_unizar.xlsx")
  cat("1/4 Leyendo IO de origen (", config$tipo_entrada, ")...\n", sep = "")
  if (config$tipo_entrada == "intermedia" && !is.null(config$hoja)) stop("--hoja se utiliza con --tipo-entrada completa.")
  if (compatibility_AX) {
    origin <- read_unizar_AX(config$entrada,config$matriz_a)
    origin$ns <- 48L
    origin$intermediate <- TRUE
    origin$note <- "A externa con el orden de la A historica en ambos ejes; X historica de UNIZAR."
    files <- c(config$matriz_a,config$entrada,config$produccion_referencia)
  } else {
    origin <- if (config$tipo_entrada == "intermedia") read_intermediate_io(config$entrada, config$paises) else read_full_io(config$entrada, config$hoja)
    files <- c(config$entrada, origin$metadata_files)
  }
  mapping <- weights <- reference <- NULL
  if (origin$ns == 48) {
    mapping <- openxlsx::read.xlsx(config$correspondencia, sheet = "Rafa_intermediate_wili")
    weights <- read_weights(config$pesos)
    if (isTRUE(origin$intermediate)) {
      if (identical(normalizePath(config$plantilla, mustWork = FALSE), normalizePath(default_template, mustWork = FALSE))) {
        checksum_file <- sub("[.]RData$", ".md5", default_template)
        if (!file.exists(checksum_file) || !identical(unname(tools::md5sum(config$plantilla)), readLines(checksum_file, warn = FALSE))) stop("La plantilla fija falta o ha cambiado.")
        files <- c(files, checksum_file)
      }
      reference <- read_full_io(config$plantilla)
      files <- c(files, config$pesos, config$correspondencia, config$plantilla)
    } else {
      if (is.null(config$referencia_completa)) stop("Falta --referencia-completa para distribuir todos los bloques de una entrada completa de 48 sectores.")
      reference <- read_full_io(config$referencia_completa)
      files <- c(files, config$pesos, config$correspondencia, config$referencia_completa)
    }
  }
  if (origin$ns == 62L) cat("2/4 IO ya de 62 sectores: se conservan sus bloques; no se ejecuta conversion 48 -> 62.\n") else cat("2/4 Convirtiendo IO de ", origin$ns, " a 62 sectores...\n", sep = "")
  if (compatibility_AX) {
    if (any(as.integer(mapping$code_za)==16L & as.integer(mapping$code_wi)==25L,na.rm=TRUE)) {
      sector_labels[25] <- "MANUFACTURE_BASIC_METALS"
    }
    if (config$cuadrar_a=="minimo-negativo") {
      origin <- balance_compatibility_AX(origin,mapping,reference)
      print(origin$cuadre,row.names=FALSE)
    }
    x_reference <- read_reference_output(config$produccion_referencia)
    fallback <- if ("--reparto-sin-pesos" %in% args) config$reparto_sin_pesos else "ninguno"
    repartition <- convert_intermediate_AX(origin$Z,origin$X,mapping,weights,x_reference,
      fallback,config$reparto_compras,config$reparto_ventas)
    converted <- combine_compatibility_AX(origin,repartition,reference,config$cerrar_cuentas=="si")
    rm(repartition); invisible(gc(FALSE))
    print(converted$checks,row.names=FALSE)
    if (!is.null(converted$closure)) print(converted$closure$summary,row.names=FALSE)
  } else converted <- convert_full_io(origin, mapping, weights, reference)
  cat("3/4 Calculando TC, BIS y BISO...\n")
  coefficients <- calculate_coefficients(converted$values, config$negativos, config$tecnologia, config$calibracion,
    converted$output,sector_labels,converted$closure$unallocated)
  if(config$bis_acotado=="si")coefficients<-bound_bis(coefficients)
  reference_audit <- NULL
  if (!is.null(config$production) || !is.null(config$trade)) {
    reference_audit <- compare_model_references(coefficients, config$production, config$trade, config$pp)
    print(reference_audit$summary, row.names = FALSE)
    set_reference_status <- function(row, label, match_name) {
      s <- reference_audit$summary[grepl(match_name, reference_audit$summary$Control), , drop = FALSE]
      if (!nrow(s)) return(invisible(NULL))
      coefficients$summary$Control[row] <<- label
      coefficients$summary$Incidencias[row] <<- sum(s$Fuera_tolerancia)
      coefficients$summary$Max_error[row] <<- max(s$Max_error, na.rm = TRUE)
      coefficients$summary$Resultado[row] <<- if (any(s$Resultado == "REVISAR")) "REVISAR" else if (any(s$Referencia_vacia > 0)) "PENDIENTE" else if (any(s$Diferencias_1e_10 > 0)) "DENTRO_TOLERANCIA" else "BIEN"
    }
    set_reference_status(7, "TC frente a Production final", "TC calculado frente a final")
    set_reference_status(8, "BIS y BISO frente a Trade (bloques comparables)", "^BIS .*frente a Trade$|^BISO intermedios frente a Trade$")
    set_reference_status(9, "Fuente PP de BISO final frente a Trade", "comprobacion de fuente")
    files <- c(files, config$production, config$trade, config$pp)
  }
  if(config$solo_parametros=="no")print(coefficients$summary, row.names = FALSE)
  if(config$solo_parametros=="si") {
    cat("4/4 Guardando solo coeficientes en un libro...\n")
    write_parameter_results(converted$values,coefficients,files,config$salida,
      if (compatibility_AX) "A" else config$hoja, if (compatibility_AX) converted else NULL)
  } else {
    cat("4/4 Guardando dos libros finales...\n")
    write_results(converted$values, coefficients, converted, files, config$salida, origin$note,
      if (compatibility_AX) "A" else config$hoja, reference_audit)
  }
  if (!is.null(origin$cuadre)) saveRDS(origin$A,file.path(config$salida,"A48_cuadrada.rds"))
  if (!is.null(converted$closure)) {
    table <- io_table(converted$values,sector_labels)
    table$AJUSTE_CIERRE_Y <- c(converted$closure$unallocated,rep(0,36))
    saveRDS(list(tabla=table,X=converted$output),file.path(config$salida,"MRIO62_cierre_estimado.rds"))
  }
  invisible(list(io = converted$values, coefficients = coefficients))
}
if (sys.nframe() == 0L) main()

#' @importFrom utils head
NULL

#' Read a data file (CSV, Excel, or TSV) into a data frame
#'
#' @param path Path to data file.
#' @return A data.frame.
#' @export
load_data <- function(path) {
  ext <- tolower(tools::file_ext(path))

  df <- switch(ext,
    "xlsx" = , "xls" = , "xlsm" = , "xlsb" = , "ods" = readxl::read_excel(path),
    "tsv"  = , "txt" = readr::read_tsv(path, show_col_types = FALSE),
    readr::read_csv(path, show_col_types = FALSE)
  )
  df <- as.data.frame(df)
  names(df) <- trimws(names(df))

  # Drop fully-empty unnamed columns (common Excel artifact)
  unnamed <- grep("^\\.{3}\\d+$|^$", names(df))
  for (i in unnamed) {
    if (all(is.na(df[[i]]))) df[[i]] <- NULL
  }
  df
}


# --- Column detection helpers ------------------------------------------------

.LAT_NAMES <- c("lat", "latitude", "y", "northing")
.LON_NAMES <- c("lon", "long", "longitude", "lng", "x", "easting")
.OUTCOME_CANDIDATES <- c("outcome", "case_control", "status", "class",
                          "target", "casecontrol")
.CASE_VALUES <- c("case", "cases", "1", "yes", "true", "positive", "present")

.is_float_col <- function(df, col, n = 5L) {
  vals <- head(trimws(as.character(stats::na.omit(df[[col]]))), n)
  # Accept plain decimals AND scientific notation (e.g. 1.1e1, -2.5E-3)
  length(vals) > 0 &&
    all(grepl("^-?\\d+(\\.\\d+)?([eE][-+]?\\d+)?$", vals, perl = TRUE))
}

.is_pair_col <- function(df, col, n = 5L) {
  vals <- head(trimws(as.character(stats::na.omit(df[[col]]))), n)
  pat <- "^[\\[\\(]?\\s*-?\\d+(\\.\\d+)?\\s*,\\s*-?\\d+(\\.\\d+)?\\s*[\\]\\)]?$"
  length(vals) > 0 && all(grepl(pat, vals, perl = TRUE))
}


#' Detect latitude and longitude columns
#'
#' @param df A data.frame.
#' @param lat_col Optional explicit latitude column name.
#' @param lon_col Optional explicit longitude column name.
#' @return A named list with elements `lat` (column name), `lon` (column
#'   name), and `df` (the data.frame; may have new `.spotmapr_auto_lat`/`.spotmapr_auto_lon`
#'   columns appended if a combined column was split).
#' @export
detect_lat_lon <- function(df, lat_col = NULL, lon_col = NULL) {
  cols <- names(df)

  if (!is.null(lat_col) && !is.null(lon_col)) {
    missing <- setdiff(c(lat_col, lon_col), cols)
    if (length(missing) > 0)
      stop("Columns not found: ", paste(missing, collapse = ", "),
           call. = FALSE)
    return(list(lat = lat_col, lon = lon_col, df = df))
  }

  # Combined "lat,lon" column?
  for (col in cols) {
    if (.is_pair_col(df, col)) {
      txt <- gsub("[\\[\\]()]", "", as.character(df[[col]]))
      parts <- strsplit(txt, ",")
      v1 <- as.numeric(trimws(vapply(parts, `[`, character(1), 1)))
      v2 <- as.numeric(trimws(vapply(parts, `[`, character(1), 2)))

      max1 <- max(abs(v1), na.rm = TRUE)
      max2 <- max(abs(v2), na.rm = TRUE)

      if (max1 > 90 && max2 <= 90) {
        lat_s <- v2; lon_s <- v1
      } else if (max2 > 90 && max1 <= 90) {
        lat_s <- v1; lon_s <- v2
      } else {
        lat_s <- ifelse(abs(v1) > abs(v2), v2, v1)
        lon_s <- ifelse(abs(v1) > abs(v2), v1, v2)
      }
      df[[".spotmapr_auto_lat"]] <- lat_s
      df[[".spotmapr_auto_lon"]] <- lon_s
      return(list(lat = ".spotmapr_auto_lat", lon = ".spotmapr_auto_lon", df = df))
    }
  }

  # Separate numeric columns by name
  numeric_cols <- cols[vapply(cols, function(c) .is_float_col(df, c), logical(1))]
  found_lat <- NULL
  found_lon <- NULL

  for (c in numeric_cols) {
    cl <- tolower(c)
    if (any(vapply(.LAT_NAMES, function(n) grepl(n, cl, fixed = TRUE), logical(1))))
      found_lat <- c
    if (any(vapply(.LON_NAMES, function(n) grepl(n, cl, fixed = TRUE), logical(1))))
      found_lon <- c
  }

  if (!is.null(found_lat) && !is.null(found_lon))
    return(list(lat = found_lat, lon = found_lon, df = df))

  if (length(numeric_cols) == 2)
    return(list(lat = numeric_cols[1], lon = numeric_cols[2], df = df))

  stop("Could not auto-detect lat/lon columns. Available columns: ",
       paste(cols, collapse = ", "),
       ". Pass lat_col and lon_col explicitly.", call. = FALSE)
}


#' Detect the outcome column and case value
#'
#' @param df A data.frame.
#' @param outcome_col Optional explicit outcome column name.
#' @param case_value Optional explicit value that represents a case.
#' @return A named list with elements `outcome_col` and `case_value`.
#' @export
detect_outcome <- function(df, outcome_col = NULL, case_value = NULL) {
  cols <- names(df)

  if (!is.null(outcome_col) && !(outcome_col %in% cols))
    stop("outcome_col '", outcome_col, "' not found. Available: ",
         paste(cols, collapse = ", "), call. = FALSE)

  if (is.null(outcome_col)) {
    for (cand in .OUTCOME_CANDIDATES) {
      matches <- cols[grepl(cand, tolower(cols), fixed = TRUE)]
      if (length(matches) > 0) {
        outcome_col <- matches[1]
        break
      }
    }
  }

  if (is.null(outcome_col))
    stop("Could not find outcome column. Available columns: ",
         paste(cols, collapse = ", "),
         ". Pass outcome_col explicitly.", call. = FALSE)

  norm <- tolower(trimws(as.character(df[[outcome_col]])))
  norm[norm == "na" | norm == "nan"] <- NA
  values <- unique(stats::na.omit(norm))

  if (is.null(case_value)) {
    hit <- values[values %in% .CASE_VALUES]
    case_value <- if (length(hit) > 0) hit[1] else if (length(values) > 0) values[1] else NULL
  } else {
    # Validate user-supplied case_value actually exists in the column
    case_value_norm <- tolower(trimws(as.character(case_value)))
    if (!(case_value_norm %in% values)) {
      stop("case_value '", case_value, "' not found in column '",
           outcome_col, "'. Available values: ",
           paste(values, collapse = ", "),
           ". Note: matching is case-insensitive.",
           call. = FALSE)
    }
  }

  if (is.null(case_value))
    stop("No values found in outcome column '", outcome_col, "'.", call. = FALSE)

  list(outcome_col = outcome_col, case_value = tolower(trimws(case_value)))
}

# ==============================================================================
# Variability helpers
# ==============================================================================

#' Format numeric columns for rule evaluation
#'
#' @param data Data frame
#' @param n_sigfig Number of significant figures
#' @return Data frame with numeric columns formatted as character
#' @noRd
format_numeric_for_rules <- function(data, n_sigfig) {
  num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  if (length(num_cols) == 0) {
    return(data)
  }

  for (col in num_cols) {
    data[[col]] <- vapply(
      data[[col]],
      function(x) {
        if (is.na(x)) return(NA_character_)
        format_sigfig_pad(x, n_sigfig)
      },
      character(1)
    )
  }

  data
}

#' Format numeric values with fixed significant figures
#' @noRd
format_sigfig_pad <- function(x, n_sigfig) {
  if (is.na(x)) return(NA_character_)
  if (is.character(x)) return(x)

  base <- trimws(formatC(x, digits = n_sigfig, format = "g"))
  if (grepl("[eE]", base)) {
    return(base)
  }

  sign <- if (startsWith(base, "-")) "-" else ""
  core <- sub("^[-+]", "", base)

  if (!grepl("\\.", core)) {
    sig <- gsub("^0+", "", core)
    if (sig == "") sig <- "0"
    sig_count <- nchar(sig)
    if (sig_count < n_sigfig) {
      core <- paste0(
        core,
        ".",
        paste(rep("0", n_sigfig - sig_count), collapse = "")
      )
    }
    return(paste0(sign, core))
  }

  parts <- strsplit(core, "\\.", fixed = FALSE)[[1]]
  int <- parts[1]
  frac <- parts[2]
  digits_all <- paste0(int, frac)
  sig <- sub("^0+", "", digits_all)
  if (sig == "") sig <- "0"
  sig_count <- nchar(sig)
  if (sig_count < n_sigfig) {
    frac <- paste0(
      frac,
      paste(rep("0", n_sigfig - sig_count), collapse = "")
    )
  }
  paste0(sign, int, ".", frac)
}

#' Build variability display column using spec rules
#' @noRd
build_variability <- function(data, spec) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    stop("spec must be a TableSpec object")
  }

  rules <- spec@variability_rules
  args <- lapply(rules, function(q) {
    rlang::eval_tidy(q, data = data)
  })

  dplyr::case_when(!!!args)
}

#' Build variability for parameter tables
#' @noRd
build_variability_parameter <- function(data, spec) {
  data_fmt <- format_numeric_for_rules(data, spec@n_sigfig)
  if (!"fixed" %in% names(data_fmt)) {
    data_fmt$fixed <- NA
  }
  build_variability(data_fmt, spec)
}

#' Build variability for comparison tables
#' @noRd
build_variability_comparison <- function(data, spec, suffix_cols) {
  data_fmt <- format_numeric_for_rules(data, spec@n_sigfig)
  model_indices <- get_comparison_model_indices(names(data_fmt), suffix_cols)

  if (length(model_indices) == 0) {
    return(data)
  }

  for (idx in model_indices) {
    data_tmp <- data_fmt
    suffixed <- grep(paste0("_", idx, "$"), names(data_tmp), value = TRUE)
    for (col in suffixed) {
      base <- sub(paste0("_", idx, "$"), "", col)
      data_tmp[[base]] <- data_tmp[[col]]
    }
    data[[paste0("variability_", idx)]] <- build_variability(data_tmp, spec)
  }

  data
}

#' @noRd
wants_variability_column <- function(spec) {
  "variability" %in%
    c(spec@columns %||% character(0), spec@add_columns %||% character(0)) &&
    !"variability" %in% spec@drop_columns
}

#' @noRd
wants_variability_components <- function(spec) {
  any(c("cv", "corr", "sd") %in%
    c(spec@columns %||% character(0), spec@add_columns %||% character(0)))
}

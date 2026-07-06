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
        if (is.na(x)) {
          return(NA_character_)
        }
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
  if (is.na(x)) {
    return(NA_character_)
  }
  if (is.character(x)) {
    return(x)
  }
  if (!is.finite(x)) {
    return(as.character(x))
  }

  # Fixed notation (never scientific), matching hyperion's own sigfig formatter,
  # so cells and footnotes of the same table stay consistent. `format = "fg"`
  # keeps significant figures without switching to `1e+03`-style output.
  base <- trimws(formatC(signif(x, n_sigfig), digits = n_sigfig, format = "fg"))

  sign <- if (startsWith(base, "-")) "-" else ""
  core <- sub("^[-+]", "", base)

  if (!grepl("\\.", core)) {
    sig <- gsub("^0+", "", core)
    if (sig == "") {
      sig <- "0"
    }
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
  if (sig == "") {
    sig <- "0"
  }
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
#'
#' Each rule is a two-sided formula `condition ~ value`. The **condition** is
#' evaluated against the raw numeric data (`data`) so comparisons like `cv > 30`
#' are numeric, and the **value** is evaluated against the display-formatted data
#' (`data_fmt`) so labels like `sprintf("(CV = %s%%)", cv)` show rounded figures.
#' Evaluating both against formatted strings would make `cv > 30` a lexicographic
#' string comparison.
#' @noRd
build_variability <- function(data, data_fmt, spec) {
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("spec must be a TableSpec object")
  }

  rules <- spec@variability_rules
  rule_vars <- unique(unlist(lapply(rules, function(q) {
    all.vars(rlang::quo_get_expr(q))
  })))
  for (v in setdiff(rule_vars, names(data))) {
    data[[v]] <- NA
  }
  for (v in setdiff(rule_vars, names(data_fmt))) {
    data_fmt[[v]] <- NA
  }

  n <- nrow(data)
  out <- rep(NA_character_, n)
  unset <- rep(TRUE, n)

  for (q in rules) {
    expr <- rlang::quo_get_expr(q)
    env <- rlang::quo_get_env(q)
    if (!rlang::is_formula(expr) || length(expr) != 3L) {
      rlang::abort("variability rules must be two-sided formulas (`condition ~ value`)")
    }

    cond <- as.logical(rlang::eval_tidy(rlang::new_quosure(expr[[2]], env), data = data))
    cond[is.na(cond)] <- FALSE
    cond <- rep_len(cond, n)

    val <- as.character(rlang::eval_tidy(rlang::new_quosure(expr[[3]], env), data = data_fmt))
    val <- rep_len(val, n)

    take <- unset & cond
    out[take] <- val[take]
    unset[take] <- FALSE
  }

  out
}

#' Build variability for parameter tables
#' @noRd
build_variability_parameter <- function(data, spec) {
  data_fmt <- format_numeric_for_rules(data, spec@n_sigfig)
  build_variability(data, data_fmt, spec)
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
    raw_tmp <- data
    fmt_tmp <- data_fmt
    suffixed <- grep(paste0("_", idx, "$"), names(fmt_tmp), value = TRUE)
    for (col in suffixed) {
      base <- sub(paste0("_", idx, "$"), "", col)
      fmt_tmp[[base]] <- fmt_tmp[[col]]
      if (col %in% names(raw_tmp)) {
        raw_tmp[[base]] <- raw_tmp[[col]]
      }
    }
    data[[paste0("variability_", idx)]] <- build_variability(raw_tmp, fmt_tmp, spec)
  }

  data
}

#' @noRd
wants_variability_column <- function(spec) {
  base_cols <- spec@columns %||% spec@default_columns
  "variability" %in%
    c(base_cols, spec@add_columns %||% character(0)) &&
    !"variability" %in% spec@drop_columns
}

#' @noRd
wants_variability_components <- function(spec) {
  base_cols <- spec@columns %||% spec@default_columns
  any(
    c("cv", "corr", "sd") %in%
      c(base_cols, spec@add_columns %||% character(0))
  )
}

#' @noRd
variability_plan <- function(spec) {
  if (is.null(spec) || !S7::S7_inherits(spec, TableSpec)) {
    return(list(
      wants_variability = FALSE,
      wants_components = FALSE,
      build_variability = FALSE
    ))
  }

  wants_variability <- wants_variability_column(spec)
  wants_components <- wants_variability_components(spec)

  list(
    wants_variability = wants_variability,
    wants_components = wants_components,
    build_variability = wants_variability && !wants_components
  )
}

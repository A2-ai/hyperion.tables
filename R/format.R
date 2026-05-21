# ==============================================================================
# Table helpers
# ==============================================================================

#' Find columns that are all NA or empty
#'
#' @param df Data frame to check
#' @return Character vector of column names that are all NA/empty
#' @noRd
find_empty_columns <- function(df) {
  is_all_empty <- function(x) {
    if (is.character(x)) {
      all(is.na(x) | x == "")
    } else {
      all(is.na(x))
    }
  }
  names(df)[vapply(df, is_all_empty, logical(1))]
}

#' Get CI percent from spec
#' @noRd
get_ci_pct <- function(spec, default = 95) {
  if (!is.null(spec) && "ci" %in% names(S7::props(spec))) {
    return(round(spec@ci@level * 100))
  }
  default
}

#' Check if a fixed flag is TRUE (handles logical or character)
#' @noRd
is_fixed_true <- function(x) {
  if (is.logical(x)) {
    return(!is.na(x) & x)
  }
  if (is.numeric(x)) {
    return(!is.na(x) & x != 0)
  }
  x_chr <- toupper(trimws(as.character(x)))
  !is.na(x_chr) & x_chr %in% c("TRUE", "T", "1", "YES", "Y")
}

#' Add display-friendly fixed_fmt columns
#' @noRd
add_fixed_display_columns <- function(df, fixed_cols) {
  if (length(fixed_cols) == 0) {
    return(df)
  }

  for (fc in fixed_cols) {
    if (!fc %in% names(df)) {
      next
    }
    fmt_col <- if (fc == "fixed") {
      "fixed_fmt"
    } else {
      sub("^fixed_", "fixed_fmt_", fc)
    }
    df[[fmt_col]] <- dplyr::if_else(is_fixed_true(df[[fc]]), "Fixed", "")
  }

  df
}

#' Blank CI values for fixed parameters
#' @noRd
blank_ci_for_fixed <- function(df) {
  blank_ci_cols <- function(data, fixed_col, ci_low, ci_high) {
    if (!fixed_col %in% names(data)) {
      return(data)
    }
    fixed_true <- is_fixed_true(data[[fixed_col]])
    if (any(fixed_true)) {
      if (ci_low %in% names(data)) {
        data[[ci_low]][fixed_true] <- NA_real_
      }
      if (ci_high %in% names(data)) {
        data[[ci_high]][fixed_true] <- NA_real_
      }
    }
    data
  }

  df <- blank_ci_cols(df, "fixed", "ci_low", "ci_high")

  fixed_cols <- grep("^fixed_\\d+$", names(df), value = TRUE)
  for (fc in fixed_cols) {
    idx <- sub("^fixed_", "", fc)
    df <- blank_ci_cols(
      df,
      fc,
      paste0("ci_low_", idx),
      paste0("ci_high_", idx)
    )
  }

  df
}

# ==============================================================================
# Footnote helpers
# ==============================================================================

#' Build a label map for parameter table columns
#'
#' @param ci_pct Confidence interval percentage
#' @return Named list of labels for gt::cols_label()
#' @noRd
build_parameter_label_map <- function(ci_pct) {
  list(
    name = "Parameter",
    description = "",
    symbol = "Symbol",
    unit = "Unit",
    estimate = "Estimate",
    ci_low = sprintf("%d%% CI", ci_pct),
    ci_high = sprintf("%d%% CI", ci_pct),
    variability = "",
    rse = "RSE (%)",
    shrinkage = "Shrinkage (%)",
    fixed = "Fixed",
    stderr = "SE"
  )
}

#' Build a label map for summary table columns
#'
#' @return Named list of labels for gt::cols_label()
#' @noRd
build_summary_label_map <- function() {
  list(
    model = "Model",
    based_on = "Reference",
    description = "Description",
    n_parameters = "No. Params",
    problem = "Problem",
    number_data_records = "Records",
    number_subjects = "Subjects",
    number_obs = "Observations",
    estimation_method = "Method",
    estimation_time = "Est. Time",
    covariance_time = "Cov. Time",
    postprocess_time = "Post Time",
    function_evaluations = "Func. Evals",
    significant_digits = "Sig. Digits",
    ofv = "OFV",
    dofv = "$\\Delta$OFV",
    condition_number = "Cond. No.",
    termination_status = "Termination",
    pvalue = "p-value",
    df = "df"
  )
}

#' Apply CI label overrides based on requested columns
#' @noRd
adjust_ci_labels <- function(label_map, spec, ci_pct) {
  if (is.null(spec)) {
    return(label_map)
  }

  # Get effective columns (requested minus dropped)
  dropped <- expand_ci_drop_columns(spec@drop_columns)
  effective_cols <- setdiff(get_spec_columns(spec), dropped)

  ci_low_shown <- "ci_low" %in% effective_cols
  ci_high_shown <- "ci_high" %in% effective_cols

  if (ci_low_shown && !ci_high_shown) {
    label_map$ci_low <- sprintf("Lower %d%% CI", ci_pct)
  }
  if (ci_high_shown && !ci_low_shown) {
    label_map$ci_high <- sprintf("Upper %d%% CI", ci_pct)
  }
  label_map
}

#' Detect which statistics are used in a parameter table
#'
#' @param params Parameter data frame (after apply_table_spec or comparison)
#' @param spec Optional TableSpec; when supplied, flags for ci/cv/sd/corr are
#'   suppressed when the corresponding columns won't render, so footnotes don't
#'   reference columns the reader can't see.
#' @return Named list of logicals indicating which stats are present
#' @noRd
detect_table_statistics <- function(params, spec = NULL) {
  if (is.null(spec)) {
    ci_ok <- cv_ok <- sd_ok <- corr_ok <- TRUE
  } else {
    base_cols <- spec@columns %||% spec@default_columns
    effective <- setdiff(
      c(base_cols, spec@add_columns %||% character(0)),
      spec@drop_columns %||% character(0)
    )
    vary <- "variability" %in% effective
    cv_ok <- vary || "cv" %in% effective
    sd_ok <- vary || "sd" %in% effective
    corr_ok <- vary || "corr" %in% effective
    dropped <- expand_ci_drop_columns(spec@drop_columns %||% character(0))
    ci_ok <- !all(c("ci_low", "ci_high") %in% dropped)
  }
  has_cv_col <- "cv" %in% names(params) && cv_ok
  has_sd_col <- "sd" %in% names(params) && sd_ok
  has_corr_col <- "corr" %in% names(params) && corr_ok
  has_transforms <- "transforms" %in% names(params)
  col_names <- names(params)

  # Helper to check for CV with specific kind and transform
  cv_with <- function(kind, transforms) {
    has_cv_col &&
      has_transforms &&
      any(
        !is.na(params$cv) &
          params$kind == kind &
          tolower(params$transforms) %in% tolower(transforms)
      )
  }

  # Check for any CI columns (ci_low, ci_high, ci_low_1, ci_high_2, etc.)
  ci_cols <- grep("^ci_(low|high)", col_names, value = TRUE)
  has_ci <- ci_ok &&
    length(ci_cols) > 0 &&
    any(vapply(ci_cols, function(col) any(!is.na(params[[col]])), logical(1)))

  # Check for RSE columns (handle both regular and comparison table column names)
  has_rse_regular <- "rse" %in% col_names && any(!is.na(params$rse))
  has_rse_comparison <- ("rse_1" %in% col_names && any(!is.na(params$rse_1))) ||
    ("rse_2" %in% col_names && any(!is.na(params$rse_2)))

  list(
    # Column presence
    has_ci = has_ci,
    has_rse = has_rse_regular || has_rse_comparison,
    has_stderr = "stderr" %in% col_names && any(!is.na(params$stderr)),
    has_shrinkage = "shrinkage" %in%
      names(params) &&
      any(!is.na(params$shrinkage)),

    # Merged column statistics (cv/sd/corr)
    has_cv = has_cv_col && any(!is.na(params$cv)),
    has_sd = has_sd_col &&
      any(!is.na(params$sd) & is.na(params$cv) & is.na(params$corr)),
    has_corr = has_corr_col && any(!is.na(params$corr)),

    # CV formula detection by kind and transform
    # Theta LogAddErr: sqrt(exp(Est^2) - 1) * 100
    has_theta_logadderr_cv = cv_with("THETA", "logadderr"),

    # Omega LogNormal: sqrt(exp(Est) - 1) * 100
    has_omega_lognormal_cv = cv_with("OMEGA", "lognormal"),

    # Omega Proportional: sqrt(Est) * 100
    has_omega_proportional_cv = cv_with("OMEGA", "proportional"),

    # Sigma LogNormal/LogAddErr: sqrt(exp(Est) - 1) * 100
    has_sigma_lognormal_cv = cv_with("SIGMA", c("lognormal", "logadderr")),

    # Sigma Proportional: sqrt(Est) * 100
    has_sigma_proportional_cv = cv_with("SIGMA", "proportional")
  )
}

#' Build equations footnote content
#'
#' Generates equation footnotes (CI formula, % Change, CV formulas) based on
#' what statistics are present in the table.
#'
#' @param stats Named list from detect_table_statistics()
#' @param ci_pct Confidence interval percentage (e.g., 95)
#' @param comparison_stats Optional list with has_pct_change for comparison tables
#' @param summary_stats Optional list with dofv_excluded for summary tables
#' @return List of markdown character strings for footnotes, or NULL if none
#' @noRd
build_equations_footnote <- function(
  stats,
  ci_pct,
  comparison_stats = NULL,
  summary_stats = NULL
) {
  footnotes <- list()

  # CI formula
  if (stats$has_ci) {
    footnotes <- c(
      footnotes,
      list(
        sprintf(
          "%d%% CI: $\\mathrm{Estimate} \\pm z_{%.3g} \\cdot \\mathrm{SE}$",
          ci_pct,
          (1 - ci_pct / 100) / 2
        )
      )
    )
  }

  # % Change formula for comparison tables
  if (!is.null(comparison_stats) && isTRUE(comparison_stats$has_pct_change)) {
    footnotes <- c(
      footnotes,
      list(
        "% Change: $\\frac{\\mathrm{Estimate}_{\\mathrm{model}} - \\mathrm{Estimate}_{\\mathrm{ref}}}{\\mathrm{Estimate}_{\\mathrm{ref}}} \\cdot 100$"
      )
    )
  }

  # CV formulas - group by formula type to avoid duplication

  # Formula: sqrt(exp(Est^2) - 1) * 100 (Theta LogAddErr)
  if (stats$has_theta_logadderr_cv) {
    footnotes <- c(
      footnotes,
      list(
        paste0(
          "CV% for log-additive error $\\theta$: ",
          "$\\sqrt{\\exp(\\mathrm{Estimate}^2) - 1} \\times 100$"
        )
      )
    )
  }

  # Formula: sqrt(exp(Est) - 1) * 100 (Omega LogNormal, Sigma LogNormal/LogAddErr)
  if (stats$has_omega_lognormal_cv || stats$has_sigma_lognormal_cv) {
    parts <- character(0)
    if (stats$has_omega_lognormal_cv) {
      parts <- c(parts, "log-normal $\\Omega$")
    }
    if (stats$has_sigma_lognormal_cv) {
      parts <- c(parts, "log-normal $\\Sigma$")
    }
    footnotes <- c(
      footnotes,
      list(
        sprintf(
          "CV%% for %s: $\\sqrt{\\exp(\\mathrm{Estimate}) - 1} \\times 100$",
          paste(parts, collapse = " and ")
        )
      )
    )
  }

  # Formula: sqrt(Est) * 100 (Omega Proportional, Sigma Proportional)
  if (stats$has_omega_proportional_cv || stats$has_sigma_proportional_cv) {
    parts <- character(0)
    if (stats$has_omega_proportional_cv) {
      parts <- c(parts, "$\\Omega$")
    }
    if (stats$has_sigma_proportional_cv) {
      parts <- c(parts, "$\\Sigma$")
    }
    footnotes <- c(
      footnotes,
      list(
        sprintf(
          "CV%% for proportional %s: $\\sqrt{\\mathrm{Estimate}} \\times 100$",
          paste(parts, collapse = " and ")
        )
      )
    )
  }

  if (length(footnotes) == 0) {
    return(NULL)
  }

  footnotes
}

#' Build abbreviations footnote content
#'
#' Generates the abbreviations section for table footnotes based on
#' what statistics are present in the table.
#'
#' @param stats Named list from detect_table_statistics()
#' @param comparison_stats Optional list with has_ofv and has_lrt for comparison tables
#' @param summary_stats Optional list with has_ofv, has_dofv, has_cond_num for summary tables
#' @return Character vector with "Abbreviations:" header + wrapped lines, or NULL
#' @noRd
build_abbreviations_footnote <- function(
  stats,
  comparison_stats = NULL,
  summary_stats = NULL
) {
  abbrevs <- character(0)
  if (stats$has_ci) {
    abbrevs <- c(abbrevs, "CI = confidence intervals")
  }
  if (stats$has_rse) {
    abbrevs <- c(abbrevs, "RSE = relative standard error")
  }
  if (stats$has_ci || stats$has_stderr) {
    abbrevs <- c(abbrevs, "SE = standard error")
  }
  if (stats$has_cv) {
    abbrevs <- c(abbrevs, "CV = coefficient of variation")
  }
  if (stats$has_sd) {
    abbrevs <- c(abbrevs, "SD = standard deviation")
  }
  if (stats$has_corr) {
    abbrevs <- c(abbrevs, "Corr = correlation")
  }

  # Comparison table abbreviations
  if (!is.null(comparison_stats)) {
    if (isTRUE(comparison_stats$has_ofv)) {
      abbrevs <- c(abbrevs, "OFV = Objective Function Value")
    }
    if (isTRUE(comparison_stats$has_lrt)) {
      abbrevs <- c(abbrevs, "LRT = Likelihood Ratio Test")
      abbrevs <- c(abbrevs, "df = degrees of freedom")
    }
  }

  # Summary table abbreviations
  if (!is.null(summary_stats)) {
    if (isTRUE(summary_stats$has_ofv)) {
      abbrevs <- c(abbrevs, "OFV = Objective Function Value")
    }
    if (isTRUE(summary_stats$has_dofv)) {
      abbrevs <- c(abbrevs, "\u0394OFV = change in OFV from reference model")
    }
    if (isTRUE(summary_stats$has_cond_num)) {
      abbrevs <- c(abbrevs, "Cond. No. = Condition Number")
    }
    if (isTRUE(summary_stats$has_pvalue)) {
      abbrevs <- c(abbrevs, "p-value from LRT (Likelihood Ratio Test)")
      abbrevs <- c(abbrevs, "df = degrees of freedom")
    }
  }

  result <- character(0)

  if (length(abbrevs) > 0) {
    abbrev_text <- paste(abbrevs, collapse = "; ")
    wrapped_abbrevs <- strwrap(abbrev_text, width = 80)
    result <- c("Abbreviations:", wrapped_abbrevs)
  }

  if (length(result) == 0) {
    return(NULL)
  }

  result
}

# ==============================================================================
# Formatting helpers (Greek symbols, markdown)
# ==============================================================================

#' Convert parameter kind to Greek symbol in LaTeX math notation
#'
#' Returns raw LaTeX (without $..$ delimiters) for use in param_symbol_md().
#' @noRd
greek_to_latex <- function(kind, random_effect) {
  stopifnot(length(kind) == length(random_effect))

  n <- length(kind)
  out <- rep(NA_character_, n)

  # THETA: enumerate in order of appearance
  is_theta <- !is.na(kind) & kind == "THETA"
  if (any(is_theta)) {
    theta_idx <- seq_len(sum(is_theta))
    out[is_theta] <- sprintf("\\theta_{%d}", theta_idx)
  }

  # Helper: from random_effect -> "row,col" for lower triangle
  # e.g. "ETA1" -> "1,1"; "ETA1:ETA2" -> "2,1"
  make_cov_idx <- function(re) {
    nums_list <- regmatches(re, gregexpr("\\d+", re))
    vapply(
      nums_list,
      function(nums_chr) {
        if (length(nums_chr) == 0L) {
          return("")
        }
        nums <- as.integer(nums_chr)

        if (length(nums) == 1L) {
          sprintf("%d,%d", nums, nums) # ETA1 -> (1,1)
        } else {
          r <- max(nums[1:2]) # ETA1:ETA2 -> (2,1)
          c <- min(nums[1:2])
          sprintf("%d,%d", r, c)
        }
      },
      character(1)
    )
  }

  # OMEGA: ETA... -> Omega
  is_omega <- !is.na(kind) & kind == "OMEGA" & !is.na(random_effect)
  if (any(is_omega)) {
    idx_str <- make_cov_idx(random_effect[is_omega])
    out[is_omega] <- sprintf("\\Omega_{(%s)}", idx_str)
  }

  # SIGMA: EPS... -> Sigma
  is_sigma <- !is.na(kind) & kind == "SIGMA" & !is.na(random_effect)
  if (any(is_sigma)) {
    idx_str <- make_cov_idx(random_effect[is_sigma])
    out[is_sigma] <- sprintf("\\Sigma_{(%s)}", idx_str)
  }

  out
}

#' Build parameter symbols as LaTeX math expressions
#'
#' Wraps in exp() for LogNormal and logistic for Logit transforms.
#' Returns complete LaTeX math expressions wrapped in $..$.
#' @noRd
param_symbol_md <- function(kind, random_effect, transforms) {
  base_sym <- greek_to_latex(kind, random_effect)

  tr <- transforms
  if (is.factor(tr)) {
    tr <- as.character(tr)
  }

  # Build raw LaTeX expression (without $..$ delimiters)
  latex_expr <- dplyr::case_when(
    !is.na(tr) & tolower(tr) == "lognormal" ~ paste0("\\exp(", base_sym, ")"),
    !is.na(tr) & tolower(tr) == "logit" ~
      paste0("1/(1 + \\exp(-", base_sym, "))"),
    TRUE ~ base_sym
  )

  # Wrap in $..$ for inline LaTeX math (only for non-NA values)
  dplyr::if_else(
    !is.na(latex_expr),
    paste0("$", latex_expr, "$"),
    NA_character_
  )
}

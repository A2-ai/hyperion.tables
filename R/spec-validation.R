# ==============================================================================
# Shared spec validation helpers
# ==============================================================================

#' @noRd
valid_table_columns <- function() {
  c(
    "kind",
    "name",
    "random_effect",
    "description",
    "symbol",
    "unit",
    "estimate",
    "stderr",
    "diagonal",
    "ci_low",
    "ci_high",
    "fixed",
    "variability",
    "cv",
    "corr",
    "sd",
    "rse",
    "shrinkage"
  )
}

#' @noRd
valid_summary_columns <- function() {
  c(
    # From tree metadata
    "based_on",
    "description",
    # From parameters
    "n_parameters",
    # From run_details
    "problem",
    "number_data_records",
    "number_subjects",
    "number_obs",
    "estimation_method",
    "estimation_time",
    "covariance_time",
    "postprocess_time",
    "function_evaluations",
    "significant_digits",
    # From minimization_results
    "ofv",
    "dofv",
    "condition_number",
    "termination_status",
    # Computed LRT fields
    "pvalue",
    "df"
  )
}

#' @noRd
comparison_suffix_columns <- function() {
  c(
    "symbol",
    "unit",
    "estimate",
    "rse",
    "ci_low",
    "ci_high",
    "variability",
    "stderr",
    "fixed",
    "shrinkage"
  )
}

#' @noRd
expand_ci_alias <- function(cols) {
  if (is.null(cols) || length(cols) == 0) {
    return(cols)
  }
  if ("ci" %in% cols) {
    replace_idx <- which(cols == "ci")
    cols <- unlist(
      lapply(seq_along(cols), function(i) {
        if (i %in% replace_idx) c("ci_low", "ci_high") else cols[[i]]
      }),
      use.names = FALSE
    )
    cols <- cols[!duplicated(cols)]
  }
  cols
}

#' @noRd
expand_ci_drop_columns <- function(drop_columns) {
  if (length(drop_columns) == 0) {
    return(drop_columns)
  }

  ci_aliases <- c("ci", "ci_1", "ci_2", "ci_left", "ci_right")
  if (any(drop_columns %in% ci_aliases)) {
    drop_columns <- unique(c(drop_columns, "ci_low", "ci_high"))
  }

  drop_columns
}

#' @noRd
table_spec_valid_columns <- function() {
  c(valid_table_columns(), "ci", "pct_change")
}

#' @noRd
summary_spec_valid_columns <- function() {
  valid_summary_columns()
}

#' @noRd
validate_columns_in_set <- function(cols, valid, label) {
  if (is.null(cols)) {
    return(NULL)
  }
  bad <- setdiff(cols, valid)
  if (length(bad) == 0) {
    return(NULL)
  }
  sprintf(
    "%s must be in: %s\n  Got: %s",
    label,
    paste(valid, collapse = ", "),
    paste(bad, collapse = ", ")
  )
}

#' @noRd
table_drop_columns_invalid <- function(cols) {
  if (length(cols) == 0) {
    return(character(0))
  }

  comparison_cols <- comparison_suffix_columns()
  comparison_drop_cols <- c(
    paste0(comparison_cols, "_1"),
    paste0(comparison_cols, "_2"),
    paste0(comparison_cols, "_left"),
    paste0(comparison_cols, "_right")
  )
  ci_aliases <- c("ci", "ci_1", "ci_2", "ci_left", "ci_right")
  valid_drop_cols <- c(
    valid_table_columns(),
    comparison_drop_cols,
    "pct_change",
    ci_aliases
  )

  comparison_pattern <- paste0(
    "^(",
    paste(comparison_cols, collapse = "|"),
    ")_\\d+$"
  )
  ci_num_pattern <- "^ci_\\d+$"
  pct_change_pattern <- "^pct_change_\\d+$"

  is_valid_drop <- function(col) {
    col %in%
      valid_drop_cols ||
      grepl(comparison_pattern, col) ||
      grepl(ci_num_pattern, col) ||
      grepl(pct_change_pattern, col)
  }

  cols[!vapply(cols, is_valid_drop, logical(1))]
}

#' @noRd
table_drop_columns_message <- function(bad, label = "@drop_columns") {
  main_drop_cols <- c(valid_table_columns(), "pct_change", "ci")
  sprintf(
    paste(
      "%s must be in: %s",
      "For comparisons, use numeric suffixes (_1, _2, _3, ...) or _left/_right for two-model tables.",
      "Got: %s",
      sep = "\n"
    ),
    label,
    paste(main_drop_cols, collapse = ", "),
    paste(bad, collapse = ", ")
  )
}

#' @noRd
validate_table_drop_columns <- function(cols, label = "@drop_columns") {
  bad <- table_drop_columns_invalid(cols)
  if (length(bad) == 0) {
    return(NULL)
  }
  table_drop_columns_message(bad, label = label)
}

#' Validate p-value threshold
#'
#' @param threshold Numeric or NULL
#' @return NULL if valid, or error message string if invalid
#' @noRd
validate_pvalue_threshold <- function(threshold) {
  if (is.null(threshold)) {
    return(NULL)
  }
  if (
    length(threshold) != 1 ||
      is.na(threshold) ||
      threshold <= 0 ||
      threshold >= 1
  ) {
    return(sprintf(
      "@pvalue_threshold must be NULL or a number between 0 and 1. Got: %s",
      threshold
    ))
  }
  NULL
}

#' @noRd
validate_ofv_decimals <- function(value, label = "@n_decimals_ofv") {
  if (length(value) != 1) {
    return(sprintf(
      "%s must be NA or a non-negative whole number. Got: %s",
      label,
      value
    ))
  }
  if (is.na(value)) {
    return(NULL)
  }
  if (value < 0 || value != floor(value)) {
    return(sprintf(
      "%s must be NA or a non-negative whole number. Got: %s",
      label,
      value
    ))
  }
  NULL
}

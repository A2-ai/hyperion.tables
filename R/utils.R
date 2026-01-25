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

#' Check if a value is a non-empty scalar character string
#' @noRd
is_scalar_nonempty_char <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

#' Normalize a groupname column to a valid column name or NULL
#' @noRd
normalize_groupname_col <- function(groupname_col, data_names) {
  if (!is_scalar_nonempty_char(groupname_col)) {
    return(NULL)
  }
  if (!groupname_col %in% data_names) {
    return(NULL)
  }
  groupname_col
}

#' Select columns for missing text substitution
#' @noRd
missing_apply_columns <- function(data, apply_to) {
  if (is.null(apply_to) || apply_to == "all") {
    return(names(data))
  }
  if (apply_to == "numeric") {
    return(names(data)[vapply(data, is.numeric, logical(1))])
  }
  if (apply_to == "character") {
    return(names(data)[vapply(
      data,
      function(x) is.character(x) || is.factor(x),
      logical(1)
    )])
  }
  names(data)
}

#' Compute LRT p-value from a test statistic and degrees of freedom
#'
#' @param test_stat Numeric test statistic (assumed >= 0)
#' @param df Degrees of freedom
#' @return Numeric p-value, or NA if inputs are invalid
#' @noRd
lrt_pvalue <- function(test_stat, df) {
  if (is.na(test_stat) || is.na(df) || df <= 0) {
    return(NA_real_)
  }

  stats::pchisq(test_stat, df = df, lower.tail = FALSE)
}

#' Format p-value for display
#'
#' @param pval Numeric p-value
#' @param n_sigfig Significant digits
#' @param scientific Logical indicating scientific notation
#' @return Character string, or NA if input is NA
#' @noRd
format_pvalue_string <- function(pval, n_sigfig, scientific, threshold = NULL) {
  if (is.na(pval)) {
    return(NA_character_)
  }

  if (!is.null(threshold) && pval < threshold) {
    return(sprintf("< %s", threshold))
  }

  if (scientific) {
    format(pval, scientific = TRUE, digits = n_sigfig)
  } else {
    as.character(signif(pval, n_sigfig))
  }
}

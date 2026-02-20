# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

#' Null-coalescing operator
#'
#' Returns the right-hand side if the left-hand side is NULL, otherwise returns the left-hand side.
#'
#' @param x Left-hand side value
#' @param y Right-hand side value (default if x is NULL)
#' @return x if x is not NULL, otherwise y
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Check that a suggested package is installed
#'
#' @param pkg Package name
#' @param reason Why the package is needed (appended to error message)
#' @noRd
check_suggested <- function(pkg, reason = NULL) {
  if (requireNamespace(pkg, quietly = TRUE)) return(invisible())
  msg <- paste0("Package '", pkg, "' is required")
  if (!is.null(reason)) msg <- paste(msg, reason)
  rlang::abort(paste0(msg, "\nInstall it with: rv add ", pkg))
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

#' Safely extract a field from a model summary
#'
#' Returns NA if the summary is NULL or the field is NULL/missing.
#'
#' @param summary A model summary list (or NULL)
#' @param field Character name of the field to extract
#' @return The field value, or NA if not available
#' @noRd
safe_summary_field <- function(summary, field) {
  if (is.null(summary) || is.null(summary[[field]])) return(NA)
  summary[[field]]
}

#' Compute LRT p-value from a test statistic and degrees of freedom
#'
#' Negative values of `test_stat` (i.e., the child model has a higher OFV than
#' the parent) are valid and will produce p-values near 1.
#'
#' @param test_stat Numeric test statistic (parent OFV - child OFV).
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

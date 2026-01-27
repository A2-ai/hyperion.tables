# ==============================================================================
# Shared spec validation helpers
# ==============================================================================

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

#' @noRd
validate_table_footnote_order <- function(order, label = "@footnote_order") {
  valid <- c("summary_info", "equations", "abbreviations")

  if (!is.null(order)) {
    if (length(order) == 0) {
      return(sprintf("%s must be NULL or have at least one section", label))
    }
    bad <- setdiff(order, valid)
    if (length(bad) > 0) {
      return(sprintf(
        "%s must be in: %s\n  Got: %s",
        label,
        paste(valid, collapse = ", "),
        paste(bad, collapse = ", ")
      ))
    }
  }
  NULL
}

#' @noRd
validate_summary_footnote_order <- function(order, label = "@footnote_order") {
  if (!is.null(order) && !identical(order, "abbreviations")) {
    return(sprintf("%s must be NULL or 'abbreviations'", label))
  }
  NULL
}

# ==============================================================================
# Shared Rendering + Preprocessing for HyperionTable
# ==============================================================================

#' Apply shared formatting rules to a HyperionTable
#'
#' Applies CI merging, numeric formatting, and missing text rules and returns
#' a formatted data frame. Does not mutate the underlying HyperionTable.
#'
#' @param table HyperionTable object
#' @return Data frame ready for rendering
#' @export
apply_formatting <- function(table) {
  if (!S7::S7_inherits(table, HyperionTable)) {
    rlang::abort("table must be a HyperionTable object")
  }

  data <- table@data
  numeric_cols <- table@numeric_cols

  summary_overrides <- apply_summary_render_overrides(table, data, numeric_cols)
  data <- summary_overrides$data
  numeric_cols <- summary_overrides$numeric_cols

  data <- format_numeric_for_table(data, table, numeric_cols)

  data <- build_variability_for_table(data, table)

  data <- merge_ci_for_table(data, table)

  data <- apply_missing_text_policy(
    data,
    missing_text = table@missing_text,
    apply_to = table@missing_apply_to
  )

  groupname_col <- normalize_groupname_col(table@groupname_col, names(data))
  visible_cols <- setdiff(names(data), table@hide_cols)
  data_cols <- visible_cols
  if (!is.null(groupname_col)) {
    data_cols <- union(groupname_col, data_cols)
  }
  dplyr::select(data, dplyr::all_of(data_cols))
}

#' @noRd
format_numeric_for_table <- function(data, table, numeric_cols) {
  if (table@table_type != "summary") {
    all_numeric <- names(data)[vapply(data, is.numeric, logical(1))]
    numeric_cols <- union(numeric_cols, all_numeric)
  }

  format_numeric_columns_shared(
    data,
    numeric_cols = numeric_cols,
    n_sigfig = table@n_sigfig,
    formatter = NULL
  )
}

#' @noRd
build_variability_for_table <- function(data, table) {
  spec <- table@source_spec
  plan <- variability_plan(spec)
  if (plan$build_variability && table@table_type == "parameter") {
    data$variability <- build_variability_parameter(data, spec)
  }
  data
}

#' @noRd
merge_ci_for_table <- function(data, table) {
  if (isTRUE(table@ci@merge)) {
    data <- merge_ci_columns_data(
      data,
      ci_merges = table@ci_merges,
      ci_missing_text = table@ci@missing_text,
      ci_missing_rows = table@ci_missing_rows,
      n_sigfig = table@n_sigfig,
      pattern = table@ci@pattern,
      formatter = NULL
    )
  }
  data
}

#' Apply summary-specific display overrides
#' @noRd
apply_summary_render_overrides <- function(table, data, numeric_cols) {
  if (!S7::S7_inherits(table, HyperionTable)) {
    rlang::abort("table must be a HyperionTable object")
  }
  if (table@table_type != "summary") {
    return(list(data = data, numeric_cols = numeric_cols))
  }

  spec <- table@source_spec

  if ("pvalue" %in% names(data)) {
    use_scientific <- spec@pvalue_scientific
    pval_threshold <- spec@pvalue_threshold
    n_sig <- spec@n_sigfig
    df_dropped <- !is.null(spec) &&
      !is.null(spec@drop_columns) &&
      "df" %in% spec@drop_columns
    merge_df <- !df_dropped && "df" %in% names(data)
    data$pvalue <- vapply(
      seq_len(nrow(data)),
      function(i) {
        pval <- data$pvalue[i]
        if (is.na(pval)) {
          return(NA_character_)
        }
        format_p <- format_pvalue_string(
          pval,
          n_sig,
          use_scientific,
          pval_threshold
        )
        if (!merge_df) return(format_p)
        df_val <- data$df[i]
        if (is.na(df_val)) {
          return(NA_character_)
        }
        sprintf("%s (df = %d)", format_p, df_val)
      },
      character(1)
    )
    if (merge_df && "df" %in% names(data)) {
      data <- dplyr::select(data, -dplyr::all_of("df"))
    }
  }

  ofv_cols <- intersect(c("ofv", "dofv"), names(data))
  if (length(ofv_cols) > 0) {
    for (col in ofv_cols) {
      if (is.numeric(data[[col]])) {
        data[[col]] <- vapply(
          data[[col]],
          function(x) {
            if (is.na(x)) return("")
            formatC(x, digits = spec@n_decimals_ofv, format = "f")
          },
          character(1)
        )
      }
    }
  }

  numeric_cols <- setdiff(numeric_cols, ofv_cols)
  list(data = data, numeric_cols = numeric_cols)
}

#' Merge CI columns into a single display column
#'
#' @param data Data frame
#' @param ci_merges List of CI merge specifications
#' @param ci_missing_text Text for missing CI values in specific rows
#' @param ci_missing_rows Logical vector or row indices indicating which rows
#'   should show ci_missing_text when CI is NA. Others show empty string.
#' @param n_sigfig Number of significant figures for formatting
#' @param pattern sprintf pattern for CI display
#' @param formatter Optional formatter function(x, n_sigfig)
#' @return Data frame with CI columns merged
#' @noRd
merge_ci_columns_data <- function(
  data,
  ci_merges,
  ci_missing_text = "-",
  ci_missing_rows = NULL,
  n_sigfig = 3,
  pattern = "[%s, %s]",
  formatter = NULL
) {
  if (length(ci_merges) == 0) {
    return(data)
  }

  is_missing_row <- function(i) {
    if (is.null(ci_missing_rows)) {
      return(FALSE)
    }
    if (is.logical(ci_missing_rows)) {
      return(isTRUE(ci_missing_rows[i]))
    }
    i %in% ci_missing_rows
  }

  format_ci_value <- function(x) {
    if (!is.null(formatter)) {
      return(formatter(x, n_sigfig))
    }
    format_value(x, n_sigfig)
  }

  for (merge in ci_merges) {
    ci_low <- merge$ci_low
    ci_high <- merge$ci_high

    if (!all(c(ci_low, ci_high) %in% names(data))) {
      next
    }

    merged_values <- vapply(
      seq_len(nrow(data)),
      function(i) {
        low <- data[[ci_low]][i]
        high <- data[[ci_high]][i]
        if (is.na(low) || is.na(high)) {
          if (is_missing_row(i)) {
            return(ci_missing_text)
          }
          return("")
        }
        sprintf(pattern, format_ci_value(low), format_ci_value(high))
      },
      character(1)
    )

    data[[ci_low]] <- merged_values
    data <- dplyr::select(data, -dplyr::all_of(ci_high))
  }

  data
}

#' Format a single numeric value
#' @noRd
format_value <- function(x, n_sigfig = 3) {
  format_sigfig_pad(x, n_sigfig)
}

#' Format numeric columns for display
#'
#' @param data Data frame
#' @param numeric_cols Character vector of numeric column names
#' @param n_sigfig Number of significant figures
#' @param formatter Optional formatter function(x, n_sigfig)
#' @param skip_cols Columns to skip formatting
#' @return Data frame with formatted columns
#' @noRd
format_numeric_columns_shared <- function(
  data,
  numeric_cols,
  n_sigfig,
  formatter = NULL,
  skip_cols = character(0)
) {
  if (length(numeric_cols) == 0) {
    return(data)
  }

  format_numeric_value <- function(x) {
    if (is.na(x)) return(NA_character_)
    if (!is.null(formatter)) {
      return(formatter(x, n_sigfig))
    }
    format_sigfig_pad(x, n_sigfig)
  }

  for (col in numeric_cols) {
    if (!col %in% names(data)) next
    if (col %in% skip_cols) next
    if (!is.numeric(data[[col]])) next

    data[[col]] <- vapply(
      data[[col]],
      format_numeric_value,
      character(1)
    )
  }

  data
}

#' Apply missing text substitution
#'
#' @param data Data frame
#' @param missing_text Text to substitute for NA values
#' @param apply_to "all", "numeric", or "character"
#' @return Data frame with missing text applied
#' @noRd
apply_missing_text_policy <- function(
  data,
  missing_text = "",
  apply_to = "all"
) {
  if (is.null(missing_text)) {
    return(data)
  }

  target_cols <- names(data)
  if (apply_to == "numeric") {
    target_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  } else if (apply_to == "character") {
    target_cols <- names(data)[vapply(data, is.character, logical(1))]
  }

  for (col in target_cols) {
    if (is.factor(data[[col]])) {
      data[[col]] <- as.character(data[[col]])
    }
    data[[col]][is.na(data[[col]])] <- missing_text
  }

  data
}

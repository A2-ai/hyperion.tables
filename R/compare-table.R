# ==============================================================================
# Comparison table assembly
# ==============================================================================

#' Detect which statistics are present in a comparison table
#'
#' @param comparison Data frame from compare_with()
#' @return Named list of logicals indicating which stats are present
#' @noRd
get_comparison_last_two <- function(comparison, suffix_cols) {
  meta <- normalize_comparison_meta(comparison, suffix_cols)
  labels <- meta$labels
  summaries <- meta$summaries
  if (length(labels) < 2) {
    labels <- c(labels, "Model")
  }
  if (length(summaries) < 2) {
    summaries <- c(summaries, list(NULL))
  }
  list(
    labels = utils::tail(labels, 2),
    summaries = utils::tail(summaries, 2)
  )
}

detect_comparison_statistics <- function(comparison) {
  fallback_suffix_cols <- comparison_suffix_columns()
  spec <- attr(comparison, "table_spec")
  suffix_cols <- get_comparison_suffix_cols(
    spec,
    comparison,
    fallback_suffix_cols
  )
  meta <- normalize_comparison_meta(comparison, suffix_cols)
  summaries <- meta$summaries
  labels <- meta$labels
  model_indices <- get_comparison_model_indices(names(comparison), suffix_cols)

  if (length(labels) < length(model_indices)) {
    labels <- c(
      labels,
      paste0("Model ", (length(labels) + 1):length(model_indices))
    )
  }

  if (length(summaries) < length(model_indices)) {
    summaries <- c(
      summaries,
      rep(list(NULL), length(model_indices) - length(summaries))
    )
  }

  # Get the last model and its reference
  ref_idx <- NULL
  if (length(model_indices) > 1) {
    last_idx <- utils::tail(model_indices, 1)
    right_pos <- length(model_indices)
    ref_ctx <- resolve_reference_context(
      comparison,
      last_idx,
      model_indices,
      labels,
      summaries,
      fallback_pos = right_pos - 1
    )
    sum1 <- ref_ctx$left_sum
    sum2 <- summaries[[right_pos]]
    ref_idx <- ref_ctx$left_idx
  } else {
    sum1 <- if (length(summaries) >= 1) summaries[[1]] else NULL
    sum2 <- if (length(summaries) >= 2) summaries[[2]] else NULL
    ref_idx <- 1
  }

  # Check if OFV is shown
  has_ofv <- length(summaries) > 0 &&
    any(vapply(
      summaries,
      function(sum) !is.null(sum$ofv) && !is.na(sum$ofv),
      logical(1)
    ))

  # Check if LRT is shown (both OFVs, same nobs, df > 0)
  has_lrt <- FALSE
  if (length(model_indices) > 1) {
    for (i in seq(2, length(model_indices))) {
      right_idx <- model_indices[i]
      right_sum <- summaries[[i]]
      ref_ctx <- resolve_reference_context(
        comparison,
        right_idx,
        model_indices,
        meta$labels,
        summaries,
        fallback_pos = i - 1
      )
      lrt_result <- can_show_lrt(
        comparison,
        ref_ctx$left_idx,
        right_idx,
        ref_ctx$left_sum,
        right_sum
      )
      if (lrt_result$show) {
        has_lrt <- TRUE
        break
      }
    }
  }

  # Check if pct_change is shown
  pct_cols <- grep("^pct_change(_\\d+)?$", names(comparison), value = TRUE)
  has_pct_change <- length(pct_cols) > 0 && any(!is.na(comparison[pct_cols]))

  list(
    has_ofv = has_ofv,
    has_lrt = has_lrt,
    has_pct_change = has_pct_change,
    ref_idx = ref_idx
  )
}

#' Return appropriate N/A string based on whether the summary is NULL
#' @noRd
na_label <- function(summary) {
  if (is.null(summary)) "N/A (no summary)" else "N/A"
}

#' Format condition number footnote line
#' @return Character string or NULL
#' @noRd
format_condition_number_footnote <- function(
  left_sum,
  right_sum,
  left_label,
  right_label,
  n_sigfig
) {
  cn1 <- safe_summary_field(left_sum, "condition_number")
  cn2 <- safe_summary_field(right_sum, "condition_number")

  if (is.na(cn1) && is.na(cn2)) {
    return(NULL)
  }

  cn1_str <- if (!is.na(cn1)) {
    hyperion::format_hyperion_sigfig_string(cn1, n_sigfig)
  } else {
    na_label(left_sum)
  }
  cn2_str <- if (!is.na(cn2)) {
    hyperion::format_hyperion_sigfig_string(cn2, n_sigfig)
  } else {
    na_label(right_sum)
  }

  sprintf(
    "Condition Number: %s (%s), %s (%s)",
    cn1_str,
    left_label,
    cn2_str,
    right_label
  )
}

#' Format observation count footnote line
#' @return Character string or NULL
#' @noRd
format_nobs_footnote <- function(left_sum, right_sum, left_label, right_label) {
  nobs1 <- safe_summary_field(left_sum, "number_obs")
  nobs2 <- safe_summary_field(right_sum, "number_obs")

  if (is.na(nobs1) && is.na(nobs2)) {
    return(NULL)
  }

  nobs1_str <- if (!is.na(nobs1)) as.character(nobs1) else na_label(left_sum)
  nobs2_str <- if (!is.na(nobs2)) as.character(nobs2) else na_label(right_sum)

  sprintf(
    "No. of Observations: %s (%s), %s (%s)",
    nobs1_str,
    left_label,
    nobs2_str,
    right_label
  )
}

#' Format OFV and LRT footnote line
#' @return Character string or NULL
#' @noRd
format_ofv_lrt_footnote <- function(
  comparison,
  left_idx,
  right_idx,
  left_sum,
  right_sum,
  left_label,
  right_label,
  nobs1,
  nobs2,
  n_sigfig,
  ofv_decimals,
  pvalue_scientific,
  pvalue_threshold
) {
  ofv1 <- safe_summary_field(left_sum, "ofv")
  ofv2 <- safe_summary_field(right_sum, "ofv")

  if (is.na(ofv1) && is.na(ofv2)) {
    return(NULL)
  }

  ofv1_str <- if (!is.na(ofv1)) {
    hyperion::format_hyperion_decimal_string(ofv1, ofv_decimals)
  } else {
    na_label(left_sum)
  }
  ofv2_str <- if (!is.na(ofv2)) {
    hyperion::format_hyperion_decimal_string(ofv2, ofv_decimals)
  } else {
    na_label(right_sum)
  }

  ofv_parts <- sprintf(
    "OFV: %s (%s), %s (%s)",
    ofv1_str,
    left_label,
    ofv2_str,
    right_label
  )

  if (!is.na(ofv1) && !is.na(ofv2)) {
    same_nobs <- !is.na(nobs1) && !is.na(nobs2) && nobs1 == nobs2
    if (same_nobs) {
      delta_ofv <- ofv2 - ofv1
      fixed1 <- comparison[[paste0("fixed_", left_idx)]]
      fixed2 <- comparison[[paste0("fixed_", right_idx)]]

      if (!is.null(fixed1) && !is.null(fixed2)) {
        k1 <- sum(!is.na(fixed1) & !fixed1, na.rm = TRUE)
        k2 <- sum(!is.na(fixed2) & !fixed2, na.rm = TRUE)
        df <- abs(k2 - k1)

        if (df > 0) {
          lrt_result <- can_show_lrt(
            comparison,
            left_idx,
            right_idx,
            left_sum,
            right_sum
          )
          if (lrt_result$show) {
            p_value <- lrt_pvalue(-delta_ofv, df)
            pval_str <- format_pvalue_string(
              p_value,
              n_sigfig,
              pvalue_scientific,
              pvalue_threshold
            )
            ofv_parts <- c(
              ofv_parts,
              sprintf(
                "delta = %s, LRT p-value = %s (df=%d)",
                hyperion::format_hyperion_decimal_string(
                  delta_ofv,
                  ofv_decimals
                ),
                pval_str,
                df
              )
            )
          } else {
            rlang::inform(c(
              sprintf(
                "LRT suppressed for %s vs %s: %s",
                left_label,
                right_label,
                lrt_result$reason
              ),
              i = "Both OFVs and matching observation counts are present, but LRT conditions are not met."
            ))
          }
        }
      }
    }
  }

  paste(ofv_parts, collapse = " | ")
}

#' Build comparison footnote with OFV and LRT statistics
#'
#' @param comparison Data frame from compare_with()
#' @param n_sigfig Number of significant figures for formatting
#' @param ofv_decimals Number of decimal places for OFV values
#' @param pvalue_scientific If TRUE, format p-values in scientific notation
#' @param pvalue_threshold If not NULL, p-values below this show as "< threshold"
#' @return Character vector of footnote lines, or NULL if no summaries
#' @noRd
build_comparison_footnote <- function(
  comparison,
  n_sigfig,
  ofv_decimals = NULL,
  pvalue_scientific = TRUE,
  pvalue_threshold = NULL
) {
  fallback_suffix_cols <- comparison_suffix_columns()
  spec <- attr(comparison, "table_spec")
  suffix_cols <- get_comparison_suffix_cols(
    spec,
    comparison,
    fallback_suffix_cols,
    include_fixed_for_ci = TRUE
  )
  meta <- normalize_comparison_meta(comparison, suffix_cols)
  labels <- meta$labels
  summaries <- meta$summaries

  model_indices <- get_comparison_model_indices(names(comparison), suffix_cols)
  if (length(model_indices) < 2) {
    return(NULL)
  }

  if (length(labels) < length(model_indices)) {
    labels <- c(
      labels,
      paste0("Model ", (length(labels) + 1):length(model_indices))
    )
  }

  if (length(summaries) < length(model_indices)) {
    summaries <- c(
      summaries,
      rep(list(NULL), length(model_indices) - length(summaries))
    )
  }

  lines <- character(0)

  for (i in 2:length(model_indices)) {
    right_idx <- model_indices[i]
    right_label <- labels[i]
    right_sum <- summaries[[i]]

    ref_ctx <- resolve_reference_context(
      comparison,
      right_idx,
      model_indices,
      labels,
      summaries,
      fallback_pos = i - 1
    )
    left_idx <- ref_ctx$left_idx
    left_label <- ref_ctx$left_label
    left_sum <- ref_ctx$left_sum

    cn_line <- format_condition_number_footnote(
      left_sum,
      right_sum,
      left_label,
      right_label,
      n_sigfig
    )
    if (!is.null(cn_line)) {
      lines <- c(lines, cn_line)
    }

    nobs_line <- format_nobs_footnote(
      left_sum,
      right_sum,
      left_label,
      right_label
    )
    if (!is.null(nobs_line)) {
      lines <- c(lines, nobs_line)
    }

    nobs1 <- safe_summary_field(left_sum, "number_obs")
    nobs2 <- safe_summary_field(right_sum, "number_obs")

    ofv_line <- format_ofv_lrt_footnote(
      comparison,
      left_idx,
      right_idx,
      left_sum,
      right_sum,
      left_label,
      right_label,
      nobs1,
      nobs2,
      n_sigfig,
      ofv_decimals,
      pvalue_scientific,
      pvalue_threshold
    )
    if (!is.null(ofv_line)) lines <- c(lines, ofv_line)
  }

  if (length(lines) > 0) lines else NULL
}

#' Validate that a comparison can produce renderable output
#' @noRd
validate_comparison_renderable <- function(comparison, model_cols) {
  if (nrow(comparison) == 0) {
    rlang::abort("Comparison has no rows after preparation.")
  }

  all_model_col_names <- unlist(model_cols, use.names = FALSE)
  if (length(all_model_col_names) == 0) {
    rlang::abort(c(
      "No model-specific columns remain after applying spec.",
      i = "Check your `drop_columns`, `columns`, or `hide_empty_columns` settings."
    ))
  }
}

#' Build comparison table
#'
#' Creates a formatted table comparing parameters from two or more models.
#' Supports multiple output formats: gt (default), flextable, or the
#' intermediate HyperionTable object.
#'
#' @param comparison Comparison data frame from `compare_with()`
#' @param output Output format: "gt" (default), "flextable", or "data" for
#'   the intermediate HyperionTable object.
#'
#' @importFrom rlang .data
#'
#' @return A gt table, flextable, or HyperionTable object depending on `output`
#' @export
make_comparison_table <- function(
  comparison,
  output = c("gt", "flextable", "data")
) {
  output <- match.arg(output)

  if (output == "flextable") {
    check_suggested("flextable", reason = "for flextable output.")
  }

  if (!inherits(comparison, "hyperion_comparison")) {
    rlang::abort(
      "Input must be a hyperion_comparison object from compare_with()"
    )
  }

  # Preserve attributes before dplyr operations (which strip custom attrs)
  spec <- attr(comparison, "table_spec")
  if (is.null(spec)) {
    rlang::abort(
      "TableSpec not found. Run apply_table_spec(params, spec, info) first."
    )
  }
  fallback_suffix_cols <- comparison_suffix_columns()

  # Prepare data + layout (sections, fixed display, hide rules, labels).
  prep <- prepare_comparison_table_data(
    comparison,
    spec,
    fallback_suffix_cols
  )
  comparison <- prep$comparison
  layout <- prep$layout
  labels <- prep$labels
  suffix_cols <- prep$suffix_cols
  model_indices <- prep$model_indices

  display_cols <- layout$display_cols
  hide_cols <- layout$hide_cols
  show_pct_change <- layout$show_pct_change
  pct_change_cols <- layout$pct_change_cols
  fixed_display_cols <- layout$fixed_display_cols

  ci_pct <- get_ci_pct(spec, default = 95)

  # Compute model spanner columns and reorder the data.
  saved_attrs <- capture_comparison_attrs(comparison)
  model_layout <- compute_comparison_model_cols(
    comparison,
    display_cols,
    model_indices,
    hide_cols,
    spec,
    show_pct_change
  )
  comparison <- model_layout$comparison
  model_cols <- model_layout$model_cols
  comparison <- restore_comparison_attrs(comparison, saved_attrs)

  validate_comparison_renderable(comparison, model_cols)

  # Build label map
  label_map <- build_comparison_label_map(
    labels,
    pct_change_cols,
    show_pct_change,
    ci_pct,
    spec,
    fixed_display_cols,
    model_indices,
    comparison,
    hide_cols
  )

  # Create intermediate representation
  htable <- hyperion_comparison_table(
    comparison,
    layout,
    model_cols,
    labels,
    spec,
    label_map,
    model_indices
  )

  # Return based on output format
  table <- switch(
    output,
    data = htable,
    flextable = render_to_flextable(htable),
    # default
    render_to_gt(htable)
  )
  table
}

#' Render comparison table as gt (internal)
#'
#' Preserves the original gt rendering logic for backwards compatibility.
#'
#' @noRd
render_gt_comparison_table <- function(
  comparison,
  layout,
  model_cols,
  labels,
  spec,
  label_map,
  model_indices,
  n_sigfig,
  ci_pct,
  pct_change_cols
) {
  htable <- hyperion_comparison_table(
    comparison,
    layout,
    model_cols,
    labels,
    spec,
    label_map,
    model_indices
  )
  render_to_gt(htable)
}

# ==============================================================================
# HyperionTable Constructor for Comparison Tables
# ==============================================================================

#' Create HyperionTable from comparison table layout
#'
#' @param comparison Data frame from prepare_comparison_table_data()
#' @param layout List with display_cols, hide_cols, etc.
#' @param model_cols List of columns per model
#' @param labels Character vector of model labels
#' @param spec TableSpec object
#' @param label_map Named list of column labels
#' @param model_indices Integer vector of model indices
#' @return HyperionTable object
#' @noRd
hyperion_comparison_table <- function(
  comparison,
  layout,
  model_cols,
  labels,
  spec,
  label_map,
  model_indices
) {
  # Build spanners from model_cols
  spanners <- list()
  for (i in seq_along(model_cols)) {
    cols <- model_cols[[i]]
    if (length(cols) > 0) {
      label <- if (length(labels) >= i) labels[i] else paste0("Model ", i)
      spanners <- c(spanners, list(spanner_spec(label, cols)))
    }
  }

  # CI merges per model
  ci_merges <- list()
  if (!is.null(spec) && all(c("ci_low", "ci_high") %in% spec@columns)) {
    for (idx in model_indices) {
      ci_low <- paste0("ci_low_", idx)
      ci_high <- paste0("ci_high_", idx)
      if (all(c(ci_low, ci_high) %in% names(comparison))) {
        ci_merges <- c(ci_merges, list(ci_merge_spec(ci_low, ci_high)))
      }
    }
  }

  # Build border specs
  borders <- list()
  border_cols <- character(0)
  for (cols in model_cols) {
    if (length(cols) > 0) {
      border_cols <- c(border_cols, utils::tail(cols, 1))
    }
  }
  if (length(border_cols) > 0) {
    borders <- list(border_spec(
      sides = "right",
      color = "#D3D3D3",
      columns = border_cols,
      part = "body"
    ))
  }

  # Build footnotes
  footnotes <- build_comparison_footnotes(comparison, spec, layout)

  # Numeric columns
  pct_change_cols <- grep("^pct_change", names(comparison), value = TRUE)
  numeric_cols <- c(
    paste0("estimate_", model_indices),
    paste0("rse_", model_indices),
    paste0("ci_low_", model_indices),
    paste0("ci_high_", model_indices),
    pct_change_cols
  )
  numeric_cols <- intersect(numeric_cols, names(comparison))

  # Bold locations
  bold_locs <- c("column_labels", "spanners")
  if (!is.null(layout$groupname)) {
    bold_locs <- c(bold_locs, "row_groups")
  }

  HyperionTable(
    data = comparison,
    table_type = "comparison",
    groupname_col = layout$groupname,
    hide_cols = layout$hide_cols,
    col_labels = label_map,
    title = spec@title,
    spanners = spanners,
    numeric_cols = numeric_cols,
    n_sigfig = spec@n_sigfig,
    ci = spec@ci,
    ci_merges = ci_merges,
    ci_missing_rows = integer(0),
    bold_locations = bold_locs,
    borders = borders,
    footnotes = footnotes,
    missing_text = spec@missing_text,
    missing_apply_to = spec@missing_apply_to,
    source_spec = spec
  )
}

#' Build footnotes for comparison table
#' @noRd
build_comparison_footnotes <- function(comparison, spec, layout) {
  footnotes <- list()

  footnote_order <- spec@footnote_order
  if (is.null(footnote_order)) {
    return(list())
  }

  n_sigfig <- spec@n_sigfig
  ci_pct <- round(spec@ci@level * 100)

  # Build summary note
  ofv_decimals <- if (!is.na(spec@n_decimals_ofv)) spec@n_decimals_ofv else NULL
  pvalue_scientific <- spec@pvalue_scientific
  pvalue_threshold <- spec@pvalue_threshold
  summary_note <- build_comparison_footnote(
    comparison,
    n_sigfig,
    ofv_decimals,
    pvalue_scientific,
    pvalue_threshold
  )

  # Detect statistics
  stats <- detect_table_statistics(comparison)
  comparison_stats <- detect_comparison_statistics(comparison)

  # Check if CI dropped
  expanded_drop <- expand_ci_drop_columns(spec@drop_columns)
  if (all(c("ci_low", "ci_high") %in% expanded_drop)) {
    stats$has_ci <- FALSE
  }

  # Build equations
  equations <- build_equations_footnote(stats, ci_pct, comparison_stats, NULL)

  # Build abbreviations
  abbreviations <- build_abbreviations_footnote(stats, comparison_stats, NULL)

  # Assemble footnotes in order
  for (section in footnote_order) {
    if (section == "summary_info" && !is.null(summary_note)) {
      for (line in summary_note) {
        footnotes <- c(footnotes, list(footnote_spec(line, FALSE)))
      }
    } else if (section == "equations" && !is.null(equations)) {
      for (eq in equations) {
        content <- if (inherits(eq, "gt_md")) as.character(eq) else eq
        footnotes <- c(footnotes, list(footnote_spec(content, TRUE)))
      }
    } else if (section == "abbreviations" && !is.null(abbreviations)) {
      for (line in abbreviations) {
        footnotes <- c(footnotes, list(footnote_spec(line, FALSE)))
      }
    }
  }

  footnotes
}

# ==============================================================================
# Comparison table helpers
# ==============================================================================

#' Compute comparison table layout details
#' @noRd
compute_comparison_layout <- function(
  comparison,
  spec,
  suffix_cols,
  model_indices,
  fallback_suffix_cols
) {
  display_cols <- get_comparison_suffix_cols(
    spec,
    comparison,
    fallback_suffix_cols
  )
  display_cols <- setdiff(display_cols, "pct_change")
  fixed_display_cols <- "fixed" %in%
    display_cols &&
    any(grepl("^fixed_fmt_\\d+$", names(comparison)))
  if (fixed_display_cols) {
    display_cols <- sub("^fixed$", "fixed_fmt", display_cols)
  }

  hide_cols <- c("kind", "random_effect", "diagonal", ".appear_order")
  hide_suffix <- grep(
    "^(fixed|fixed_fmt|stderr|variability|shrinkage|cv|corr|sd)_\\d+$",
    names(comparison),
    value = TRUE
  )
  if ("fixed" %in% display_cols || "fixed_fmt" %in% display_cols) {
    hide_suffix <- hide_suffix[!grepl("^fixed_fmt_\\d+$", hide_suffix)]
  }
  if ("stderr" %in% display_cols) {
    hide_suffix <- hide_suffix[!grepl("^stderr_\\d+$", hide_suffix)]
  }
  if ("variability" %in% display_cols) {
    hide_suffix <- hide_suffix[!grepl("^variability_\\d+$", hide_suffix)]
  }
  if ("cv" %in% display_cols) {
    hide_suffix <- hide_suffix[!grepl("^cv_\\d+$", hide_suffix)]
  }
  if ("corr" %in% display_cols) {
    hide_suffix <- hide_suffix[!grepl("^corr_\\d+$", hide_suffix)]
  }
  if ("sd" %in% display_cols) {
    hide_suffix <- hide_suffix[!grepl("^sd_\\d+$", hide_suffix)]
  }
  if ("shrinkage" %in% display_cols) {
    hide_suffix <- hide_suffix[!grepl("^shrinkage_\\d+$", hide_suffix)]
  }
  hide_cols <- intersect(c(hide_cols, hide_suffix), names(comparison))

  add_cols <- if (!is.null(spec)) {
    spec@add_columns %||% character(0)
  } else {
    character(0)
  }
  columns <- if (!is.null(spec)) spec@columns else character(0)
  if (!"fixed" %in% c(columns, add_cols)) {
    hide_cols <- unique(c(
      hide_cols,
      grep("^fixed(_\\d+)?$", names(comparison), value = TRUE),
      grep("^fixed_fmt(_\\d+)?$", names(comparison), value = TRUE)
    ))
  } else if (fixed_display_cols) {
    hide_cols <- unique(c(
      hide_cols,
      grep("^fixed_\\d+$", names(comparison), value = TRUE)
    ))
  }

  if (!is.null(spec) && spec@hide_empty_columns) {
    fixed_requested <- if (isTRUE(spec@.columns_provided)) {
      "fixed" %in% c(spec@columns, add_cols)
    } else {
      "fixed" %in% add_cols
    }
    if (!fixed_requested) {
      fixed_cols <- grep("^fixed_\\d+$", names(comparison), value = TRUE)
      for (fc in fixed_cols) {
        if (!any(comparison[[fc]], na.rm = TRUE)) {
          hide_cols <- unique(c(
            hide_cols,
            fc,
            sub("^fixed_", "fixed_fmt_", fc)
          ))
        }
      }
    }

    empty_cols <- find_empty_columns(comparison)
    if (length(display_cols) > 0 && isTRUE(spec@.columns_provided)) {
      requested_suffixes <- unlist(
        lapply(
          display_cols,
          function(col) {
            grep(paste0("^", col, "_\\d+$"), names(comparison), value = TRUE)
          }
        ),
        use.names = FALSE
      )
      empty_cols <- setdiff(empty_cols, requested_suffixes)
    }
    hide_cols <- unique(c(hide_cols, empty_cols))
  }

  pct_change_cols <- grep("^pct_change_\\d+$", names(comparison), value = TRUE)
  columns_provided <- !is.null(spec) && isTRUE(spec@.columns_provided)
  show_pct_change <- !is.null(spec) &&
    ((!columns_provided) ||
      "pct_change" %in% columns ||
      "pct_change" %in% add_cols)
  if (length(pct_change_cols) > 0 && "pct_change" %in% names(comparison)) {
    hide_cols <- unique(c(hide_cols, "pct_change"))
  }
  if (!show_pct_change) {
    hide_cols <- unique(c(hide_cols, pct_change_cols))
    if ("pct_change" %in% names(comparison)) {
      hide_cols <- unique(c(hide_cols, "pct_change"))
    }
  }

  allowed_cols <- display_cols
  if ("ci_low" %in% allowed_cols && !"ci_high" %in% allowed_cols) {
    allowed_cols <- c(allowed_cols, "ci_high")
  }
  allowed_suffixed <- c(
    unlist(
      lapply(allowed_cols, function(col) paste0(col, "_", model_indices)),
      use.names = FALSE
    ),
    if (show_pct_change) pct_change_cols else character(0)
  )
  suffixed_cols <- grep("_(\\d+)$", names(comparison), value = TRUE)
  hide_cols <- unique(c(hide_cols, setdiff(suffixed_cols, allowed_suffixed)))
  if (!is.null(spec) && all(c("ci_low", "ci_high") %in% spec@columns)) {
    hide_cols <- unique(c(
      hide_cols,
      grep("^ci_high_\\d+$", names(comparison), value = TRUE)
    ))
  }

  if (!is.null(spec) && length(spec@drop_columns) > 0) {
    drop_cols <- sub("_left$", "_1", spec@drop_columns)
    drop_cols <- sub("_right$", "_2", drop_cols)
    if (fixed_display_cols && "fixed" %in% drop_cols) {
      drop_cols <- unique(c(drop_cols, "fixed_fmt"))
    }
    drop_suffix <- intersect(drop_cols, suffix_cols)

    drop_expanded <- unlist(
      lapply(
        drop_suffix,
        function(col) paste0(col, "_", model_indices)
      ),
      use.names = FALSE
    )

    if (
      "ci" %in% drop_cols || "ci_low" %in% drop_cols || "ci_high" %in% drop_cols
    ) {
      drop_expanded <- c(
        drop_expanded,
        paste0("ci_low_", model_indices),
        paste0("ci_high_", model_indices)
      )
    }
    if (any(drop_cols %in% c("ci_left", "ci_1"))) {
      drop_expanded <- c(drop_expanded, "ci_low_1", "ci_high_1")
    }
    if (any(drop_cols %in% c("ci_right", "ci_2"))) {
      drop_expanded <- c(drop_expanded, "ci_low_2", "ci_high_2")
    }
    drop_num <- grep("^ci_\\d+$", drop_cols, value = TRUE)
    if (length(drop_num) > 0) {
      nums <- as.integer(sub("^ci_", "", drop_num))
      drop_expanded <- c(
        drop_expanded,
        paste0("ci_low_", nums),
        paste0("ci_high_", nums)
      )
    }
    if ("pct_change" %in% drop_cols) {
      drop_expanded <- c(
        drop_expanded,
        grep("^pct_change(_\\d+)?$", names(comparison), value = TRUE)
      )
    }
    pct_num <- grep("^pct_change_\\d+$", drop_cols, value = TRUE)
    if (length(pct_num) > 0) {
      drop_expanded <- c(drop_expanded, pct_num)
    }
    drop_expanded <- c(drop_expanded, intersect(drop_cols, names(comparison)))
    hide_cols <- unique(c(hide_cols, drop_expanded))
  }

  groupname <- if (
    "section" %in% names(comparison) && !all(is.na(comparison$section))
  ) {
    "section"
  } else {
    NULL
  }

  list(
    display_cols = display_cols,
    hide_cols = hide_cols,
    show_pct_change = show_pct_change,
    pct_change_cols = pct_change_cols,
    fixed_display_cols = fixed_display_cols,
    groupname = groupname
  )
}

#' Compute comparison model columns and reorder data
#' @noRd
compute_comparison_model_cols <- function(
  comparison,
  display_cols,
  model_indices,
  hide_cols,
  spec,
  show_pct_change
) {
  if (all(c("ci_low", "ci_high") %in% display_cols)) {
    display_cols <- display_cols[display_cols != "ci_high"]
  }

  model_cols <- list()
  for (idx in model_indices) {
    cols <- paste0(display_cols, "_", idx)
    cols <- intersect(cols, names(comparison))
    cols <- cols[!cols %in% hide_cols]
    if (!is.null(spec) && all(c("ci_low", "ci_high") %in% spec@columns)) {
      cols <- cols[cols != paste0("ci_high_", idx)]
    }

    pct_col <- paste0("pct_change_", idx)
    if (
      show_pct_change &&
        pct_col %in% names(comparison) &&
        !pct_col %in% hide_cols
    ) {
      cols <- c(cols, pct_col)
    }

    model_cols[[as.character(idx)]] <- cols
  }

  desired_cols <- c("name", unlist(model_cols, use.names = FALSE))
  remaining_cols <- setdiff(names(comparison), desired_cols)
  comparison <- dplyr::select(
    comparison,
    dplyr::all_of(c(desired_cols, remaining_cols))
  )

  list(comparison = comparison, model_cols = model_cols)
}

#' Build comparison label map
#' @noRd
build_comparison_label_map <- function(
  labels,
  pct_change_cols,
  show_pct_change,
  ci_pct,
  spec,
  fixed_display_cols,
  model_indices,
  comparison,
  hide_cols
) {
  label_map <- list(name = "Parameter", pct_change = "% Change")
  pct_change_refs <- attr(comparison, "pct_change_refs")
  if (length(pct_change_cols) > 0 && show_pct_change) {
    label_map$pct_change <- NULL
    for (col in pct_change_cols) {
      idx <- as.integer(sub("^pct_change_", "", col))
      # Use reference index if available, otherwise default to idx - 1
      ref_idx <- if (!is.null(pct_change_refs[[col]])) {
        pct_change_refs[[col]]
      } else {
        idx - 1
      }
      ref_label <- if (length(labels) >= ref_idx) {
        labels[ref_idx]
      } else {
        paste0("Model ", ref_idx)
      }
      if (length(pct_change_cols) == 1) {
        label_map[[col]] <- "% Change"
      } else {
        label_map[[col]] <- sprintf("%% Change vs %s", ref_label)
      }
    }
  }

  base_labels <- build_parameter_label_map(ci_pct)
  base_labels <- base_labels[names(base_labels) != "name"]
  base_labels <- adjust_ci_labels(base_labels, spec, ci_pct)
  if (fixed_display_cols) {
    base_labels$fixed_fmt <- base_labels$fixed
    base_labels$fixed <- NULL
  }
  for (idx in model_indices) {
    for (col in names(base_labels)) {
      label_map[[paste0(col, "_", idx)]] <- base_labels[[col]]
    }
  }

  label_map[setdiff(
    intersect(names(label_map), names(comparison)),
    hide_cols
  )]
}

#' Apply model spanners to comparison table
#' @noRd
apply_model_spanners <- function(table, model_cols, labels) {
  for (i in seq_along(model_cols)) {
    cols <- model_cols[[i]]
    if (length(cols) > 0) {
      label <- if (length(labels) >= i) labels[i] else paste0("Model ", i)
      table <- table |>
        gt::tab_spanner(label = label, columns = dplyr::all_of(cols))
    }
  }
  table
}

#' Apply comparison table footnotes
#' @noRd
apply_comparison_footnotes <- function(
  table,
  comparison,
  spec,
  n_sigfig,
  ci_pct
) {
  ofv_decimals <- if (!is.null(spec) && !is.na(spec@n_decimals_ofv)) {
    spec@n_decimals_ofv
  } else {
    NULL
  }
  pvalue_scientific <- if (!is.null(spec)) spec@pvalue_scientific else TRUE
  pvalue_threshold <- if (!is.null(spec)) spec@pvalue_threshold else NULL
  summary_note <- build_comparison_footnote(
    comparison,
    n_sigfig,
    ofv_decimals,
    pvalue_scientific,
    pvalue_threshold
  )

  comparison_stats <- detect_comparison_statistics(comparison)
  add_conditional_footnotes(
    table,
    comparison,
    spec,
    comparison_stats = comparison_stats,
    summary_note = summary_note
  )
}

#' Prepare comparison table data and layout
#' @noRd
prepare_comparison_table_data <- function(
  comparison,
  spec,
  fallback_suffix_cols
) {
  # Capture all comparison attrs early (dplyr operations strip custom attrs)
  saved_attrs <- capture_comparison_attrs(comparison)

  plan <- variability_plan(spec)
  if (plan$build_variability) {
    suffix_for_variability <- unique(c(
      fallback_suffix_cols,
      "cv",
      "corr",
      "sd",
      "fixed"
    ))
    comparison <- build_variability_comparison(
      comparison,
      spec,
      suffix_for_variability
    )
  }

  suffix_cols <- get_comparison_suffix_cols(
    spec,
    comparison,
    fallback_suffix_cols
  )
  meta <- normalize_comparison_meta(comparison, suffix_cols)
  labels <- meta$labels
  summaries <- meta$summaries
  model_indices <- get_comparison_model_indices(names(comparison), suffix_cols)

  if ("section" %in% names(comparison) && !all(is.na(comparison$section))) {
    if (!is.null(spec) && length(spec@sections) > 0) {
      section_levels <- unique(get_section_order(spec))
      comparison <- comparison |>
        dplyr::mutate(
          .appear_order = dplyr::row_number(),
          section = factor(.data$section, levels = section_levels)
        ) |>
        dplyr::arrange(.data$section, .data$.appear_order)
    }
  }

  comparison <- blank_ci_for_fixed(comparison)
  fixed_cols <- grep("^fixed_\\d+$", names(comparison), value = TRUE)
  comparison <- add_fixed_display_columns(comparison, fixed_cols)

  # Restore saved attrs and set summaries/labels from meta
  comparison <- restore_comparison_attrs(comparison, saved_attrs)
  attr(comparison, "summaries") <- summaries
  attr(comparison, "labels") <- labels

  layout <- compute_comparison_layout(
    comparison,
    spec,
    suffix_cols,
    model_indices,
    fallback_suffix_cols
  )

  list(
    comparison = comparison,
    layout = layout,
    labels = labels,
    summaries = summaries,
    suffix_cols = suffix_cols,
    model_indices = model_indices
  )
}

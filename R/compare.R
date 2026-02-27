# ==============================================================================
# Model comparison functions
# ==============================================================================

#' Capture all custom attributes from a comparison object
#'
#' Used to preserve attributes before dplyr operations which strip them.
#' @param comparison A hyperion_comparison object
#' @return Named list of attributes
#' @noRd
capture_comparison_attrs <- function(comparison) {
  list(
    summaries = attr(comparison, "summaries"),
    labels = attr(comparison, "labels"),
    table_spec = attr(comparison, "table_spec"),
    pct_change_refs = attr(comparison, "pct_change_refs"),
    lineage = attr(comparison, "lineage")
  )
}

#' Restore custom attributes to a comparison object
#'
#' Used to restore attributes after dplyr operations which strip them.
#' @param comparison A data frame to restore attributes to
#' @param attrs Named list of attributes from capture_comparison_attrs()
#' @return The comparison with attributes restored
#' @noRd
restore_comparison_attrs <- function(comparison, attrs) {
  for (name in names(attrs)) {
    if (!is.null(attrs[[name]])) {
      attr(comparison, name) <- attrs[[name]]
    }
  }
  comparison
}

#' @noRd
get_comparison_model_indices <- function(names_vec, suffix_cols) {
  pattern <- paste0("^(", paste(suffix_cols, collapse = "|"), ")_(\\d+)$")
  matched <- grep(pattern, names_vec, value = TRUE)
  if (length(matched) == 0) {
    return(integer(0))
  }
  indices <- as.integer(sub(pattern, "\\2", matched))
  indices <- sort(unique(indices[!is.na(indices)]))
  indices
}

#' @noRd
normalize_comparison_meta <- function(comparison, suffix_cols) {
  labels <- attr(comparison, "labels")
  summaries <- attr(comparison, "summaries")

  if (is.null(labels)) {
    indices <- get_comparison_model_indices(names(comparison), suffix_cols)
    labels <- paste0("Model ", indices)
  }

  list(labels = labels, summaries = summaries)
}

#' @noRd
resolve_reference_context <- function(
  comparison,
  right_idx,
  model_indices,
  labels,
  summaries,
  fallback_pos
) {
  pct_change_refs <- attr(comparison, "pct_change_refs")
  pct_col <- paste0("pct_change_", right_idx)
  if (!is.null(pct_change_refs[[pct_col]])) {
    ref_idx <- pct_change_refs[[pct_col]]
    ref_pos <- which(model_indices == ref_idx)
    if (length(ref_pos) > 0) {
      return(list(
        left_idx = ref_idx,
        left_label = labels[ref_pos],
        left_sum = summaries[[ref_pos]]
      ))
    }
  }

  list(
    left_idx = model_indices[fallback_pos],
    left_label = labels[fallback_pos],
    left_sum = summaries[[fallback_pos]]
  )
}

#' Check whether LRT can be shown for a model pair
#'
#' @return list(show = logical, reason = character or NULL). When `show` is
#'   FALSE, `reason` explains why.
#' @noRd
can_show_lrt <- function(comparison, left_idx, right_idx, left_sum, right_sum) {
  suppress <- function(reason) list(show = FALSE, reason = reason)

  lineage <- attr(comparison, "lineage")
  if (is.null(lineage)) {
    return(suppress("no lineage attached"))
  }
  if (is.null(left_sum) || is.null(right_sum)) {
    return(suppress("one or both model summaries are missing"))
  }

  ofv1 <- safe_summary_field(left_sum, "ofv")
  ofv2 <- safe_summary_field(right_sum, "ofv")
  if (is.na(ofv1) || is.na(ofv2)) {
    return(suppress("one or both OFV values are missing"))
  }

  nobs1 <- safe_summary_field(left_sum, "number_obs")
  nobs2 <- safe_summary_field(right_sum, "number_obs")
  if (is.na(nobs1) || is.na(nobs2)) {
    return(suppress("one or both observation counts are missing"))
  }
  if (nobs1 != nobs2) {
    return(suppress("observation counts differ"))
  }

  fixed1 <- comparison[[paste0("fixed_", left_idx)]]
  fixed2 <- comparison[[paste0("fixed_", right_idx)]]
  if (is.null(fixed1) || is.null(fixed2)) {
    return(suppress("fixed column(s) missing from comparison"))
  }
  k1 <- sum(!is.na(fixed1) & !fixed1, na.rm = TRUE)
  k2 <- sum(!is.na(fixed2) & !fixed2, na.rm = TRUE)
  df <- abs(k2 - k1)
  if (df <= 0) {
    return(suppress("degrees of freedom is zero"))
  }

  run_name1 <- safe_summary_field(left_sum, "run_name")
  run_name2 <- safe_summary_field(right_sum, "run_name")
  if (is.na(run_name1) || is.na(run_name2)) {
    return(suppress("one or both run names are missing"))
  }

  if (!are_models_in_lineage(lineage, run_name1, run_name2)) {
    return(suppress("models not in direct lineage"))
  }

  list(show = TRUE, reason = NULL)
}

#' @noRd
get_comparison_suffix_cols <- function(
  spec,
  params,
  fallback_cols,
  include_fixed_for_ci = FALSE
) {
  if (!is.null(spec) && !is.null(spec@columns)) {
    cols <- setdiff(spec@columns, "name")
  } else {
    cols <- fallback_cols
  }

  if (include_fixed_for_ci && any(cols %in% c("ci_low", "ci_high"))) {
    cols <- unique(c(cols, "fixed"))
  }

  cols <- cols[cols != "pct_change"]

  if (inherits(params, "hyperion_comparison")) {
    cols <- cols[vapply(
      cols,
      function(col) any(grepl(paste0("^", col, "_\\d+$"), names(params))),
      logical(1)
    )]
  } else {
    cols <- intersect(cols, names(params))
  }

  cols
}

#' Validate and merge labels for a comparison
#'
#' @param is_comparison Logical, whether params1 is already a comparison
#' @param labels Character vector of new labels
#' @param existing_labels Character vector of existing labels (or NULL)
#' @return Merged label vector
#' @noRd
resolve_comparison_labels <- function(is_comparison, labels, existing_labels) {
  if (is_comparison) {
    if (length(labels) == 1) {
      labels <- c(existing_labels, labels)
    } else if (length(labels) == 2) {
      if (length(existing_labels) > 0) {
        existing_labels[length(existing_labels)] <- labels[1]
      }
      labels <- c(existing_labels, labels[2])
    } else {
      rlang::abort(
        "labels must be length 1 or 2 when comparing with an existing comparison"
      )
    }
  } else if (length(labels) != 2) {
    rlang::abort("labels must be a character vector of length 2")
  }
  labels
}

#' Resolve the reference model index for percent change calculation
#'
#' Searches by run_name in summaries first, then by label. Aborts if not found.
#'
#' @param reference_model Character string to match
#' @param existing_summaries List of model summaries
#' @param existing_labels Character vector of model labels
#' @param model_indices Integer vector of model indices
#' @param default_idx Default reference index if reference_model is NULL
#' @return Integer reference index
#' @noRd
resolve_reference_index <- function(
  reference_model,
  existing_summaries,
  existing_labels,
  model_indices,
  default_idx
) {
  if (is.null(reference_model)) {
    return(default_idx)
  }

  ref_model_clean <- tools::file_path_sans_ext(reference_model)

  # First try matching by run_name in summaries
  for (i in seq_along(existing_summaries)) {
    sum_i <- existing_summaries[[i]]
    if (!is.null(sum_i) && !is.null(sum_i$run_name)) {
      run_name_clean <- tools::file_path_sans_ext(sum_i$run_name)
      if (run_name_clean == ref_model_clean) {
        return(model_indices[i])
      }
    }
  }

  # Fall back to matching by label
  for (i in seq_along(existing_labels)) {
    label_clean <- tools::file_path_sans_ext(existing_labels[i])
    if (label_clean == ref_model_clean) {
      return(model_indices[i])
    }
  }

  # Not found — abort
  available <- character(0)
  for (i in seq_along(existing_summaries)) {
    sum_i <- existing_summaries[[i]]
    if (!is.null(sum_i) && !is.null(sum_i$run_name)) {
      available <- c(available, tools::file_path_sans_ext(sum_i$run_name))
    }
  }
  label_names <- tools::file_path_sans_ext(existing_labels)
  available <- unique(c(available, label_names))
  rlang::abort(c(
    sprintf(
      "reference_model '%s' not found in existing comparison models.",
      reference_model
    ),
    i = sprintf("Available models: %s", paste(available, collapse = ", "))
  ))
}

#' Compute percent change column on a comparison data frame
#'
#' @param comparison Data frame
#' @param ref_idx Reference model index
#' @param last_idx New model index
#' @return Modified comparison with pct_change columns added
#' @noRd
compute_pct_change <- function(comparison, ref_idx, last_idx) {
  est_ref <- paste0("estimate_", ref_idx)
  est_last <- paste0("estimate_", last_idx)
  pct_col <- paste0("pct_change_", last_idx)
  if (est_ref %in% names(comparison) && est_last %in% names(comparison)) {
    comparison[[pct_col]] <- dplyr::case_when(
      is.na(comparison[[est_ref]]) | is.na(comparison[[est_last]]) ~ NA_real_,
      comparison[[est_ref]] == 0 ~ NA_real_,
      TRUE ~
        (comparison[[est_last]] - comparison[[est_ref]]) /
        comparison[[est_ref]] *
        100
    )
    comparison$pct_change <- comparison[[pct_col]]
  }
  comparison
}

#' Determine which columns need model-index suffixes for a comparison
#'
#' Resolves the set of suffix columns based on the table spec, variability
#' transforms, and add_columns settings from params1.
#'
#' @param params1 Enriched parameter data frame (or existing comparison)
#' @return Character vector of suffix column names
#' @noRd
resolve_suffix_cols_for_comparison <- function(params1) {
  fallback_suffix_cols <- comparison_suffix_columns()
  spec1 <- attr(params1, "table_spec")

  suffix_cols <- get_comparison_suffix_cols(
    spec1,
    params1,
    fallback_suffix_cols,
    include_fixed_for_ci = TRUE
  )

  plan <- variability_plan(spec1)
  if (plan$build_variability) {
    suffix_cols <- unique(c(suffix_cols, "cv", "corr", "sd", "fixed"))
  }

  add_cols1 <- if (!is.null(spec1)) {
    spec1@add_columns %||% character(0)
  } else {
    character(0)
  }
  columns_provided <- !is.null(spec1) && isTRUE(spec1@.columns_provided)
  if (is.null(spec1) || !columns_provided) {
    suffix_cols <- unique(c(suffix_cols, "pct_change"))
  } else if ("pct_change" %in% add_cols1) {
    suffix_cols <- unique(c(suffix_cols, "pct_change"))
  }

  suffix_cols
}

#' Compute model positions and metadata for a comparison join
#'
#' Determines indices, existing labels/summaries, and summary objects needed
#' to join the next model into a comparison.
#'
#' @param params1 Enriched parameter data frame (or existing comparison)
#' @param params2 Enriched parameter data frame for the new model
#' @param suffix_cols Character vector from resolve_suffix_cols_for_comparison()
#' @return Named list with: is_comparison, existing_labels, existing_summaries,
#'   model_indices, model_count, next_index, prev_idx, last_idx, sum1, sum2,
#'   spec, coalesce_cols
#' @noRd
compute_model_positions <- function(params1, params2, suffix_cols) {
  coalesce_cols <- c("kind", "section", "random_effect", "diagonal")

  spec1 <- attr(params1, "table_spec")
  spec2 <- attr(params2, "table_spec")
  sum2 <- attr(params2, "model_summary")

  is_comparison <- inherits(params1, "hyperion_comparison")

  if (is_comparison) {
    meta <- normalize_comparison_meta(params1, suffix_cols)
    existing_labels <- meta$labels
    existing_summaries <- meta$summaries
    model_indices <- get_comparison_model_indices(names(params1), suffix_cols)
    max_index <- if (length(model_indices) > 0) max(model_indices) else 0
    model_count <- if (max_index > 0) {
      max_index
    } else {
      max(
        length(existing_labels),
        length(existing_summaries)
      )
    }
    if (length(existing_summaries) < model_count) {
      existing_summaries <- c(
        existing_summaries,
        rep(list(NULL), model_count - length(existing_summaries))
      )
    }
    if (length(existing_labels) < model_count) {
      existing_labels <- c(
        existing_labels,
        paste0("Model ", (length(existing_labels) + 1):model_count)
      )
    }
  } else {
    existing_labels <- NULL
    existing_summaries <- NULL
    model_count <- 1
    model_indices <- integer(0)
  }

  next_index <- if (is_comparison) model_count + 1 else 2

  sum1 <- if (is_comparison) {
    utils::tail(existing_summaries, 1)[[1]]
  } else {
    attr(params1, "model_summary")
  }

  spec <- if (!is.null(spec1)) spec1 else spec2

  if (is_comparison) {
    last_idx <- next_index
    prev_idx <- if (length(model_indices) > 0) {
      max(model_indices)
    } else {
      next_index - 1
    }
  } else {
    last_idx <- 2
    prev_idx <- 1
  }

  list(
    is_comparison = is_comparison,
    existing_labels = existing_labels,
    existing_summaries = existing_summaries,
    model_indices = model_indices,
    model_count = model_count,
    next_index = next_index,
    prev_idx = prev_idx,
    last_idx = last_idx,
    sum1 = sum1,
    sum2 = sum2,
    spec = spec,
    coalesce_cols = coalesce_cols
  )
}

#' Join two parameter data frames into a comparison
#'
#' Selects relevant columns, renames with model-index suffixes, and performs
#' the full outer join. Handles both initial (2-model) and chained comparisons.
#'
#' @param params1 Enriched parameter data frame (or existing comparison)
#' @param params2 Enriched parameter data frame for the new model
#' @param suffix_cols Character vector of columns to suffix
#' @param positions List from compute_model_positions()
#' @return Joined data frame (no class, no custom attrs)
#' @noRd
join_comparison_params <- function(params1, params2, suffix_cols, positions) {
  coalesce_cols <- positions$coalesce_cols
  next_index <- positions$next_index

  # Select relevant columns from params2
  keep_cols <- c("name", suffix_cols, coalesce_cols)
  keep_cols2 <- intersect(keep_cols, names(params2))
  p2 <- dplyr::select(params2, dplyr::all_of(keep_cols2))

  # Rename suffix columns for params2
  for (col in suffix_cols) {
    if (col %in% names(p2)) {
      p2 <- dplyr::rename(
        p2,
        !!paste0(col, "_", next_index) := dplyr::all_of(col)
      )
    }
  }

  if (positions$is_comparison) {
    join_comparison_chained(params1, p2, suffix_cols, coalesce_cols)
  } else {
    join_comparison_initial(params1, p2, suffix_cols, keep_cols, coalesce_cols)
  }
}

#' @noRd
join_comparison_chained <- function(params1, p2, suffix_cols, coalesce_cols) {
  base_suffix_pattern <- paste0(
    "^(",
    paste(suffix_cols, collapse = "|"),
    ")_\\d+$"
  )
  pct_pattern <- "^pct_change(_\\d+)?$"
  keep_base <- unique(c(
    "name",
    grep(base_suffix_pattern, names(params1), value = TRUE),
    grep(pct_pattern, names(params1), value = TRUE)
  ))
  base_suffix <- dplyr::select(params1, dplyr::all_of(keep_base))

  base_coalesce <- dplyr::select(
    params1,
    dplyr::any_of(c("name", coalesce_cols))
  )
  p2_coalesce <- dplyr::select(p2, dplyr::any_of(c("name", coalesce_cols)))

  comparison <- dplyr::full_join(base_suffix, p2, by = "name")

  coalesce_df <- dplyr::full_join(
    base_coalesce,
    p2_coalesce,
    by = "name",
    suffix = c("_prev", "_new")
  )
  for (col in coalesce_cols) {
    col_prev <- paste0(col, "_prev")
    col_new <- paste0(col, "_new")
    if (col_prev %in% names(coalesce_df) || col_new %in% names(coalesce_df)) {
      coalesce_df[[col]] <- dplyr::coalesce(
        coalesce_df[[col_prev]],
        coalesce_df[[col_new]]
      )
      coalesce_df <- dplyr::select(
        coalesce_df,
        -dplyr::all_of(c(col_prev, col_new))
      )
    }
  }
  comparison <- dplyr::select(comparison, -dplyr::any_of(coalesce_cols))
  dplyr::left_join(comparison, coalesce_df, by = "name")
}

#' @noRd
join_comparison_initial <- function(
  params1,
  p2,
  suffix_cols,
  keep_cols,
  coalesce_cols
) {
  keep_cols1 <- intersect(keep_cols, names(params1))
  p1 <- dplyr::select(params1, dplyr::all_of(keep_cols1))

  # Rename suffix columns with _1 and _2
  for (col in suffix_cols) {
    if (col %in% names(p1)) {
      p1 <- dplyr::rename(p1, !!paste0(col, "_1") := dplyr::all_of(col))
    }
    if (col %in% names(p2)) {
      p2 <- dplyr::rename(p2, !!paste0(col, "_2") := dplyr::all_of(col))
    }
  }

  comparison <- dplyr::full_join(p1, p2, by = "name", suffix = c("_1", "_2"))

  for (col in coalesce_cols) {
    col1 <- paste0(col, "_1")
    col2 <- paste0(col, "_2")
    if (col1 %in% names(comparison) && col2 %in% names(comparison)) {
      comparison[[col]] <- dplyr::coalesce(
        comparison[[col1]],
        comparison[[col2]]
      )
      comparison <- dplyr::select(comparison, -dplyr::all_of(c(col1, col2)))
    }
  }
  comparison
}

#' Warn if model summaries are missing
#'
#' @param is_comparison Logical, whether params1 is already a comparison
#' @param sum1 Model summary for params1 (or last model in existing comparison)
#' @param sum2 Model summary for params2
#' @noRd
warn_missing_summaries <- function(is_comparison, sum1, sum2) {
  if (!is_comparison && is.null(sum1)) {
    rlang::warn(
      "params1 is missing model_summary attribute - footnote stats will be incomplete"
    )
  }
  if (is.null(sum2)) {
    rlang::warn(
      "params2 is missing model_summary attribute - footnote stats will be incomplete"
    )
  }
}

#' Warn if two models share no parameters
#'
#' Checks the estimate columns for prev_idx and last_idx to determine if any
#' rows have non-NA values in both.
#'
#' @param comparison Joined data frame
#' @param prev_idx Index of the previous model
#' @param last_idx Index of the new model
#' @noRd
warn_no_shared_parameters <- function(comparison, prev_idx, last_idx) {
  est_left_col <- paste0("estimate_", prev_idx)
  est_right_col <- paste0("estimate_", last_idx)
  if (
    est_left_col %in%
      names(comparison) &&
      est_right_col %in% names(comparison) &&
      !any(
        !is.na(comparison[[est_left_col]]) & !is.na(comparison[[est_right_col]])
      )
  ) {
    rlang::warn("No shared parameters between models.")
  }
}

#' Attach class and all attributes to a comparison data frame
#'
#' Merges pct_change_refs from the existing comparison, builds the summaries
#' list, and sets all custom attributes.
#'
#' @param comparison Joined data frame from join_comparison_params
#' @param positions List from compute_model_positions()
#' @param labels Resolved label vector
#' @param ref_idx Reference model index for pct_change
#' @param params1 Original params1 (for preserving existing pct_change_refs)
#' @return hyperion_comparison object with all attributes
#' @noRd
finalize_comparison <- function(
  comparison,
  positions,
  labels,
  ref_idx,
  params1
) {
  pct_col <- paste0("pct_change_", positions$last_idx)

  # Merge pct_change reference indices
  existing_pct_refs <- attr(params1, "pct_change_refs")
  if (is.null(existing_pct_refs)) {
    existing_pct_refs <- list()
  }
  existing_pct_refs[[pct_col]] <- ref_idx

  # Build summaries list
  if (positions$is_comparison) {
    summaries <- c(positions$existing_summaries, list(positions$sum2))
  } else {
    summaries <- list(positions$sum1, positions$sum2)
  }

  class(comparison) <- c("hyperion_comparison", class(comparison))
  attr(comparison, "summaries") <- summaries
  attr(comparison, "labels") <- labels
  attr(comparison, "table_spec") <- positions$spec
  attr(comparison, "pct_change_refs") <- existing_pct_refs

  comparison
}

#' Compare two enriched parameter data frames
#'
#' Joins two enriched parameter data frames for side-by-side comparison.
#' Both inputs should be prepared using the standard pipeline:
#' `get_parameters() |> apply_table_spec() |> add_summary_info()`.
#' Can also be chained by passing an existing `hyperion_comparison` object as
#' `params1` to add another model comparison.
#'
#' @param params1 Enriched parameter data frame from model 1
#' @param params2 Enriched parameter data frame from model 2
#' @param labels Model labels used in table headers.
#'   For an initial 2-model comparison, supply length 2 (default:
#'   `c("Model 1", "Model 2")`).
#'   For chained comparisons (`params1` is already a `hyperion_comparison`):
#'   length 1 appends a label for the new model; length 2 renames the previous
#'   latest label to `labels[1]` and appends `labels[2]` for the new model.
#' @param reference_model Character string specifying which model to use as
#'   reference for percent change calculations. Must match a model already in
#'   the comparison by `run_name` or label (file extensions are ignored).
#'   When NULL (default), percent change is calculated relative to the previous
#'   model in the chain.
#'
#' @return Data frame with class `hyperion_comparison` containing joined
#'   parameter data with suffixed columns and comparison attributes.
#'
#' @export
compare_with <- function(
  params1,
  params2,
  labels = c("Model 1", "Model 2"),
  reference_model = NULL
) {
  suffix_cols <- resolve_suffix_cols_for_comparison(params1)
  positions <- compute_model_positions(params1, params2, suffix_cols)

  if (!positions$is_comparison && !is.null(reference_model)) {
    rlang::warn("reference_model is ignored for initial two-model comparisons")
  }

  labels <- resolve_comparison_labels(
    positions$is_comparison,
    labels,
    positions$existing_labels
  )
  warn_missing_summaries(
    positions$is_comparison,
    positions$sum1,
    positions$sum2
  )

  comparison <- join_comparison_params(params1, params2, suffix_cols, positions)
  warn_no_shared_parameters(comparison, positions$prev_idx, positions$last_idx)

  # Use updated labels (post-rename) for reference resolution so that
  # labels = c("RenamedPrev", "New") + reference_model = "RenamedPrev" works.
  ref_labels <- labels[seq_along(positions$model_indices)]
  ref_idx <- resolve_reference_index(
    if (positions$is_comparison) reference_model else NULL,
    positions$existing_summaries,
    ref_labels,
    positions$model_indices,
    default_idx = positions$prev_idx
  )
  comparison <- compute_pct_change(comparison, ref_idx, positions$last_idx)

  finalize_comparison(comparison, positions, labels, ref_idx, params1)
}

#' Add model lineage to a comparison object
#'
#' Attaches lineage information to a comparison object to enable lineage-aware
#' features like conditional LRT display. When lineage is attached, the LRT
#' footnote will only be shown for model pairs that are in a direct
#' ancestor-descendant relationship.
#'
#' @param comparison A hyperion_comparison object from `compare_with()`
#' @param lineage A hyperion_nonmem_tree object from `get_model_lineage()`
#'
#' @return The comparison object with lineage attribute attached
#'
#' @export
add_model_lineage <- function(comparison, lineage) {
  if (!inherits(comparison, "hyperion_comparison")) {
    rlang::abort(
      "comparison must be a hyperion_comparison object from compare_with()"
    )
  }
  if (!inherits(lineage, "hyperion_nonmem_tree")) {
    rlang::abort(
      "lineage must be a hyperion_nonmem_tree object from get_model_lineage()"
    )
  }

  attr(comparison, "lineage") <- lineage
  comparison
}

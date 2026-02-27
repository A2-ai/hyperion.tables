#' Build summary footnote from model summary
#'
#' @param params Data frame with model_summary attribute
#' @param n_sigfig Number of significant figures for formatting
#' @return Character string for footnote, or NULL if no summary
#' @noRd
build_summary_footnote <- function(params, n_sigfig, ofv_decimals = NULL) {
  model_sum <- attr(params, "model_summary")
  if (is.null(model_sum)) {
    return(NULL)
  }

  parts <- character(0)

  if (
    !is.null(model_sum$estimation_method) &&
      !is.na(model_sum$estimation_method)
  ) {
    parts <- c(parts, model_sum$estimation_method)
  }

  if (!is.null(model_sum$ofv) && !is.na(model_sum$ofv)) {
    parts <- c(
      parts,
      sprintf(
        "Objective function value: %s",
        hyperion::format_hyperion_decimal_string(model_sum$ofv, ofv_decimals)
      )
    )
  }

  if (
    !is.null(model_sum$condition_number) &&
      !is.na(model_sum$condition_number)
  ) {
    parts <- c(
      parts,
      sprintf(
        "Condition Number: %s",
        hyperion::format_hyperion_sigfig_string(
          model_sum$condition_number,
          n_sigfig
        )
      )
    )
  }

  if (length(parts) > 0) paste(parts, collapse = " | ") else NULL
}

#' Add model summary information for table footnote
#'
#' Attaches estimation method, OFV, condition number, and number of
#' observations to parameter data for display as the first footnote in
#' the parameter table.
#'
#' @param params Enriched parameter data frame from `apply_table_spec()`
#' @param model_sum Summary object from `summary()`, or NULL to skip
#' @param show_method logical, if TRUE adds estimation method attribute for table footnote
#' @param show_ofv logical, if TRUE adds final objective function value attribute for table footnote
#' @param show_cond_num logical, if TRUE adds final condition number attribute for table footnote
#' @param show_number_obs logical, if TRUE adds number of observations attribute for table footnote
#'
#' @return Data frame with model_summary attribute attached
#' @export
add_summary_info <- function(
  params,
  model_sum,
  show_method = TRUE,
  show_ofv = TRUE,
  show_cond_num = TRUE,
  show_number_obs = TRUE
) {
  if (is.null(model_sum)) {
    return(params)
  }

  est_method <- if (show_method) {
    dplyr::last(model_sum$run_details$estimation_method)
  } else {
    NULL
  }

  ofv <- if (show_ofv) {
    dplyr::last(model_sum$minimization_results$ofv)
  } else {
    NULL
  }

  cn <- if (show_cond_num) {
    dplyr::last(model_sum$minimization_results$condition_number)
  } else {
    NULL
  }

  n_obs <- if (show_number_obs) {
    dplyr::last(model_sum$run_details$number_obs)
  } else {
    NULL
  }

  attr(params, "model_summary") <- list(
    run_name = model_sum$run_name,
    estimation_method = est_method,
    ofv = ofv,
    condition_number = cn,
    number_obs = n_obs
  )

  params
}

#' Extract TableSpec from a parameter data frame
#'
#' Retrieves the `TableSpec` attached to a parameter data frame (e.g., from
#' `apply_table_spec()`). Returns NULL if none is found.
#'
#' @param params Data frame carrying a `table_spec` attribute
#'
#' @return A TableSpec object or NULL
#' @export
get_table_spec <- function(params) {
  spec <- attr(params, "table_spec")
  if (is.null(spec)) {
    return(NULL)
  }
  if (!S7::S7_inherits(spec, TableSpec)) {
    rlang::abort("Attached table_spec is not a TableSpec object")
  }
  spec
}

#' Order sections and select columns
#'
#' Orders rows by section according to the spec, and selects the appropriate columns.
#'
#' @param params Data frame with summary rows from `add_summary_rows()`
#' @param spec A TableSpec object
#'
#' @importFrom rlang .data
#'
#' @return Reordered data frame ready for `make_parameter_table()`
#' @noRd
#' @keywords internal
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

order_sections <- function(params, spec) {
  section_levels <- unique(get_section_order(spec))

  internal_cols <- c(
    "section",
    ".appear_order",
    "kind",
    "random_effect",
    "diagonal",
    "transforms",
    "cv",
    "corr",
    "sd"
  )
  dt_cols <- grep("^dt_", names(params), value = TRUE)

  # Only include internal columns that actually exist in the data
  internal_cols <- intersect(internal_cols, names(params))

  drop_columns <- expand_ci_drop_columns(spec@drop_columns)
  add_cols <- spec@add_columns %||% character(0)
  select_cols <- setdiff(spec@columns, drop_columns)
  if (length(add_cols) > 0) {
    select_cols <- unique(c(select_cols, add_cols))
  }
  if ("description" %in% select_cols && !spec@.columns_provided) {
    select_cols <- c(
      "name",
      "description",
      setdiff(select_cols, c("name", "description"))
    )
  }
  if (
    any(select_cols %in% c("ci_low", "ci_high")) &&
      !"fixed" %in% select_cols
  ) {
    select_cols <- unique(c(select_cols, "fixed"))
  }
  fixed_requested <- "fixed" %in% c(spec@columns, add_cols)
  if (fixed_requested && "fixed_fmt" %in% names(params)) {
    select_cols <- unique(c(select_cols, "fixed_fmt"))
  }

  # Ensure requested columns exist (variability is built during rendering)
  missing_cols <- setdiff(select_cols, names(params))
  if (length(missing_cols) > 0) {
    numeric_display_cols <- c(
      "estimate",
      "ci_low",
      "ci_high",
      "rse",
      "shrinkage",
      "stderr",
      "pct_change",
      "cv",
      "corr",
      "sd"
    )
    for (col in missing_cols) {
      if (col %in% numeric_display_cols) {
        params[[col]] <- NA_real_
      } else {
        params[[col]] <- NA_character_
      }
    }
  }

  params |>
    dplyr::mutate(
      .appear_order = dplyr::row_number(),
      section = factor(.data$section, levels = section_levels)
    ) |>
    dplyr::arrange(.data$section, .data$.appear_order) |>
    dplyr::select(dplyr::all_of(c(
      select_cols,
      internal_cols,
      dt_cols
    )))
}

# ==============================================================================
# GT table building
# ==============================================================================

#' Build parameter table
#'
#' Creates a formatted table from parameter data. Supports multiple output
#' formats: gt (default), flextable, or the intermediate HyperionTable object.
#'
#' @param params Parameter data frame from `get_parameters()` or enriched via
#'   `apply_table_spec()`
#' @param output Output format: "gt" (default), "flextable", or "data" for
#'   the intermediate HyperionTable object.
#'
#' @importFrom rlang .data
#'
#' @return A gt table, flextable, or HyperionTable object depending on `output`
#' @export
make_parameter_table <- function(
  params,
  output = c("gt", "flextable", "data")
) {
  output <- match.arg(output)

  if (output == "flextable") {
    check_suggested("flextable", reason = "for flextable output.")
  }

  # Get table_spec - required for proper formatting
  spec <- attr(params, "table_spec")
  if (is.null(spec)) {
    rlang::abort(
      "TableSpec not found. Run apply_table_spec(params, spec, info) first."
    )
  }

  # Prepare data + layout (ordering, display columns, labels, hide rules).
  layout <- prepare_parameter_table_data(params, spec)

  # Create intermediate representation
  htable <- hyperion_parameter_table(layout$params, layout, spec)

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

# ==============================================================================
# HyperionTable Constructor for Parameter Tables
# ==============================================================================

#' Create HyperionTable from parameter table layout
#'
#' @param params Data frame from prepare_parameter_table_data()
#' @param layout List with hide_cols, label_map, groupname, ci_rows
#' @param spec TableSpec object
#' @return HyperionTable object
#' @noRd
hyperion_parameter_table <- function(params, layout, spec) {
  # Determine CI merge columns
  ci_in_spec <- all(c("ci_low", "ci_high") %in% spec@columns)
  ci_in_data <- all(c("ci_low", "ci_high") %in% names(params))
  ci_merges <- if (ci_in_spec && ci_in_data) {
    list(ci_merge_spec("ci_low", "ci_high"))
  } else {
    list()
  }

  # Build footnotes
  footnotes <- build_parameter_footnotes(params, spec, layout)

  # Numeric columns for formatting
  numeric_cols <- intersect(
    c("estimate", "ci_low", "ci_high", "rse", "shrinkage"),
    names(params)
  )

  # Bold locations
  bold_locs <- c("column_labels")
  if (!is.null(spec@title) && nchar(spec@title) > 0) {
    bold_locs <- c(bold_locs, "title")
  }
  if (!is.null(layout$groupname)) {
    bold_locs <- c(bold_locs, "row_groups")
  }

  HyperionTable(
    data = params,
    table_type = "parameter",
    groupname_col = layout$groupname,
    hide_cols = layout$hide_cols,
    col_labels = layout$label_map,
    title = spec@title,
    spanners = list(),
    numeric_cols = numeric_cols,
    n_sigfig = spec@n_sigfig,
    ci = spec@ci,
    ci_merges = ci_merges,
    ci_missing_rows = as.integer(layout$ci_rows),
    bold_locations = bold_locs,
    borders = list(),
    footnotes = footnotes,
    missing_text = spec@missing_text,
    missing_apply_to = spec@missing_apply_to,
    source_spec = spec
  )
}

#' Build footnotes for parameter table
#' @noRd
build_parameter_footnotes <- function(params, spec, layout) {
  footnotes <- list()

  # Get footnote order from spec
  footnote_order <- spec@footnote_order
  if (is.null(footnote_order)) {
    return(list())
  }

  # Build summary info footnote
  ofv_decimals <- if (!is.na(spec@n_decimals_ofv)) spec@n_decimals_ofv else NULL
  summary_note <- build_summary_footnote(params, spec@n_sigfig, ofv_decimals)

  # Detect statistics
  stats <- detect_table_statistics(params)
  ci_pct <- round(spec@ci@level * 100)

  # Check if CI dropped
  expanded_drop <- expand_ci_drop_columns(spec@drop_columns)
  if (all(c("ci_low", "ci_high") %in% expanded_drop)) {
    stats$has_ci <- FALSE
  }

  # Build equations
  equations <- build_equations_footnote(stats, ci_pct, NULL, NULL)

  # Build abbreviations
  abbreviations <- build_abbreviations_footnote(stats, NULL, NULL)

  # Assemble footnotes in order
  for (section in footnote_order) {
    if (section == "summary_info" && !is.null(summary_note)) {
      footnotes <- c(footnotes, list(footnote_spec(summary_note, FALSE)))
    } else if (section == "equations" && !is.null(equations)) {
      for (eq in equations) {
        # equations are gt::md() objects, extract the content
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
# Table pipeline helpers
# ==============================================================================

#' Prepare parameter table data and layout
#' @noRd
prepare_parameter_table_data <- function(params, spec) {
  params <- blank_ci_for_fixed(params)
  params <- add_fixed_display_columns(params, "fixed")
  params <- order_sections(params, spec)

  hide_cols <- resolve_hidden_columns(params, spec)
  label_map <- build_layout_labels(params, spec, hide_cols)

  groupname <- if (length(spec@sections) > 0) "section" else NULL

  ci_rows <- integer(0)
  if (all(c("ci_low", "ci_high") %in% names(params))) {
    ci_rows <- which(!is_fixed_true(params$fixed))
  }

  list(
    params = params,
    hide_cols = hide_cols,
    label_map = label_map,
    groupname = groupname,
    ci_rows = ci_rows
  )
}

#' Determine which columns to hide in a parameter table
#' @noRd
resolve_hidden_columns <- function(params, spec) {
  empty_cols <- if (spec@hide_empty_columns) {
    find_empty_columns(params)
  } else {
    character(0)
  }

  plan <- variability_plan(spec)
  if (plan$build_variability) {
    empty_cols <- setdiff(empty_cols, "variability")
  }

  add_cols <- spec@add_columns %||% character(0)
  requested_cols <- if (isTRUE(spec@.columns_provided)) {
    unique(c(spec@columns, add_cols))
  } else {
    unique(add_cols)
  }
  if ("fixed" %in% requested_cols && "fixed_fmt" %in% names(params)) {
    requested_cols <- unique(c(setdiff(requested_cols, "fixed"), "fixed_fmt"))
  }
  if (spec@hide_empty_columns) {
    empty_cols <- setdiff(empty_cols, requested_cols)
  }

  dt_cols <- grep("^dt_", names(params), value = TRUE)
  hide_cols <- c(
    ".appear_order",
    "kind",
    "random_effect",
    "diagonal",
    "transforms",
    "cv",
    "corr",
    "sd",
    "nonmem_name",
    "user_name",
    "fixed_fmt",
    dt_cols,
    empty_cols
  )

  if ("cv" %in% requested_cols) {
    hide_cols <- setdiff(hide_cols, "cv")
  }
  if ("corr" %in% requested_cols) {
    hide_cols <- setdiff(hide_cols, "corr")
  }
  if ("sd" %in% requested_cols) {
    hide_cols <- setdiff(hide_cols, "sd")
  }
  if (plan$wants_components) {
    hide_cols <- unique(c(hide_cols, "variability"))
  }

  # Fixed column visibility (single check, was previously duplicated)
  fixed_requested <- if (isTRUE(spec@.columns_provided)) {
    "fixed" %in% c(spec@columns, add_cols)
  } else {
    "fixed" %in% add_cols
  }

  # Raw `fixed` column is always hidden (display uses `fixed_fmt`)
  hide_cols <- unique(c(hide_cols, "fixed"))
  if (fixed_requested) {
    hide_cols <- setdiff(hide_cols, "fixed_fmt")
  } else {
    hide_cols <- unique(c(hide_cols, "fixed_fmt"))
  }

  # Hide both when all fixed values are NA (only if not explicitly requested)
  if (
    !fixed_requested &&
      "fixed" %in% names(params) &&
      spec@hide_empty_columns &&
      !any(params$fixed, na.rm = TRUE)
  ) {
    hide_cols <- unique(c(hide_cols, "fixed", "fixed_fmt"))
  }

  intersect(hide_cols, names(params))
}

#' Build column label map for a parameter table
#' @noRd
build_layout_labels <- function(params, spec, hide_cols) {
  ci_pct <- get_ci_pct(spec, default = 95)
  label_map <- build_parameter_label_map(ci_pct)
  label_map <- adjust_ci_labels(label_map, spec, ci_pct)

  if ("fixed_fmt" %in% names(params)) {
    label_map$fixed_fmt <- label_map$fixed
  }
  label_map <- label_map[intersect(names(label_map), names(params))]

  # Remove labels for hidden fixed columns
  add_cols <- spec@add_columns %||% character(0)
  fixed_requested <- if (isTRUE(spec@.columns_provided)) {
    "fixed" %in% c(spec@columns, add_cols)
  } else {
    "fixed" %in% add_cols
  }

  if (!fixed_requested) {
    label_map <- label_map[setdiff(names(label_map), c("fixed", "fixed_fmt"))]
  } else if (
    !"fixed_fmt" %in% names(label_map) && "fixed" %in% names(label_map)
  ) {
    label_map$fixed_fmt <- label_map$fixed
    label_map$fixed <- NULL
  }

  label_map
}
